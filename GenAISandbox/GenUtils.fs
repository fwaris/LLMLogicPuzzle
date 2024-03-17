module GenUtils 
open System
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.SemanticKernel.Memory
open Microsoft.SemanticKernel.ChatCompletion
open Microsoft.SemanticKernel.Connectors.OpenAI
open Microsoft.SemanticKernel
open Microsoft.DeepDev
open Azure.Search.Documents.Indexes
open Azure
open Settings
open FSharp.Control

let rng = Random()
let randSelect (ls:_ list) = ls.[rng.Next(ls.Length)]

let tokenSize (s:string) = 
    let tokenizer = TokenizerBuilder.CreateByModelNameAsync("gpt-4").GetAwaiter().GetResult();
    let tokens = tokenizer.Encode(s, new System.Collections.Generic.HashSet<string>());
    float tokens.Count

let content (msg:Azure.AI.OpenAI.ChatRequestMessage) = 
    match msg with 
    | :? Azure.AI.OpenAI.ChatRequestAssistantMessage as x -> x.Content
    | :? Azure.AI.OpenAI.ChatRequestUserMessage as x -> x.Content
    | _ -> ""

let tokenEstimate (ch:ChatHistory) =
    let xs = 
        seq {
            for m in ch do
                yield m.Role.Label
                yield m.Content
        }
    String.Join("\n",xs)
    |> tokenSize

let getAzureEndpoint (endpoints:AzureOpenAIEndpoints list) =
    if List.isEmpty endpoints then failwith "No Azure OpenAI endpoints configured"
    let endpt = randSelect endpoints
    let rg = endpt.RESOURCE_GROUP
    let url = $"https://{rg}.openai.azure.com"
    rg,url,endpt.API_KEY

let getClient (parms:ServiceSettings)  =
    let rg,url,key = getAzureEndpoint parms.AZURE_OPENAI_ENDPOINTS
    Azure.AI.OpenAI.OpenAIClient(Uri url,Azure.AzureKeyCredential(key))                        
            
let getEmbeddingsClient (parms:ServiceSettings)=
    let rg,url,key = getAzureEndpoint parms.EMBEDDING_ENDPOINTS
    let clr = Azure.AI.OpenAI.OpenAIClient(Uri url,Azure.AzureKeyCredential(key))                        
    clr

let searchServiceClient (parms:ServiceSettings) = 
    match parms.AZURE_SEARCH_ENDPOINTS with 
    | [] -> failwith "No Azure Cognitive Search endpoints configured"
    | xs ->
        let ep = randSelect xs
        SearchIndexClient(Uri ep.ENDPOINT,AzureKeyCredential(ep.API_KEY))

let logger = 
    {new ILogger with
            member this.BeginScope(state) = raise (System.NotImplementedException())
            member this.IsEnabled(logLevel) = true
            member this.Log(logLevel, eventId, state, ``exception``, formatter) = 
                let msg = formatter.Invoke(state,``exception``)
                printfn "Kernel: %s" msg
    }

let loggerFactory = 
    {new ILoggerFactory with
            member this.AddProvider(provider) = ()
            member this.CreateLogger(categoryName) = logger
            member this.Dispose() = ()
    }

let promptSettings maxTokens temperature =
    new OpenAIPromptExecutionSettings(
        MaxTokens = Nullable maxTokens, 
        Temperature = temperature) 

let baseKernel settings chatModel = 
    let builder = Kernel.CreateBuilder()    
    builder.Services.AddLogging(fun c -> c.AddConsole().SetMinimumLevel(LogLevel.Information) |>ignore) |> ignore
    let rg,uri,key = getAzureEndpoint settings.AZURE_OPENAI_ENDPOINTS
    builder.AddAzureOpenAIChatCompletion(deploymentName = chatModel,endpoint = uri, apiKey = key)

let kernelArgsFrom maxTokens temperature (args:(string*string) seq) =
    let sttngs = promptSettings maxTokens temperature
    let kargs = KernelArguments(sttngs)
    for (k,v) in args do
        kargs.Add(k,v)
    kargs

let kernelArgsDefault (args:(string*string) seq) =
    let sttngs = new OpenAIPromptExecutionSettings(MaxTokens = 150, Temperature = 0, TopP = 1)
    let kargs = KernelArguments(sttngs)
    for (k,v) in args do
        kargs.Add(k,v)
    kargs

let kernelArgs (args:(string*string) seq) (overrides:OpenAIPromptExecutionSettings->unit) =
    let args = kernelArgsDefault args
    args.ExecutionSettings 
    |> Seq.iter(fun kv -> 
        let sttngs = (kv.Value :?> OpenAIPromptExecutionSettings)
        overrides sttngs)
    args

let renderPrompt (prompt:string) (args:KernelArguments) =
    task {            
        let k = Kernel.CreateBuilder().Build()
        let fac = KernelPromptTemplateFactory()
        let cfg = PromptTemplateConfig(template = prompt)            
        let pt = fac.Create(cfg)
        let! rslt = pt.RenderAsync(k,args) |> Async.AwaitTask
        return rslt
    }

let build (k:IKernelBuilder) = k.Build()

let searchResults parms ch maxDocs query (cogMems:ISemanticTextMemory seq) =
    cogMems
    |> AsyncSeq.ofSeq
    |> AsyncSeq.collect(fun cogMem ->             
        cogMem.SearchAsync("",query,maxDocs) |> AsyncSeq.ofAsyncEnum)
    |> AsyncSeq.toBlockingSeq
    |> Seq.toList

let buildHistory (sysMsg:string) (msgs:string seq) = 
    let ch = ChatHistory(sysMsg)
    msgs 
    |> Seq.indexed
    |> Seq.iter (fun (i,m) -> 
        let role = if i%2=0 then AuthorRole.User else AuthorRole.Assistant
        ch.Add(ChatMessageContent(role, m)))
    ch

let extractTripleQuoted (inp:string) =
    let lines =
        seq {
            use sr = new System.IO.StringReader(inp)
            let mutable line = sr.ReadLine()
            while line <> null do
                yield line
                line <- sr.ReadLine()
        }
        |> Seq.map(fun x -> x.Trim())
        |> Seq.toList
    let addSnip acc accSnip = 
        match accSnip with 
        |[] -> acc 
        | _ -> (List.rev accSnip)::acc
    let isQuote (s:string) = s.StartsWith("```")
    let rec start acc (xs:string list) = 
        match xs with 
        | []                   -> List.rev acc
        | x::xs when isQuote x -> accQuoted acc [] xs
        | x::xs                -> start acc xs
    and accQuoted acc accSnip xs = 
        match xs with
        | []                   -> List.rev (addSnip acc accSnip)
        | x::xs when isQuote x -> start (addSnip acc accSnip) xs
        | x::xs                -> accQuoted acc (x::accSnip) xs
    start [] lines


let defaultSysMsg = "You are a helpful AI assistant. Be brief but elaborate, if required. Let's think step-by-step. Be sure about your answer, don't make things up."

let run prompt =    
    let k = baseKernel Settings.settings "gpt-4" |> build
    let srv = k.GetRequiredService<IChatCompletionService>()   
    let prmptStngs = promptSettings 2000 0.0
    let ch = buildHistory defaultSysMsg [prompt] 
    srv.GetChatMessageContentAsync(ch,prmptStngs).Result
    