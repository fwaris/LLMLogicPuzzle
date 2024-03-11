///json structure for specificyng endpoints and keys
module Settings

type AzureOpenAIEndpoints = 
    {
        API_KEY : string
        RESOURCE_GROUP : string
        API_VERSION : string
    }

type ApiEndpoint =
    {
        API_KEY : string
        ENDPOINT : string
    }

type ModelDeployments = 
    {
        CHAT : string list
        COMPLETION : string list
        EMBEDDING : string list
    }

type ServiceSettings = 
    {
        OPENAI_KEY : string option
        LOG_CONN_STR : string option
        BING_ENDPOINT : ApiEndpoint option
        AZURE_SEARCH_ENDPOINTS: ApiEndpoint list
        AZURE_OPENAI_ENDPOINTS: AzureOpenAIEndpoints list
        EMBEDDING_ENDPOINTS : AzureOpenAIEndpoints list
    }
    with 
        static member Default = 
            {
                OPENAI_KEY = None
                LOG_CONN_STR = None
                BING_ENDPOINT = None
                AZURE_OPENAI_ENDPOINTS = []
                AZURE_SEARCH_ENDPOINTS = []
                EMBEDDING_ENDPOINTS = []
            }


let mutable settings = ServiceSettings.Default

let installSettings (path: string) =
    let path = System.Environment.ExpandEnvironmentVariables path
    let str = System.IO.File.ReadAllText path
    let sttngs = System.Text.Json.JsonSerializer.Deserialize<ServiceSettings>(str)
    settings <- sttngs

