#load "packages.fsx"
open System
open Microsoft.SemanticKernel
open Microsoft.SemanticKernel.ChatCompletion

//Settings.installSettings @"%USERPROFILE%/.fsopenai/acct_temp_poc/ServiceSettings.json"
Settings.installSettings @"%USERPROFILE%/.fsopenai/ptu/ServiceSettings.json"

let model = "gpt-4"
//let model = "gpt-35-turbo"

let k = GenUtils.baseKernel Settings.settings model |> GenUtils.build
let srv = k.GetRequiredService<IChatCompletionService>()

let sysMsg = "You are a helpful AI assistant. Be brief but elaborate, if required. Let's think step-by-step. Be sure about your answer, don't make things up."

let puzzle = """
**context**
Four friends are cycling around the city on their bikes. Each one has a different name, a different age, is riding a different bike, and have brought a different sandwich. Use the clues to find out their characteristics.

Friends: David, Eric, Gregory, Ivan
Bike Colors: Blue, Pink, Red, Yellow
Ages: 10, 11, 12, 14
Sandwiches: Cheese, Peanut Butter, Roast Beef, Turkey

**clues**
The 12-year-old cyclist is in the third position.
The cyclist with the Yellow bike is immediately after Ivan.
The 14-year-old cyclist has a Cheese sandwich.
The boy with the Peanut Butter sandwich is directly before the boy with the Turkey sandwich.
Eric is the boy who has a Peanut Butter sandwich.
The 11-year-old boy is in the last position.
Cyclist Gregory is 14 years old.
The cyclist with the Red bike is next to the cyclist named David.
Ivan is the one riding the Blue bike.
The 14-year-old boy is in the second position.

Find the postion, bike color, age and sandwith of each friend, given the context and the clues
"""

let prompt = $"""
# INSTRUCTIONS
Convert the PUZZLE into a SMTLIB formula
Ensure all constants are declared
Ensure all constraints are declared
Ensure all constraints are in the correct order

# PUZZLE
```
{puzzle}
```

SMTLIB FORMULA:
"""
let prmptStngs = GenUtils.promptSettings 2000 0.0
let ch = GenUtils.buildHistory sysMsg [prompt] 

let r = srv.GetChatMessageContentAsync(ch,prmptStngs).Result
printfn "%s" r.Content
let formulas = GenUtils.extractTripleQuoted r.Content
let formula = String.Join("\n",formulas |> Seq.collect id)
printfn "%s" formula
open Microsoft.Z3
let ctx = new Context()
let smtZ3 = ctx.ParseSMTLIB2String(formula)
let solver = ctx.MkSolver()
solver.Add(smtZ3)
solver.Check()
solver.Model.Consts |> Seq.iter (fun x-> printfn $"{x.Key.Name}={x.Value}")
