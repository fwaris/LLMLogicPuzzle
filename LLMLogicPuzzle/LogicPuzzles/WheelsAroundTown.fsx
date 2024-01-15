#load "BaseZ3.fsx"
open BaseZ3
open Microsoft.Z3

//source: https://www.ahapuzzles.com/logic/zebra/wheels-around-town/

let F = ctx.MkEnumSort("Friends", [|"David"; "Eric"; "Gregory"; "Ivan";|])
let [|david;eric;gregory;ivan|] = F.Consts

let B = ctx.MkEnumSort("Bikes", [|"Blue"; "Pink"; "Red"; "Yellow";|])
let [|blue;pink;red;yellow|] = B.Consts

let A = ctx.MkEnumSort("Age", [|"10"; "11"; "12"; "14";|])
let [|ten;eleven;twelve;fourteen|] = A.Consts

let Sw = ctx.MkEnumSort("Sandwich", [|"Cheese"; "Peanut Butter"; "Roast Beef"; "Turkey";|])
let [|cheese;peanutButter;roastBeef;turkey|] = Sw.Consts

BaseZ3.positions <- ctx.MkArrayConst("positions", F, ctx.IntSort)

let ages = ctx.MkArrayConst("ages", A, F)
let bikes = ctx.MkArrayConst("bikes", B, F)
let sandwiches = ctx.MkArrayConst("sandwiches", Sw, F)

//clues (constraints)
let clues = 
    [
        positions.[ages.[twelve]] == !2 //The 12-year-old cyclist is in the third position. (0-based)
        bikes.[yellow] >>! ivan //The cyclist with the Yellow bike is immediately after Ivan.
        ages.[fourteen] == sandwiches.[cheese]  //The 14-year-old cyclist has a Cheese sandwich.
        sandwiches.[peanutButter] <<! sandwiches.[turkey] //The boy with the Peanut Butter sandwich is directly before the boy with the Turkey sandwich.
        eric == sandwiches.[peanutButter] //Eric is the boy who has a Peanut Butter sandwich.
        positions.[ages.[eleven]] == !3 //The 11-year-old boy is in the last position.
        ages.[fourteen] == gregory //Cyclist Gregory is 14 years old.
        bikes.[red] <<>> david //The cyclist with the Red bike is next to the cyclist named David.
        bikes.[blue] == ivan //Ivan is the one riding the Blue bike.
        positions.[ages.[fourteen]] == !1 //The 14-year-old boy is in the second position.
    ]

let allDistinct : BoolExpr list = 
    [
        //ensure all values are distinct ...
        ctx.MkDistinct([|for f in A.Consts -> ages.[f]|])        
        ctx.MkDistinct([|for b in B.Consts -> bikes.[b]|])
        ctx.MkDistinct([|for s in Sw.Consts -> sandwiches.[s]|])
        ctx.MkDistinct([|for b in F.Consts -> positions.[b]|])
    ]

let pstnConstraints : BoolExpr list =
    [
        for b in F.Consts do
            yield ctx.MkGe(pstnOf b,!0)
            yield ctx.MkLe(pstnOf b,!3)
    ]

let assertions = clues @ allDistinct @ pstnConstraints |> Seq.toArray
let solver = ctx.MkSolver()
solver.Add(assertions)
solver.Check()

let bpBikes = B.Consts |> Seq.map(fun ti -> solver.Model.Eval(bikes.[ti]).ToString(),ti) |> Map.ofSeq
let bpAges = A.Consts |> Seq.map(fun ti -> solver.Model.Eval(ages.[ti]).ToString(),ti) |> Map.ofSeq
let bpSandwiches = Sw.Consts |> Seq.map(fun ti -> solver.Model.Eval(sandwiches.[ti]).ToString(),ti) |> Map.ofSeq
let bpPositions = F.Consts |> Seq.map(fun ti -> solver.Model.Eval(positions.[ti]).ToString(),ti) |> Map.ofSeq

bpPositions 
|> Map.toSeq 
|> Seq.iter(fun (v,k) -> 
    let n = k.ToString()
    printfn "%s" n
    printfn "    %A" bpBikes.[n]
    printfn "    %A" bpAges.[n]
    printfn "    %A" bpSandwiches.[n])
