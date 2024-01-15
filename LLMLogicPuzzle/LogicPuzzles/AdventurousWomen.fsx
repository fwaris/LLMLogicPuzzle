#load "BaseZ3.fsx"
open BaseZ3
open Microsoft.Z3

let W  = ctx.MkEnumSort("Woman", [|"Fiona"; "Isabelle"; "Lauren"; "Rebecca"; "Samantha"|])
let [|fiona;isabelle;lauren;rebecca;samantha|] = W.Consts

let S = ctx.MkEnumSort("Sport", [|"Motocross"; "Paragliding"; "Surfing"; "Wakeboarding"; "Windsurfing";|])
let [|motocross;paragliding;surfing;wakeboarding;windsurfing|] = S.Consts

let M = ctx.MkEnumSort("Mentor", [|"Chinese"; "Dutch"; "German"; "Mexican"; "Norwegian";|])
let [|chinese;dutch;german;mexican;norwegian|] = M.Consts

let A = ctx.MkEnumSort("Age", [|"26"; "30"; "32"; "34"; "36";|])
let [|twentySix;thirty;thirtyTwo;thirtyFour;thirtySix|] = A.Consts

let So = ctx.MkEnumSort("Shoes", [|"Green"; "Pink"; "Red"; "White"; "Yellow";|])
let [|green;pink;red;white;yellow|] = So.Consts

BaseZ3.positions <- ctx.MkArrayConst("positions", W, ctx.IntSort)

let ages = ctx.MkArrayConst("ages", A, W)
let mentors = ctx.MkArrayConst("mentors", M, W)
let sports = ctx.MkArrayConst("sports", S, W)
let shoes = ctx.MkArrayConst("shoes", So, W)

let clues = 
    [
        positions.[mentors.[german]] == !1 //The woman who had a German mentor is at the second position.
        sports.[windsurfing] <<>> shoes.[pink] //The person who likes Windsurfing is next to the one wearing Pink shoes.
        sports.[paragliding] == samantha //Samantha is also the one who practices Paragliding.
        positions.[fiona] == !1 //Fiona is at the second position.
        rebecca <<>> sports.[motocross] //Rebecca is next to the person who enjoys Motocross.
        mentors.[chinese] <<>> sports.[windsurfing] //The woman who had a Chinese mentor is next to the person who likes Windsurfing.
        mentors.[dutch] == ages.[thirtyFour] //The woman who had a Dutch mentor is also 34 years old.
        shoes.[green] <<>> rebecca //The woman wearing Green shoes is next to Rebecca.
        between mentors.[norwegian] (sports.[surfing],mentors.[dutch]) //The woman who had a Norwegian mentor is somewhere between the woman who likes Surfing and the woman who had a Dutch mentor, in that order.
        shoes.[white] >>! shoes.[green] //The woman wearing White shoes is immediately to the right of the one wearing Green shoes.
        sports.[windsurfing] >>! isabelle //The person who likes Windsurfing is immediately to the right of Isabelle.
        pstnOf shoes.[red] << pstnOf rebecca  //The woman wearing Red shoes is somewhere to the left of Rebecca.
        pstnOf shoes.[red] << pstnOf ages.[twentySix ]//The person wearing Red shoes is somewhere to the left of the 26-year-old person.
        pstnOf mentors.[chinese] == !2 //The woman who had a Chinese mentor is in the middle position.
        ages.[thirtyTwo] <<! shoes.[red] //The 32-year-old person is immediately to the left of the one wearing Red shoes.
        between sports.[wakeboarding] (mentors.[mexican], sports.[surfing]) //The woman who likes Wakeboarding is somewhere between the woman who had a Mexican mentor and the person who enjoys Surfing, in that order.
        shoes.[red] <<! ages.[thirtySix]//The woman wearing Red shoes is immediately to the left of the 36-year-old woman.
    ]

let allDistinct = 
    [
        //ensure all values are distinct ...
        ctx.MkDistinct([|for f in A.Consts -> ages.[f]|])        
        ctx.MkDistinct([|for b in M.Consts -> mentors.[b]|])
        ctx.MkDistinct([|for s in S.Consts -> sports.[s]|])
        ctx.MkDistinct([|for b in So.Consts -> shoes.[b]|])
        ctx.MkDistinct([|for b in W.Consts -> positions.[b]|])
    ]
let pstnConstraints : BoolExpr list =
    [
        for b in W.Consts do
            yield ctx.MkGe(pstnOf b,!0)
            yield ctx.MkLe(pstnOf b,!4)
    ]

let assertions = clues @ allDistinct @ pstnConstraints |> Seq.toArray
let solver = ctx.MkSolver()
solver.Add(assertions)
solver.Check()

let bpAges = A.Consts |> Seq.map(fun ti -> solver.Model.Eval(ages.[ti]).ToString(),ti) |> Map.ofSeq
let bpMentors = M.Consts |> Seq.map(fun ti -> solver.Model.Eval(mentors.[ti]).ToString(),ti) |> Map.ofSeq
let bpSports = S.Consts |> Seq.map(fun ti -> solver.Model.Eval(sports.[ti]).ToString(),ti) |> Map.ofSeq
let bpShoes = So.Consts |> Seq.map(fun ti -> solver.Model.Eval(shoes.[ti]).ToString(),ti) |> Map.ofSeq
let bpPositions = W.Consts |> Seq.map(fun ti -> solver.Model.Eval(positions.[ti]).ToString(),ti) |> Map.ofSeq

bpPositions 
|> Map.toSeq 
|> Seq.iter(fun (v,k) -> 
    let n = k.ToString()
    printfn "%s" n
    printfn "    %A" bpShoes.[n]
    printfn "    %A" bpSports.[n]
    printfn "    %A" bpMentors.[n]
    printfn "    %A" bpAges.[n])




