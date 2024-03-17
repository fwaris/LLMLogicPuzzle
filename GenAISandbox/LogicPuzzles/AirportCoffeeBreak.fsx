#load "BaseZ3.fsx"
open BaseZ3
open Microsoft.Z3

//source: https://www.ahapuzzles.com/logic/zebra/airport-coffee-break/

let B = ctx.MkEnumSort("Businessperson" , [|"Adam"; "John"; "Paul"; "Quent"; "Angela"|])
let [|adam;john;paul;quent;angela|] = B.Consts

let C = ctx.MkEnumSort("City" , [|"London"; "Amsterdam"; "Manila"; "Seoul"; "Warsaw"|])
let [|london;amsterdam;manila;seoul;warsaw|] = C.Consts

let G = ctx.MkEnumSort("Gate", [|"12"; "89"; "25"; "98"; "34";|])
let [|g12;g89;g25;g98;g34|] = G.Consts

let T = ctx.MkEnumSort("Time", [|"4:00 PM"; "4:30 PM";  "5:00 PM"; "5:30 PM" ; "7:00 PM" ;|])
let [|t400;t430;t500;t530;t700|] = T.Consts

let S = ctx.MkEnumSort("Sport", [|"Archery"; "Diving"; "Judo"; "Sailing"; "Snowboarding"|])
let [|archery;diving;judo;sailing;snowboarding|] = S.Consts

let Ti = ctx.MkEnumSort("Tie", [|"Blue"; "White"; "Green"; "Yellow"; "Black"|])
let [|blue;white;green;yellow;black|] = Ti.Consts

let ties = ctx.MkArrayConst("ties", Ti, B)
let gates = ctx.MkArrayConst("gates", G, B)
let cities = ctx.MkArrayConst("cities", C, B)
let sports = ctx.MkArrayConst("sports", S, B)
let times = ctx.MkArrayConst("times", T, B)
BaseZ3.positions <- ctx.MkArrayConst("seats", B, ctx.IntSort)


//clues (constraints)

let clues : BoolExpr list =
    [
        times.[t530] == adam //Adam's flight departs at 5:30 PM.
        ties.[blue] <<! john //The man wearing a Blue tie is seated to the left of John.        
        gates.[g89] == john  //John's departure gate is 89.
        gates.[g25] <<>> cities.[london] //The person departing from Gate 25 is seated next to the person flying to the English-speaking city.
        atEnd 4 sports.[sailing] //The businessman whose favorite sport is Sailing is seated at one of the ends.
        ties.[white] >>! ties.[green]  //The businessman wearing a White tie is directly after the businessman wearing a Green tie.
        ties.[blue] <<! ties.[yellow]
        between gates.[g89] (cities.[seoul],gates.[g34])  //The man departing from Gate 89 is situated between the man flying to Seoul and the man departing from Gate 34, in that order.
        between ties.[white] (quent,ties.[black]) //The businessman wearing a White tie is somewhere between Quent and the businessman wearing a Black tie, in that order.
        times.[t500] == john //John's flight also departs at 5:00 PM.
        gates.[g98] >>! times.[t400]  //The man departing from Gate 98 is directly after the man with the earliest flight.
        between sports.[snowboarding] (cities.[manila],sports.[judo]) //The person whose favorite sport involves a board is seated somewhere between the man flying to Manila and the person who enjoys a fighting sport, in that order.
        atEnd 4 paul //Paul is at one of the ends.
        cities.[manila] <<>> ties.[blue] //The businessman flying to Manila is next to the man wearing the Blue tie.
        gates.[g89] == ties.[yellow] //The man who wears a Yellow tie is also departing from Gate 89.
        times.[t700] == cities.[amsterdam] //The man flying to the capital of the Netherlands has the latest flight.
        sports.[diving] >>! ties.[yellow] //The person whose favorite sport involves a pool is to the right of the person wearing the Yellow tie.
        cities.[london] == times.[t500] //The man flying to London is also scheduled to depart at 5:00 PM.
        cities.[seoul] <<! cities.[manila] //The man flying to Seoul is directly before the man flying to Manila.
        cities.[manila] == times.[t430] //The flight to Manila departs at 4:30 PM.
        atEnd 4 cities.[amsterdam] //The person traveling to the capital of the Netherlands is seated at one of the ends.
    ]

let allDistinct : BoolExpr list = 
    [
        //ensure all values are distinct for times, gates, ...
        ctx.MkDistinct([|for t in T.Consts -> times.[t]|])
        ctx.MkDistinct([|for g in G.Consts -> gates.[g]|])
        ctx.MkDistinct([|for c in C.Consts -> cities.[c]|])
        ctx.MkDistinct([|for s in S.Consts -> sports.[s]|])
        ctx.MkDistinct([|for ti in Ti.Consts -> ties.[ti]|])
        ctx.MkDistinct([|for b in B.Consts -> positions.[b]|])
    ]

let seatConstraints : BoolExpr list =
    [
        for b in B.Consts do
            yield ctx.MkGe(pstnOf b,!0)
            yield ctx.MkLe(pstnOf b,!4)
    ]

let assertions = clues @ allDistinct @ seatConstraints |> Seq.toArray
let solver = ctx.MkSolver()
solver.Add(assertions)
solver.Check()

let bpTies = Ti.Consts |> Seq.map(fun ti -> solver.Model.Eval(ties.[ti]).ToString(),ti) |> Map.ofSeq
let bpCities = C.Consts |> Seq.map(fun c -> solver.Model.Eval(cities.[c]).ToString(),c) |> Map.ofSeq
let bpTimes = T.Consts |> Seq.map(fun t -> solver.Model.Eval(times.[t]).ToString(),t) |> Map.ofSeq
let bpGates = G.Consts |> Seq.map(fun g -> solver.Model.Eval(gates.[g]).ToString(),g) |> Map.ofSeq
let bpSports = S.Consts |> Seq.map(fun s -> solver.Model.Eval(sports.[s]).ToString(),s) |> Map.ofSeq
let bpSeats = B.Consts |> Seq.map(fun b -> solver.Model.Eval(positions.[b]).ToString() |> int,b) |> Map.ofSeq
;;
bpSeats 
|> Map.toSeq 
|> Seq.iter(fun (v,k) -> 
    let n = k.ToString()
    printfn "%s" n
    printfn "    %A" bpTies.[n]
    printfn "    %A" bpCities.[n]
    printfn "    %A" bpTimes.[n]
    printfn "    %A" bpGates.[n]
    printfn "    %A" bpSports.[n])
