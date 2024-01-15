#r "nuget: Microsoft.Z3"
open Microsoft.Z3
let ctx = new Context()

//utility operators
let inline (!) a = match box a with :? int as a -> ctx.MkInt(a) | _ -> failwith "not handled"
let inline (<<==) (a:ArrayExpr) (b:Expr,c:Expr) =  ctx.MkStore(a,b,c) 
let (==>>) a b = ctx.MkImplies(a,b)
let inline (==) a b = ctx.MkEq(a,b)
let inline (<<) a b = ctx.MkLt(a,b)
let inline (<<=) a b = ctx.MkLe(a,b)
let inline (>>=) a b = ctx.MkGe(a,b)
let inline (>>) a b = ctx.MkGt(a,b)
let inline (&&&&) a b = ctx.MkAnd(a,b)
let inline (||||) a b = ctx.MkOr(a,b)

let mutable positions : ArrayExpr = Unchecked.defaultof<_>

//seating operators and functions
let pstnOf (a:Expr) = positions.[a] :?> ArithExpr

///is left of 
let (<<!) (a:Expr) (b:Expr) = (pstnOf a) + 1 == pstnOf b

//is right of
let (>>!) (a:Expr) (b:Expr) = (pstnOf a) - 1 == pstnOf b

///next to 
let (<<>>) (a:Expr) (b:Expr) = (a <<! b) |||| (b <<! a)

let atEnd (a:Expr) = ((pstnOf a) == !0) |||| ((pstnOf a) == !4)

let between (a:Expr) (b:Expr,c:Expr) = 
    let sa,sb,sc = pstnOf a, pstnOf b, pstnOf c
    (sa >> sb) &&&& (sa << sc)
