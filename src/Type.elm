module Type exposing (..)


type Expr
    = Integer Int
    | Plus Expr Expr
    | Minus Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
