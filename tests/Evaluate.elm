module Evaluate exposing (evaluate)

import Type exposing (Expr(..))


evaluate : Expr -> Int
evaluate e =
    case e of
        Int x ->
            x

        Neg x ->
            -(evaluate x)

        Add x y ->
            evaluate x + evaluate y

        Sub x y ->
            evaluate x - evaluate y

        Mul x y ->
            evaluate x * evaluate y

        Div x y ->
            evaluate x // evaluate y
