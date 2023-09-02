module Parse exposing (parse)

import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , int
        , lazy
        , oneOf
        , succeed
        , symbol
        )
import Type exposing (Expr(..))



-- expr = term ("+" term | "-" term)*


expr : Parser Expr
expr =
    let
        exprHelp : Expr -> Parser Expr
        exprHelp left =
            oneOf
                [ succeed (Plus left)
                    |. symbol "+"
                    |= lazy (\_ -> term)
                    |> andThen exprHelp
                , succeed (Minus left)
                    |. symbol "-"
                    |= lazy (\_ -> term)
                    |> andThen exprHelp
                , succeed left
                ]
    in
    term
        |> andThen exprHelp



-- term = primary ("*" primary | "//" primary)?


term : Parser Expr
term =
    let
        termHelp : Expr -> Parser Expr
        termHelp left =
            oneOf
                [ succeed (Mul left)
                    |. symbol "*"
                    |= lazy (\_ -> primary)
                    |> andThen termHelp
                , succeed (Div left)
                    |. symbol "//"
                    |= lazy (\_ -> primary)
                    |> andThen termHelp
                , succeed left
                ]
    in
    primary |> andThen termHelp



-- primary = int | "(" expr ")"


primary : Parser Expr
primary =
    oneOf
        [ succeed Integer
            |= int
        , succeed identity
            |. symbol "("
            |= lazy (\_ -> expr)
            |. symbol ")"
        ]


parse : String -> Result (List Parser.DeadEnd) Expr
parse p =
    Parser.run expr p
