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



-- expr = term (PLUS term | MINUS term)*


expr : Parser Expr
expr =
    term
        |> andThen
            (\left ->
                oneOf
                    [ succeed (\right -> Plus left right)
                        |. symbol "+"
                        |= lazy (\_ -> expr)
                    , succeed (\right -> Minus left right)
                        |. symbol "-"
                        |= lazy (\_ -> expr)
                    , succeed left
                    ]
            )



-- term = primary (MUL primary | DIV primary)*


term : Parser Expr
term =
    primary
        |> andThen
            (\left ->
                oneOf
                    [ succeed (Mul left)
                        |. symbol "*"
                        |= lazy (\_ -> term)
                    , succeed (Div left)
                        |. symbol "//"
                        |= lazy (\_ -> term)
                    , succeed left
                    ]
            )



-- primary = INT | "(" expr ")"


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
