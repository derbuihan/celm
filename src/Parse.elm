module Parse exposing (parse)

import Parser exposing ((|.), (|=), Parser, Step(..), andThen, chompWhile, int, lazy, oneOf, succeed, symbol)
import Type exposing (Expr(..))



-- expr = mul ("+" mul | "-" mul)*


expr : Parser Expr
expr =
    let
        exprHelp : Expr -> Parser Expr
        exprHelp left =
            oneOf
                [ succeed (Add left)
                    |. symbol "+"
                    |= lazy (\_ -> mul)
                    |> andThen exprHelp
                , succeed (Sub left)
                    |. symbol "-"
                    |= lazy (\_ -> mul)
                    |> andThen exprHelp
                , succeed left
                ]
    in
    mul |> andThen exprHelp



-- mul = unary ("*" unary | "//" unary)*


mul : Parser Expr
mul =
    let
        mulHelp : Expr -> Parser Expr
        mulHelp left =
            oneOf
                [ succeed (Mul left)
                    |. symbol "*"
                    |= lazy (\_ -> unary)
                    |> andThen mulHelp
                , succeed (Div left)
                    |. symbol "//"
                    |= lazy (\_ -> unary)
                    |> andThen mulHelp
                , succeed left
                ]
    in
    unary |> andThen mulHelp



-- unary = "-"? primary


unary : Parser Expr
unary =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed Neg
                |. symbol "-"
                |= primary
            , succeed identity
                |= primary
            ]



-- primary = int | "(" expr ")"


primary : Parser Expr
primary =
    succeed identity
        |= oneOf
            [ succeed Int
                |= int
            , succeed identity
                |. symbol "("
                |= lazy (\_ -> expr)
                |. symbol ")"
            ]
        |. spaces


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ' || c == '\t')


parse : String -> Result (List Parser.DeadEnd) Expr
parse p =
    Parser.run expr p
