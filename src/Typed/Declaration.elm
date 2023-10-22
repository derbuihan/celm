module Typed.Declaration exposing (TypedDeclaration(..), fromNodeDeclaration)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing (DeadEnd, Problem(..))
import Typed.Expression exposing (TypedExpression, TypedFunction, fromFunction, fromNodeExpression)
import Typed.Node exposing (Type(..), TypedNode(..), type_)
import Typed.Pattern exposing (TypedPattern, fromNodePattern)


type TypedDeclaration
    = TypedDestructuring (TypedNode TypedPattern) (TypedNode TypedExpression)
    | TypedFunctionDeclaration TypedFunction


fromNodeDeclaration : Node Declaration -> Result (List DeadEnd) (TypedNode TypedDeclaration)
fromNodeDeclaration (Node range_ node) =
    let
        { row, column } =
            range_.start
    in
    case node of
        Destructuring pattern expr_ ->
            let
                typedPattern : Result (List DeadEnd) (TypedNode TypedPattern)
                typedPattern =
                    fromNodePattern pattern

                typedExpression : Result (List DeadEnd) (TypedNode TypedExpression)
                typedExpression =
                    fromNodeExpression expr_
            in
            Result.map2 (\p e -> TypedNode { range = range_, type_ = type_ e } (TypedDestructuring p e)) typedPattern typedExpression

        FunctionDeclaration func ->
            let
                typedFunction : Result (List DeadEnd) TypedFunction
                typedFunction =
                    fromFunction range_ func
            in
            Result.map
                (\f -> TypedNode { range = range_, type_ = Unit } (TypedFunctionDeclaration f))
                typedFunction

        _ ->
            Err [ DeadEnd row column (Problem "Unsupported declaration") ]
