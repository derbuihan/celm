module Typed.Declaration exposing (TypedDeclaration(..), fromNodeDeclaration)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing (DeadEnd, Problem(..))
import Typed.Expression exposing (TypedExpression, TypedFunction, fromFunction, fromNodeExpression)
import Typed.Node exposing (Env, Type(..), TypedNode(..), countLabel, env, type_)
import Typed.Pattern exposing (TypedPattern, fromNodePattern)


type TypedDeclaration
    = TypedDestructuring (TypedNode TypedPattern) (TypedNode TypedExpression)
    | TypedFunctionDeclaration TypedFunction


fromNodeDeclaration : Env -> Node Declaration -> Result (List DeadEnd) (TypedNode TypedDeclaration)
fromNodeDeclaration env_ (Node range_ node) =
    let
        { row, column } =
            range_.start
    in
    case node of
        Destructuring pattern expr_ ->
            let
                typedPattern : Result (List DeadEnd) (TypedNode TypedPattern)
                typedPattern =
                    fromNodePattern env_ pattern

                typedExpression : Result (List DeadEnd) (TypedNode TypedExpression)
                typedExpression =
                    typedPattern
                        |> Result.andThen (\p -> fromNodeExpression (p |> env) expr_)
            in
            Result.map2
                (\p e -> TypedNode { range = range_, type_ = type_ e, env = e |> env |> countLabel } (TypedDestructuring p e))
                typedPattern
                typedExpression

        FunctionDeclaration func ->
            let
                typedFunction : Result (List DeadEnd) TypedFunction
                typedFunction =
                    fromFunction env_ func
            in
            Result.map
                (\f -> TypedNode { range = range_, type_ = Unit, env = f.declaration |> env |> countLabel } (TypedFunctionDeclaration f))
                typedFunction

        _ ->
            Err [ DeadEnd row column (Problem "Unsupported declaration") ]
