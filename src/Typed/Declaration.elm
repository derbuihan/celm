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


fromNodeDeclaration : Env -> Node Declaration -> Result (List DeadEnd) ( Env, TypedNode TypedDeclaration )
fromNodeDeclaration env_ (Node range_ node) =
    let
        { row, column } =
            range_.start
    in
    case node of
        Destructuring pattern expr_ ->
            let
                typedPattern : Result (List DeadEnd) ( Env, TypedNode TypedPattern )
                typedPattern =
                    fromNodePattern env_ pattern

                parttenEnv : Result (List DeadEnd) Env
                parttenEnv =
                    typedPattern |> Result.map (\( e, _ ) -> e |> countLabel)

                typedExpression : Result (List DeadEnd) ( Env, TypedNode TypedExpression )
                typedExpression =
                    parttenEnv
                        |> Result.andThen (\env__ -> fromNodeExpression env_ expr_)

                exprEnv : Result (List DeadEnd) Env
                exprEnv =
                    typedExpression |> Result.map (\( e, _ ) -> e)

                lastEnv : Result (List DeadEnd) Env
                lastEnv =
                    exprEnv |> Result.map (\e -> e |> countLabel)
            in
            Result.map3
                (\( _, ptrn ) env__ ( _, expr ) -> ( env__, TypedNode { range = range_, type_ = type_ expr, env = env__ } (TypedDestructuring ptrn expr) ))
                typedPattern
                lastEnv
                typedExpression

        FunctionDeclaration func ->
            let
                typedFunction : Result (List DeadEnd) ( Env, TypedFunction )
                typedFunction =
                    fromFunction env_ func

                lastEnv : Result (List DeadEnd) Env
                lastEnv =
                    typedFunction |> Result.map (\( e, _ ) -> e |> countLabel)
            in
            Result.map2
                (\e ( _, f ) -> ( e, TypedNode { range = range_, type_ = Unit, env = e } (TypedFunctionDeclaration f) ))
                lastEnv
                typedFunction

        _ ->
            Err [ DeadEnd row column (Problem "Unsupported declaration") ]
