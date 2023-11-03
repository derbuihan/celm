module Typed.Declaration exposing (TypedDeclaration(..), fromNodeDeclaration)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing (DeadEnd, Problem(..))
import Typed.Expression exposing (TypedExpression, TypedFunction, fromFunction, fromNodeExpression)
import Typed.Node exposing (Meta, Type(..), TypedNode(..), type_)
import Typed.Pattern exposing (TypedPattern, fromNodePattern)


type TypedDeclaration
    = TypedDestructuring (TypedNode TypedPattern) (TypedNode TypedExpression)
    | TypedFunctionDeclaration TypedFunction


fromNodeDeclaration : Meta -> Node Declaration -> Result (List DeadEnd) ( Meta, TypedNode TypedDeclaration )
fromNodeDeclaration meta_ (Node range_ node) =
    let
        { row, column } =
            range_.start
    in
    case node of
        Destructuring pattern expr_ ->
            let
                typedPattern : Result (List DeadEnd) ( Meta, TypedNode TypedPattern )
                typedPattern =
                    fromNodePattern meta_ pattern

                parttenMeta : Result (List DeadEnd) Meta
                parttenMeta =
                    typedPattern
                        |> Result.map (Tuple.first >> (\meta__ -> { meta__ | label = meta__.label + 1 }))

                typedExpression : Result (List DeadEnd) ( Meta, TypedNode TypedExpression )
                typedExpression =
                    parttenMeta
                        |> Result.andThen (\meta__ -> fromNodeExpression meta__ expr_)

                exprMeta : Result (List DeadEnd) Meta
                exprMeta =
                    typedExpression |> Result.map (\( e, _ ) -> e)

                lastMeta : Result (List DeadEnd) Meta
                lastMeta =
                    exprMeta |> Result.map (\meta__ -> { meta__ | range = range_, label = meta__.label + 1 })
            in
            Result.map3
                (\( _, ptrn ) meta__ ( _, expr ) -> ( meta__, TypedNode meta__ (TypedDestructuring ptrn expr) ))
                typedPattern
                lastMeta
                typedExpression

        FunctionDeclaration func ->
            let
                typedFunction : Result (List DeadEnd) ( Meta, TypedFunction )
                typedFunction =
                    fromFunction meta_ func

                lastMeta : Result (List DeadEnd) Meta
                lastMeta =
                    typedFunction |> Result.map (\( meta__, _ ) -> { meta__ | range = range_, label = meta__.label + 1 })
            in
            Result.map2
                (\meta__ ( _, f ) -> ( meta__, TypedNode meta__ (TypedFunctionDeclaration f) ))
                lastMeta
                typedFunction

        _ ->
            Err [ DeadEnd row column (Problem "Unsupported declaration") ]
