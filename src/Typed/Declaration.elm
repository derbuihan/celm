module Typed.Declaration exposing (..)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Parser exposing (DeadEnd, Problem(..))
import Typed.Expression exposing (TypedExpression, TypedFunction, fromFunction, fromNodeExpression)
import Typed.Node exposing (Type(..), TypedNode(..))
import Typed.Pattern exposing (TypedPattern, fromNodePattern)


type TypedDeclaration
    = TypedDestructuring (TypedNode TypedPattern) (TypedNode TypedExpression)
    | TypedFunctionDeclaration TypedFunction


fromNodeDeclaration : Node Declaration -> Result (List DeadEnd) (TypedNode TypedDeclaration)
fromNodeDeclaration (Node range_ node) =
    let
        typedDeclaration =
            fromDeclaration range_ node
    in
    Result.map (\d -> TypedNode { range = range_, type_ = Unit } d) typedDeclaration


fromDeclaration : Range -> Declaration -> Result (List DeadEnd) TypedDeclaration
fromDeclaration range_ decl =
    case decl of
        Destructuring pattern expr_ ->
            let
                typedPattern =
                    fromNodePattern pattern

                typedExpression =
                    fromNodeExpression expr_
            in
            Result.map2 (\p e -> TypedDestructuring p e) typedPattern typedExpression

        FunctionDeclaration func ->
            let
                typedFunction =
                    fromFunction range_ func
            in
            Result.map
                TypedFunctionDeclaration
                typedFunction

        _ ->
            Err [ DeadEnd range_.start.row range_.start.column (Problem "Unsupported declaration") ]
