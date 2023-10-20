module Typed.Expression exposing (..)

import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Parser exposing (DeadEnd, Problem(..))
import Typed.Node exposing (Meta, Type(..), TypedNode(..))


type TypedExpression
    = TypedUnitExpr
    | TypedOperatorApplication String InfixDirection (TypedNode TypedExpression) (TypedNode TypedExpression)
    | TypedFunctionOrValue ModuleName String
    | TypedIfBlock (TypedNode TypedExpression) (TypedNode TypedExpression) (TypedNode TypedExpression)
    | TypedInteger Int
    | TypedNegation (TypedNode TypedExpression)
    | TypedParenthesizedExpression (TypedNode TypedExpression)


type alias TypedFunction =
    { declaration : TypedNode TypedFunctionImplementation
    }


type alias TypedFunctionImplementation =
    { expression : TypedNode TypedExpression
    }


getMetaExpression : Range -> Expression -> Result (List DeadEnd) Meta
getMetaExpression range_ expr =
    case expr of
        Integer _ ->
            Ok { range = range_, type_ = Int }

        ParenthesizedExpression node ->
            let
                meta_ =
                    getMetaNodeExpression node
            in
            Result.map (\m -> { range = range_, type_ = m.type_ }) meta_

        Negation node ->
            let
                meta_ =
                    getMetaNodeExpression node
            in
            Result.map (\m -> { range = range_, type_ = m.type_ }) meta_

        OperatorApplication op _ left right ->
            let
                lhs =
                    getMetaNodeExpression left

                rhs =
                    getMetaNodeExpression right
            in
            case ( lhs, rhs ) of
                ( Ok lhs_, Ok rhs_ ) ->
                    case ( lhs_.type_, rhs_.type_ ) of
                        ( Int, Int ) ->
                            if List.member op [ "+", "-", "*", "/" ] then
                                Ok { range = range_, type_ = Int }

                            else if List.member op [ "==", "/=", "<", ">", "<=", ">=" ] then
                                Ok { range = range_, type_ = Bool }

                            else
                                Err [ DeadEnd range_.start.row range_.start.column (Problem "Unsupported operator") ]

                        _ ->
                            Err [ DeadEnd range_.start.row range_.start.column (Problem "Unsupported expression") ]

                _ ->
                    Err [ DeadEnd range_.start.row range_.start.column (Problem "Unsupported expression") ]

        _ ->
            Err [ DeadEnd range_.start.row range_.start.column (Problem "Unsupported expression") ]


getMetaNodeExpression : Node Expression -> Result (List DeadEnd) Meta
getMetaNodeExpression (Node r e) =
    getMetaExpression r e


fromNodeExpression : Node Expression -> Result (List DeadEnd) (TypedNode TypedExpression)
fromNodeExpression (Node range_ node) =
    let
        meta_ =
            getMetaNodeExpression (Node range_ node)

        typedExpression =
            fromExpression range_ node
    in
    Result.map2 (\m e -> TypedNode m e) meta_ typedExpression


fromExpression : Range -> Expression -> Result (List DeadEnd) TypedExpression
fromExpression range_ expr =
    case expr of
        UnitExpr ->
            Ok TypedUnitExpr

        Integer int ->
            Ok (TypedInteger int)

        Negation node ->
            let
                typedExpression =
                    fromNodeExpression node
            in
            typedExpression |> Result.map TypedNegation

        ParenthesizedExpression node ->
            let
                typedExpression =
                    fromNodeExpression node
            in
            typedExpression |> Result.map TypedParenthesizedExpression

        OperatorApplication op dir left right ->
            let
                lhs =
                    fromNodeExpression left

                rhs =
                    fromNodeExpression right
            in
            Result.map2 (\lhs_ rhs_ -> TypedOperatorApplication op dir lhs_ rhs_) lhs rhs

        _ ->
            Err [ DeadEnd range_.start.row range_.start.column (Problem "Unsupported expression") ]


fromFunction : Range -> Function -> Result (List DeadEnd) TypedFunction
fromFunction _ func =
    let
        typedFunction =
            fromNodeFunctionImplementation func.declaration
    in
    Result.map (\d -> { declaration = d }) typedFunction


fromNodeFunctionImplementation : Node FunctionImplementation -> Result (List DeadEnd) (TypedNode TypedFunctionImplementation)
fromNodeFunctionImplementation (Node range_ node) =
    let
        typedFunctionImplementation =
            fromFunctionImplementation range_ node
    in
    Result.map (\d -> TypedNode { range = range_, type_ = Int } d) typedFunctionImplementation


fromFunctionImplementation : Range -> FunctionImplementation -> Result (List DeadEnd) TypedFunctionImplementation
fromFunctionImplementation _ funcimpl =
    let
        typedExpression =
            fromNodeExpression funcimpl.expression
    in
    Result.map (\expr_ -> TypedFunctionImplementation expr_) typedExpression
