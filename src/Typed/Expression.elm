module Typed.Expression exposing (TypedExpression(..), TypedFunction, TypedFunctionImplementation, fromFunction, fromNodeExpression)

import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Parser exposing (DeadEnd, Problem(..))
import Typed.Node exposing (Meta, Type(..), TypedNode(..))


type TypedExpression
    = TypedUnitExpr
    | TypedOperatorApplication String InfixDirection (TypedNode TypedExpression) (TypedNode TypedExpression)
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
        UnitExpr ->
            Ok { range = range_, type_ = Unit }

        OperatorApplication op _ left right ->
            let
                lhs : Result (List DeadEnd) Meta
                lhs =
                    getMetaNodeExpression left

                rhs : Result (List DeadEnd) Meta
                rhs =
                    getMetaNodeExpression right
            in
            case ( Result.map .type_ lhs, Result.map .type_ rhs ) of
                ( Ok Int, Ok Int ) ->
                    if List.member op [ "+", "-", "*", "/" ] then
                        Ok { range = range_, type_ = Int }

                    else if List.member op [ "==", "/=", "<", ">", "<=", ">=" ] then
                        Ok { range = range_, type_ = Bool }

                    else
                        Err [ DeadEnd range_.start.row range_.start.column (Problem "Type: Unsupported expression") ]

                _ ->
                    Err [ DeadEnd range_.start.row range_.start.column (Problem "Type: Unsupported expression") ]

        IfBlock condition then_ else_ ->
            let
                condition_ : Result (List DeadEnd) Meta
                condition_ =
                    getMetaNodeExpression condition

                then__ : Result (List DeadEnd) Meta
                then__ =
                    getMetaNodeExpression then_

                else__ : Result (List DeadEnd) Meta
                else__ =
                    getMetaNodeExpression else_
            in
            case ( condition_, then__, else__ ) of
                ( Ok condition__, Ok then___, Ok else___ ) ->
                    if condition__.type_ == Bool && then___.type_ == else___.type_ then
                        Ok { range = range_, type_ = then___.type_ }

                    else
                        Err [ DeadEnd range_.start.row range_.start.column (Problem "Type: Unsupported expression") ]

                ( Err errs, _, _ ) ->
                    Err errs

                ( _, Err errs, _ ) ->
                    Err errs

                ( _, _, Err errs ) ->
                    Err errs

        Integer _ ->
            Ok { range = range_, type_ = Int }

        ParenthesizedExpression node ->
            let
                meta_ : Result (List DeadEnd) Meta
                meta_ =
                    getMetaNodeExpression node
            in
            Result.map (\m -> { range = range_, type_ = m.type_ }) meta_

        Negation node ->
            let
                meta_ : Result (List DeadEnd) Meta
                meta_ =
                    getMetaNodeExpression node
            in
            Result.map (\m -> { range = range_, type_ = m.type_ }) meta_

        _ ->
            Err [ DeadEnd range_.start.row range_.start.column (Problem "Type: Unsupported expression") ]


getMetaNodeExpression : Node Expression -> Result (List DeadEnd) Meta
getMetaNodeExpression (Node r e) =
    getMetaExpression r e


fromNodeExpression : Node Expression -> Result (List DeadEnd) (TypedNode TypedExpression)
fromNodeExpression (Node range_ node) =
    let
        meta_ : Result (List DeadEnd) Meta
        meta_ =
            getMetaNodeExpression (Node range_ node)

        typedExpression : Result (List DeadEnd) TypedExpression
        typedExpression =
            fromExpression range_ node
    in
    Result.map2 (\m e -> TypedNode m e) meta_ typedExpression


fromExpression : Range -> Expression -> Result (List DeadEnd) TypedExpression
fromExpression range_ expr =
    case expr of
        UnitExpr ->
            Ok TypedUnitExpr

        OperatorApplication op dir left right ->
            let
                lhs : Result (List DeadEnd) (TypedNode TypedExpression)
                lhs =
                    fromNodeExpression left

                rhs : Result (List DeadEnd) (TypedNode TypedExpression)
                rhs =
                    fromNodeExpression right
            in
            Result.map2 (\lhs_ rhs_ -> TypedOperatorApplication op dir lhs_ rhs_) lhs rhs

        IfBlock condition then_ else_ ->
            let
                condition_ : Result (List DeadEnd) (TypedNode TypedExpression)
                condition_ =
                    fromNodeExpression condition

                then__ : Result (List DeadEnd) (TypedNode TypedExpression)
                then__ =
                    fromNodeExpression then_

                else__ : Result (List DeadEnd) (TypedNode TypedExpression)
                else__ =
                    fromNodeExpression else_
            in
            Result.map3 (\condition__ then___ else___ -> TypedIfBlock condition__ then___ else___) condition_ then__ else__

        Integer int ->
            Ok (TypedInteger int)

        Negation node ->
            let
                typedExpression : Result (List DeadEnd) (TypedNode TypedExpression)
                typedExpression =
                    fromNodeExpression node
            in
            typedExpression |> Result.map TypedNegation

        ParenthesizedExpression node ->
            let
                typedExpression : Result (List DeadEnd) (TypedNode TypedExpression)
                typedExpression =
                    fromNodeExpression node
            in
            typedExpression |> Result.map TypedParenthesizedExpression

        _ ->
            Err [ DeadEnd range_.start.row range_.start.column (Problem "Type: Unsupported expression") ]


fromFunction : Range -> Function -> Result (List DeadEnd) TypedFunction
fromFunction _ func =
    let
        typedFunction : Result (List DeadEnd) (TypedNode TypedFunctionImplementation)
        typedFunction =
            fromNodeFunctionImplementation func.declaration
    in
    Result.map (\d -> { declaration = d }) typedFunction


fromNodeFunctionImplementation : Node FunctionImplementation -> Result (List DeadEnd) (TypedNode TypedFunctionImplementation)
fromNodeFunctionImplementation (Node range_ node) =
    let
        typedFunctionImplementation : Result (List DeadEnd) TypedFunctionImplementation
        typedFunctionImplementation =
            fromFunctionImplementation range_ node
    in
    Result.map (\d -> TypedNode { range = range_, type_ = Int } d) typedFunctionImplementation


fromFunctionImplementation : Range -> FunctionImplementation -> Result (List DeadEnd) TypedFunctionImplementation
fromFunctionImplementation _ funcimpl =
    let
        typedExpression : Result (List DeadEnd) (TypedNode TypedExpression)
        typedExpression =
            fromNodeExpression funcimpl.expression
    in
    Result.map (\expr_ -> TypedFunctionImplementation expr_) typedExpression
