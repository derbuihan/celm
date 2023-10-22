module Typed.Expression exposing (TypedExpression(..), TypedFunction, TypedFunctionImplementation, fromFunction, fromNodeExpression)

import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Parser exposing (DeadEnd, Problem(..))
import Typed.Node exposing (Type(..), TypedNode(..), type_)


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


fromNodeExpression : Node Expression -> Result (List DeadEnd) (TypedNode TypedExpression)
fromNodeExpression (Node range_ expr) =
    let
        { row, column } =
            range_.start
    in
    case expr of
        UnitExpr ->
            Ok (TypedNode { range = range_, type_ = Unit } TypedUnitExpr)

        OperatorApplication op dir left right ->
            let
                leftNode : Result (List DeadEnd) (TypedNode TypedExpression)
                leftNode =
                    fromNodeExpression left

                rightNode : Result (List DeadEnd) (TypedNode TypedExpression)
                rightNode =
                    fromNodeExpression right
            in
            case ( leftNode, rightNode ) of
                ( Ok (TypedNode lm lhs), Ok (TypedNode rm rhs) ) ->
                    case ( lm.type_, rm.type_ ) of
                        ( Int, Int ) ->
                            if List.member op [ "+", "-", "*", "/" ] then
                                Ok (TypedNode lm (TypedOperatorApplication op dir (TypedNode lm lhs) (TypedNode rm rhs)))

                            else if List.member op [ "==", "/=", "<", ">", "<=", ">=" ] then
                                Ok (TypedNode { range = range_, type_ = Bool } (TypedOperatorApplication op dir (TypedNode lm lhs) (TypedNode rm rhs)))

                            else
                                Err [ DeadEnd row column (Problem "Type: Unsupported operator application") ]

                        ( Bool, Bool ) ->
                            if List.member op [ "==", "/=", "<", ">", "<=", ">=" ] then
                                Ok (TypedNode { range = range_, type_ = Bool } (TypedOperatorApplication op dir (TypedNode lm lhs) (TypedNode rm rhs)))

                            else
                                Err [ DeadEnd row column (Problem "Type: Unsupported operator application") ]

                        _ ->
                            Err [ DeadEnd row column (Problem "Type: Unsupported operator application") ]

                ( Ok _, Err rhs_ ) ->
                    Err rhs_

                ( Err lhs_, Ok _ ) ->
                    Err lhs_

                ( Err lhs_, Err rhs_ ) ->
                    Err (lhs_ ++ rhs_)

        IfBlock cond then_ else_ ->
            let
                condNode : Result (List DeadEnd) (TypedNode TypedExpression)
                condNode =
                    fromNodeExpression cond

                thenNode : Result (List DeadEnd) (TypedNode TypedExpression)
                thenNode =
                    fromNodeExpression then_

                elseNode : Result (List DeadEnd) (TypedNode TypedExpression)
                elseNode =
                    fromNodeExpression else_
            in
            case ( condNode, thenNode, elseNode ) of
                ( Ok (TypedNode cm cond_), Ok (TypedNode tm then__), Ok (TypedNode em else__) ) ->
                    if cm.type_ == Bool && tm.type_ == em.type_ then
                        Ok (TypedNode tm (TypedIfBlock (TypedNode cm cond_) (TypedNode tm then__) (TypedNode em else__)))

                    else
                        Err [ DeadEnd row column (Problem "Type: If block condition must be a boolean and then and else must be of the same type") ]

                ( Err cond_, _, _ ) ->
                    Err cond_

                ( _, Err then__, _ ) ->
                    Err then__

                ( _, _, Err else__ ) ->
                    Err else__

        Integer int ->
            Ok (TypedNode { range = range_, type_ = Int } (TypedInteger int))

        Negation node ->
            let
                typedExpression : Result (List DeadEnd) (TypedNode TypedExpression)
                typedExpression =
                    fromNodeExpression node
            in
            case typedExpression of
                Ok (TypedNode nm node_) ->
                    if nm.type_ == Int then
                        Ok (TypedNode nm (TypedNegation (TypedNode nm node_)))

                    else
                        Err [ DeadEnd row column (Problem "Type: Negation must be applied to an integer") ]

                Err node_ ->
                    Err node_

        ParenthesizedExpression node ->
            let
                typedExpression : Result (List DeadEnd) (TypedNode TypedExpression)
                typedExpression =
                    fromNodeExpression node
            in
            Result.map (\node_ -> TypedNode { range = range_, type_ = type_ node_ } (TypedParenthesizedExpression node_)) typedExpression

        _ ->
            Err [ DeadEnd row column (Problem "Type: Unsupported expression") ]


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
        exprNode : Result (List DeadEnd) (TypedNode TypedExpression)
        exprNode =
            fromNodeExpression node.expression
    in
    Result.map (\expr_ -> TypedNode { range = range_, type_ = type_ expr_ } (TypedFunctionImplementation expr_)) exprNode
