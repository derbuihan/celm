module Typed.Expression exposing (TypedExpression(..), TypedFunction, TypedFunctionImplementation, fromFunction, fromNodeExpression)

import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing (DeadEnd, Problem(..))
import Typed.Node exposing (Env, Type(..), TypedNode(..), countEnv, env, type_)


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


fromNodeExpression : Env -> Node Expression -> Result (List DeadEnd) (TypedNode TypedExpression)
fromNodeExpression env_ (Node range_ expr) =
    let
        { row, column } =
            range_.start
    in
    case expr of
        UnitExpr ->
            Ok (TypedNode { range = range_, type_ = Unit, env = env_ |> countEnv } TypedUnitExpr)

        OperatorApplication op dir left right ->
            let
                leftNode : Result (List DeadEnd) (TypedNode TypedExpression)
                leftNode =
                    fromNodeExpression env_ left

                rightNode : Result (List DeadEnd) (TypedNode TypedExpression)
                rightNode =
                    leftNode
                        |> Result.andThen (\(TypedNode lm _) -> fromNodeExpression lm.env right)
            in
            case ( leftNode, rightNode ) of
                ( Ok (TypedNode lm lhs), Ok (TypedNode rm rhs) ) ->
                    case ( lm.type_, rm.type_ ) of
                        ( Int, Int ) ->
                            if List.member op [ "+", "-", "*", "/" ] then
                                Ok (TypedNode { range = range_, type_ = Int, env = rm.env |> countEnv } (TypedOperatorApplication op dir (TypedNode lm lhs) (TypedNode rm rhs)))

                            else if List.member op [ "==", "/=", "<", ">", "<=", ">=" ] then
                                Ok (TypedNode { range = range_, type_ = Bool, env = rm.env |> countEnv } (TypedOperatorApplication op dir (TypedNode lm lhs) (TypedNode rm rhs)))

                            else
                                Err [ DeadEnd row column (Problem "Type: Unsupported operator application") ]

                        ( Bool, Bool ) ->
                            if List.member op [ "==", "/=", "<", ">", "<=", ">=" ] then
                                Ok (TypedNode { range = range_, type_ = Bool, env = rm.env |> countEnv } (TypedOperatorApplication op dir (TypedNode lm lhs) (TypedNode rm rhs)))

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
                    fromNodeExpression env_ cond

                thenNode : Result (List DeadEnd) (TypedNode TypedExpression)
                thenNode =
                    condNode
                        |> Result.andThen (\(TypedNode cm _) -> fromNodeExpression cm.env then_)

                elseNode : Result (List DeadEnd) (TypedNode TypedExpression)
                elseNode =
                    thenNode
                        |> Result.andThen (\(TypedNode tm _) -> fromNodeExpression tm.env else_)
            in
            case ( condNode, thenNode, elseNode ) of
                ( Ok (TypedNode cm cond_), Ok (TypedNode tm then__), Ok (TypedNode em else__) ) ->
                    if cm.type_ == Bool && tm.type_ == em.type_ then
                        Ok (TypedNode { range = range_, type_ = tm.type_, env = em.env |> countEnv } (TypedIfBlock (TypedNode cm cond_) (TypedNode tm then__) (TypedNode em else__)))

                    else
                        Err [ DeadEnd row column (Problem "Type: If block condition must be a boolean and then and else must be of the same type") ]

                ( Err err, _, _ ) ->
                    Err err

                ( _, Err err, _ ) ->
                    Err err

                ( _, _, Err err ) ->
                    Err err

        Integer int ->
            Ok (TypedNode { range = range_, type_ = Int, env = env_ |> countEnv } (TypedInteger int))

        Negation node ->
            let
                typedExpression : Result (List DeadEnd) (TypedNode TypedExpression)
                typedExpression =
                    fromNodeExpression env_ node
            in
            case typedExpression of
                Ok (TypedNode nm node_) ->
                    if nm.type_ == Int then
                        Ok (TypedNode { range = range_, type_ = Int, env = nm.env |> countEnv } (TypedNegation (TypedNode nm node_)))

                    else
                        Err [ DeadEnd row column (Problem "Type: Negation must be applied to an integer") ]

                Err node_ ->
                    Err node_

        ParenthesizedExpression node ->
            let
                typedExpression : Result (List DeadEnd) (TypedNode TypedExpression)
                typedExpression =
                    fromNodeExpression env_ node
            in
            Result.map (\node_ -> TypedNode { range = range_, type_ = type_ node_, env = node_ |> env |> countEnv } (TypedParenthesizedExpression node_)) typedExpression

        _ ->
            Err [ DeadEnd row column (Problem "Type: Unsupported expression") ]


fromFunction : Env -> Function -> Result (List DeadEnd) TypedFunction
fromFunction env_ func =
    let
        typedFunction : Result (List DeadEnd) (TypedNode TypedFunctionImplementation)
        typedFunction =
            fromNodeFunctionImplementation env_ func.declaration
    in
    Result.map (\d -> { declaration = d }) typedFunction


fromNodeFunctionImplementation : Env -> Node FunctionImplementation -> Result (List DeadEnd) (TypedNode TypedFunctionImplementation)
fromNodeFunctionImplementation env_ (Node range_ node) =
    let
        exprNode : Result (List DeadEnd) (TypedNode TypedExpression)
        exprNode =
            fromNodeExpression env_ node.expression
    in
    Result.map (\expr_ -> TypedNode { range = range_, type_ = type_ expr_, env = expr_ |> env |> countEnv } (TypedFunctionImplementation expr_)) exprNode
