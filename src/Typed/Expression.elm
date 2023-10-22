module Typed.Expression exposing (TypedExpression(..), TypedFunction, TypedFunctionImplementation, TypedLetDeclaration(..), fromFunction, fromNodeExpression)

import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation, LetBlock, LetDeclaration(..))
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing (DeadEnd, Problem(..))
import Typed.ModuleName exposing (TypedModuleName)
import Typed.Node exposing (Env, Type(..), TypedNode(..), countEnv, env, type_)


type TypedExpression
    = TypedUnitExpr
    | TypedOperatorApplication String InfixDirection (TypedNode TypedExpression) (TypedNode TypedExpression)
    | TypedFunctionOrValue TypedModuleName String
    | TypedIfBlock (TypedNode TypedExpression) (TypedNode TypedExpression) (TypedNode TypedExpression)
    | TypedInteger Int
    | TypedNegation (TypedNode TypedExpression)
    | TypedParenthesizedExpression (TypedNode TypedExpression)
    | TypedLetExpression TypedLetBlock


type alias TypedLetBlock =
    { declarations : List (TypedNode TypedLetDeclaration)
    , expression : TypedNode TypedExpression
    }


type TypedLetDeclaration
    = TypedLetFunction TypedFunction


type alias TypedFunction =
    { declaration : TypedNode TypedFunctionImplementation
    }


type alias TypedFunctionImplementation =
    { name : TypedNode String
    , expression : TypedNode TypedExpression
    }


fromLetBlock : Env -> LetBlock -> Result (List DeadEnd) TypedLetBlock
fromLetBlock env_ letb =
    let
        declarations : Result (List DeadEnd) (List (TypedNode TypedLetDeclaration))
        declarations =
            inferLetDeclarations env_ letb.declarations

        inferLetDeclarations : Env -> List (Node LetDeclaration) -> Result (List DeadEnd) (List (TypedNode TypedLetDeclaration))
        inferLetDeclarations env__ decls =
            case decls of
                [] ->
                    Ok []

                decl :: decls_ ->
                    let
                        typedDecl : Result (List DeadEnd) (TypedNode TypedLetDeclaration)
                        typedDecl =
                            fromNodeLetDeclaration env__ decl
                    in
                    case typedDecl of
                        Ok typedDecl_ ->
                            Result.map (\typedDecls_ -> typedDecl_ :: typedDecls_)
                                (inferLetDeclarations (typedDecl_ |> env |> countEnv) decls_)

                        Err deadEnds ->
                            Err deadEnds

        expression : Result (List DeadEnd) (TypedNode TypedExpression)
        expression =
            fromNodeExpression env_ letb.expression
    in
    case ( declarations, expression ) of
        ( Ok decls_, Ok expr_ ) ->
            Ok { declarations = decls_, expression = expr_ }

        ( Err decls_, _ ) ->
            Err decls_

        ( _, Err expr_ ) ->
            Err expr_


fromNodeLetDeclaration : Env -> Node LetDeclaration -> Result (List DeadEnd) (TypedNode TypedLetDeclaration)
fromNodeLetDeclaration env_ (Node range_ letdecl) =
    case letdecl of
        LetFunction func ->
            let
                typedFunction : Result (List DeadEnd) TypedFunction
                typedFunction =
                    fromFunction env_ func
            in
            Result.map
                (\func_ ->
                    TypedNode
                        { range = range_
                        , type_ = func_.declaration |> type_
                        , env = func_.declaration |> env |> countEnv
                        }
                        (TypedLetFunction func_)
                )
                typedFunction

        _ ->
            Err [ DeadEnd range_.start.row range_.start.column (Problem "Type: Unsupported let declaration") ]


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

        FunctionOrValue moduleName name ->
            Ok (TypedNode { range = range_, type_ = Int, env = env_ |> countEnv } (TypedFunctionOrValue moduleName name))

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

        LetExpression letblock ->
            let
                typedLetBlock : Result (List DeadEnd) TypedLetBlock
                typedLetBlock =
                    fromLetBlock env_ letblock
            in
            case typedLetBlock of
                Ok typedLetBlock_ ->
                    Ok
                        (TypedNode
                            { range = range_
                            , type_ = typedLetBlock_.expression |> type_
                            , env = typedLetBlock_.expression |> env |> countEnv
                            }
                            (TypedLetExpression typedLetBlock_)
                        )

                Err deadEnds ->
                    Err deadEnds

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
        nameNode : Result (List DeadEnd) (TypedNode String)
        nameNode =
            fromNodeString env_ node.name

        exprNode : Result (List DeadEnd) (TypedNode TypedExpression)
        exprNode =
            fromNodeExpression env_ node.expression
    in
    Result.map2
        (\name_ expr_ ->
            TypedNode { range = range_, type_ = type_ expr_, env = expr_ |> env |> countEnv } (TypedFunctionImplementation name_ expr_)
        )
        nameNode
        exprNode


fromNodeString : Env -> Node String -> Result (List DeadEnd) (TypedNode String)
fromNodeString env_ (Node range_ str) =
    Ok (TypedNode { range = range_, type_ = Int, env = env_ |> countEnv } str)
