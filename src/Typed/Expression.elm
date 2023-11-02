module Typed.Expression exposing (TypedExpression(..), TypedFunction, TypedFunctionImplementation, TypedLetDeclaration(..), fromFunction, fromNodeExpression, fromNodeLetDeclaration)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation, LetBlock, LetDeclaration(..))
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.Node exposing (Node(..))
import List.Extra as List
import Parser exposing (DeadEnd, Problem(..))
import Typed.ModuleName exposing (TypedModuleName)
import Typed.Node exposing (Env, Type(..), TypedNode(..), addRequiredVariable, countLabel, env, getLastEnv, inferNodes, insertOffset, resetOffsets, resetRequiredVariables, type_, value)


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


inferLetDeclarations : Env -> List (Node LetDeclaration) -> Result (List DeadEnd) (List ( Env, TypedNode TypedLetDeclaration ))
inferLetDeclarations env_ decls =
    case decls of
        [] ->
            Ok []

        decl :: decls_ ->
            case decl of
                Node _ (LetFunction func) ->
                    let
                        name : String
                        name =
                            func.declaration |> (\(Node _ decl_) -> decl_.name) |> (\(Node _ name_) -> name_)

                        typedDecl : Result (List DeadEnd) ( Env, TypedNode TypedLetDeclaration )
                        typedDecl =
                            fromNodeLetDeclaration (env_ |> insertOffset name |> resetRequiredVariables) decl

                        lastEnv : Result (List DeadEnd) Env
                        lastEnv =
                            typedDecl |> Result.map Tuple.first
                    in
                    case ( lastEnv, typedDecl ) of
                        ( Ok lastEnv_, Ok ( _, decl_ ) ) ->
                            Result.map (\typedDecls_ -> ( lastEnv_, decl_ ) :: typedDecls_) (inferLetDeclarations lastEnv_ decls_)

                        ( Err err, _ ) ->
                            Err err

                        ( _, Err err ) ->
                            Err err

                Node _ (LetDestructuring _ _) ->
                    Err [ DeadEnd 0 0 (Problem "Type: Destructuring is not supported") ]


fromLetBlock : Env -> LetBlock -> Result (List DeadEnd) ( Env, TypedLetBlock )
fromLetBlock env_ letBlock =
    let
        declarations : Result (List DeadEnd) ( List Env, List (TypedNode TypedLetDeclaration) )
        declarations =
            inferLetDeclarations env_ letBlock.declarations |> Result.map List.unzip

        declsLastEnv : Result (List DeadEnd) Env
        declsLastEnv =
            declarations |> Result.map (Tuple.first >> List.last >> Maybe.withDefault env_ >> resetRequiredVariables)

        expression : Result (List DeadEnd) ( Env, TypedNode TypedExpression )
        expression =
            declsLastEnv
                |> Result.andThen (\env__ -> fromNodeExpression env__ letBlock.expression)

        lastEnv : Result (List DeadEnd) Env
        lastEnv =
            expression |> Result.map (Tuple.first >> resetRequiredVariables)
    in
    case ( lastEnv, declarations, expression ) of
        ( Ok lastEnv_, Ok ( _, decls_ ), Ok ( _, expr_ ) ) ->
            Ok ( lastEnv_, { declarations = decls_, expression = expr_ } )

        ( Err err, _, _ ) ->
            Err err

        ( _, Err err, _ ) ->
            Err err

        ( _, _, Err err ) ->
            Err err


fromNodeLetDeclaration : Env -> Node LetDeclaration -> Result (List DeadEnd) ( Env, TypedNode TypedLetDeclaration )
fromNodeLetDeclaration env_ (Node range_ letdecl) =
    let
        { row, column } =
            range_.start
    in
    case letdecl of
        LetFunction func ->
            let
                typedFunction : Result (List DeadEnd) ( Env, TypedFunction )
                typedFunction =
                    fromFunction env_ func

                funcEnv : Result (List DeadEnd) Env
                funcEnv =
                    typedFunction |> Result.map Tuple.first

                lastEnv : Result (List DeadEnd) Env
                lastEnv =
                    funcEnv |> Result.map countLabel
            in
            case ( lastEnv, typedFunction ) of
                ( Ok lastEnv_, Ok ( _, func_ ) ) ->
                    Ok
                        ( lastEnv_
                        , TypedNode
                            { range = range_, type_ = func_.declaration |> type_, env = lastEnv_ }
                            (TypedLetFunction func_)
                        )

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

        _ ->
            Err [ DeadEnd row column (Problem "Type: Unsupported let declaration") ]


fromNodeExpression : Env -> Node Expression -> Result (List DeadEnd) ( Env, TypedNode TypedExpression )
fromNodeExpression env_ (Node range_ expr) =
    let
        { row, column } =
            range_.start
    in
    case expr of
        UnitExpr ->
            let
                lastEnv =
                    env_ |> countLabel
            in
            Ok ( lastEnv, TypedNode { range = range_, type_ = Unit, env = lastEnv } TypedUnitExpr )

        OperatorApplication op dir left right ->
            let
                leftNode : Result (List DeadEnd) ( Env, TypedNode TypedExpression )
                leftNode =
                    fromNodeExpression env_ left

                leftEnv : Result (List DeadEnd) Env
                leftEnv =
                    leftNode |> Result.map (\( env__, _ ) -> env__)

                rightNode : Result (List DeadEnd) ( Env, TypedNode TypedExpression )
                rightNode =
                    leftEnv
                        |> Result.andThen (\env__ -> fromNodeExpression env__ right)

                rightEnv : Result (List DeadEnd) Env
                rightEnv =
                    rightNode |> Result.map (\( env__, _ ) -> env__)

                lastEnv : Result (List DeadEnd) Env
                lastEnv =
                    rightEnv |> Result.map countLabel
            in
            case ( lastEnv, leftNode, rightNode ) of
                ( Ok lastEnv_, Ok ( _, TypedNode lm lhs ), Ok ( _, TypedNode rm rhs ) ) ->
                    case ( lm.type_, rm.type_ ) of
                        ( Int, Int ) ->
                            if List.member op [ "+", "-", "*", "/" ] then
                                Ok
                                    ( lastEnv_
                                    , TypedNode { range = range_, type_ = Int, env = lastEnv_ }
                                        (TypedOperatorApplication op dir (TypedNode lm lhs) (TypedNode rm rhs))
                                    )

                            else if List.member op [ "==", "/=", "<", ">", "<=", ">=" ] then
                                Ok
                                    ( lastEnv_
                                    , TypedNode { range = range_, type_ = Bool, env = lastEnv_ }
                                        (TypedOperatorApplication op dir (TypedNode lm lhs) (TypedNode rm rhs))
                                    )

                            else
                                Err [ DeadEnd row column (Problem "Type: Unsupported operator application") ]

                        ( Bool, Bool ) ->
                            if List.member op [ "==", "/=", "<", ">", "<=", ">=" ] then
                                Ok
                                    ( lastEnv_
                                    , TypedNode { range = range_, type_ = Bool, env = lastEnv_ }
                                        (TypedOperatorApplication op dir (TypedNode lm lhs) (TypedNode rm rhs))
                                    )

                            else
                                Err [ DeadEnd row column (Problem "Type: Unsupported operator application") ]

                        _ ->
                            Err [ DeadEnd row column (Problem "Type: Unsupported operator application") ]

                ( Err err, _, _ ) ->
                    Err err

                ( _, Err err, _ ) ->
                    Err err

                ( _, _, Err err ) ->
                    Err err

        FunctionOrValue moduleName name ->
            if name == "True" || name == "False" then
                let
                    lastEnv : Env
                    lastEnv =
                        env_ |> countLabel
                in
                Ok ( lastEnv, TypedNode { range = range_, type_ = Bool, env = lastEnv } (TypedFunctionOrValue moduleName name) )

            else
                let
                    lastEnv : Env
                    lastEnv =
                        env_ |> countLabel |> addRequiredVariable name Int
                in
                Ok
                    ( lastEnv
                    , TypedNode
                        { range = range_, type_ = Int, env = lastEnv }
                        (TypedFunctionOrValue moduleName name)
                    )

        IfBlock cond then_ else_ ->
            let
                condNode : Result (List DeadEnd) ( Env, TypedNode TypedExpression )
                condNode =
                    fromNodeExpression env_ cond

                condEnv : Result (List DeadEnd) Env
                condEnv =
                    condNode |> Result.map (\( env__, _ ) -> env__)

                thenNode : Result (List DeadEnd) ( Env, TypedNode TypedExpression )
                thenNode =
                    condEnv |> Result.andThen (\env__ -> fromNodeExpression env__ then_)

                thenEnv : Result (List DeadEnd) Env
                thenEnv =
                    thenNode |> Result.map (\( env__, _ ) -> env__)

                elseNode : Result (List DeadEnd) ( Env, TypedNode TypedExpression )
                elseNode =
                    thenEnv |> Result.andThen (\env__ -> fromNodeExpression env__ else_)

                elseEnv : Result (List DeadEnd) Env
                elseEnv =
                    elseNode |> Result.map (\( env__, _ ) -> env__)

                lastEnv : Result (List DeadEnd) Env
                lastEnv =
                    elseEnv |> Result.map countLabel
            in
            case ( condNode, thenNode, elseNode ) of
                ( Ok ( _, TypedNode cm cond_ ), Ok ( _, TypedNode tm then__ ), Ok ( _, TypedNode em else__ ) ) ->
                    if cm.type_ == Bool && tm.type_ == em.type_ then
                        lastEnv
                            |> Result.map
                                (\lastEnv_ ->
                                    ( lastEnv_
                                    , TypedNode { range = range_, type_ = tm.type_, env = lastEnv_ } (TypedIfBlock (TypedNode cm cond_) (TypedNode tm then__) (TypedNode em else__))
                                    )
                                )

                    else
                        Err
                            [ DeadEnd row column (Problem "Type: If block condition must be a boolean and then and else must be of the same type") ]

                ( Err err, _, _ ) ->
                    Err err

                ( _, Err err, _ ) ->
                    Err err

                ( _, _, Err err ) ->
                    Err err

        Integer int ->
            let
                lastEnv : Env
                lastEnv =
                    env_ |> countLabel
            in
            Ok ( lastEnv, TypedNode { range = range_, type_ = Int, env = lastEnv } (TypedInteger int) )

        Negation node ->
            let
                typedExpression : Result (List DeadEnd) ( Env, TypedNode TypedExpression )
                typedExpression =
                    fromNodeExpression env_ node

                exprEnv : Result (List DeadEnd) Env
                exprEnv =
                    typedExpression |> Result.map (\( env__, _ ) -> env__)

                lastEnv : Result (List DeadEnd) Env
                lastEnv =
                    exprEnv |> Result.map countLabel
            in
            case ( lastEnv, typedExpression ) of
                ( Ok lastEnv_, Ok ( _, TypedNode nm node_ ) ) ->
                    if nm.type_ == Int then
                        Ok
                            ( lastEnv_
                            , TypedNode { range = range_, type_ = Int, env = lastEnv_ } (TypedNegation (TypedNode nm node_))
                            )

                    else
                        Err [ DeadEnd row column (Problem "Type: Negation must be applied to an integer") ]

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

        ParenthesizedExpression node ->
            let
                typedExpression : Result (List DeadEnd) ( Env, TypedNode TypedExpression )
                typedExpression =
                    fromNodeExpression env_ node

                exprEnv : Result (List DeadEnd) Env
                exprEnv =
                    typedExpression |> Result.map (\( env__, _ ) -> env__)

                lastEnv : Result (List DeadEnd) Env
                lastEnv =
                    exprEnv |> Result.map countLabel
            in
            case ( lastEnv, typedExpression ) of
                ( Ok lastEnv_, Ok ( _, TypedNode nm node_ ) ) ->
                    Ok
                        ( lastEnv_
                        , TypedNode { range = range_, type_ = nm.type_, env = lastEnv_ } (TypedParenthesizedExpression (TypedNode nm node_))
                        )

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

        LetExpression letblock ->
            let
                typedLetBlock : Result (List DeadEnd) ( Env, TypedLetBlock )
                typedLetBlock =
                    fromLetBlock env_ letblock

                letBlockEnv : Result (List DeadEnd) Env
                letBlockEnv =
                    typedLetBlock |> Result.map (\( env__, _ ) -> env__)

                lastEnv : Result (List DeadEnd) Env
                lastEnv =
                    letBlockEnv |> Result.map (countLabel >> resetRequiredVariables)
            in
            case ( lastEnv, typedLetBlock ) of
                ( Ok lastEnv_, Ok ( _, typedLetBlock_ ) ) ->
                    Ok
                        ( lastEnv_
                        , TypedNode
                            { range = range_, type_ = typedLetBlock_.expression |> type_, env = lastEnv_ }
                            (TypedLetExpression typedLetBlock_)
                        )

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

        _ ->
            Err [ DeadEnd row column (Problem "Type: Unsupported expression") ]


fromFunction : Env -> Function -> Result (List DeadEnd) ( Env, TypedFunction )
fromFunction env_ func =
    let
        typedFunction : Result (List DeadEnd) ( Env, TypedNode TypedFunctionImplementation )
        typedFunction =
            fromNodeFunctionImplementation env_ func.declaration
    in
    typedFunction
        |> Result.map (\( lastEnv, decl ) -> ( lastEnv, { declaration = decl } ))


fromNodeFunctionImplementation : Env -> Node FunctionImplementation -> Result (List DeadEnd) ( Env, TypedNode TypedFunctionImplementation )
fromNodeFunctionImplementation env_ (Node range_ node) =
    let
        nameNode : Result (List DeadEnd) ( Env, TypedNode String )
        nameNode =
            fromNodeString env_ node.name

        nameEnv : Result (List DeadEnd) Env
        nameEnv =
            nameNode |> Result.map (\( env__, _ ) -> env__)

        exprNode : Result (List DeadEnd) ( Env, TypedNode TypedExpression )
        exprNode =
            nameEnv |> Result.andThen (\nameEnv_ -> fromNodeExpression nameEnv_ node.expression)

        lastEnv : Result (List DeadEnd) Env
        lastEnv =
            exprNode |> Result.map (\( env__, _ ) -> env__ |> countLabel)
    in
    Result.map3
        (\( _, name_ ) env__ ( _, expr_ ) ->
            ( env__
            , TypedNode
                { range = range_, type_ = type_ expr_, env = env__ }
                (TypedFunctionImplementation name_ expr_)
            )
        )
        nameNode
        lastEnv
        exprNode


fromNodeString : Env -> Node String -> Result (List DeadEnd) ( Env, TypedNode String )
fromNodeString env_ (Node range_ str) =
    let
        lastEnv : Env
        lastEnv =
            env_ |> countLabel
    in
    Ok ( lastEnv, TypedNode { range = range_, type_ = Int, env = lastEnv } str )
