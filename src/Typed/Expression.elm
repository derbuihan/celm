module Typed.Expression exposing (TypedExpression(..), TypedFunction, TypedFunctionImplementation, TypedLetDeclaration(..), fromFunction, fromNodeExpression, fromNodeLetDeclaration)

import Dict exposing (keys)
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation, LetBlock, LetDeclaration(..))
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.Node exposing (Node(..))
import Graph exposing (AcyclicGraph, Graph, checkAcyclic, fromNodeLabelsAndEdgePairs, topologicalSort)
import List.Extra as List exposing (elemIndex)
import Parser exposing (DeadEnd, Problem(..))
import Typed.ModuleName exposing (TypedModuleName)
import Typed.Node exposing (Meta, Type(..), TypedNode(..), VarInfo, addRequiredVariable, insertVariable, resetRequiredVariables, value)


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


inferLetDeclarations : Meta -> List (Node LetDeclaration) -> Result (List DeadEnd) (List ( Meta, TypedNode TypedLetDeclaration ))
inferLetDeclarations meta_ decls =
    case decls of
        [] ->
            Ok []

        decl :: decls_ ->
            case decl of
                Node _ (LetFunction func) ->
                    let
                        { declaration } =
                            func

                        (Node _ { name }) =
                            declaration

                        (Node _ variable_name) =
                            name

                        typedDecl : Result (List DeadEnd) ( Meta, TypedNode TypedLetDeclaration )
                        typedDecl =
                            fromNodeLetDeclaration (meta_ |> insertVariable variable_name |> resetRequiredVariables) decl

                        lastMeta : Result (List DeadEnd) Meta
                        lastMeta =
                            typedDecl |> Result.map Tuple.first
                    in
                    case ( lastMeta, typedDecl ) of
                        ( Ok lastMeta_, Ok ( _, decl_ ) ) ->
                            Result.map (\typedDecls_ -> ( lastMeta_, decl_ ) :: typedDecls_) (inferLetDeclarations lastMeta_ decls_)

                        ( Err err, _ ) ->
                            Err err

                        ( _, Err err ) ->
                            Err err

                Node _ (LetDestructuring _ _) ->
                    Err [ DeadEnd 0 0 (Problem "Type: Destructuring is not supported") ]


sortLetDeclarations : ( List Meta, List (TypedNode TypedLetDeclaration) ) -> Result (List DeadEnd) (List (TypedNode TypedLetDeclaration))
sortLetDeclarations ( metas, declarations ) =
    let
        variables : List String
        variables =
            declarations |> List.map (value >> (\(TypedLetFunction func) -> func.declaration) >> value >> .name >> value)

        dependences : List ( Int, Int )
        dependences =
            metas
                |> List.indexedMap (\i meta_ -> meta_ |> .required_variables |> keys |> List.map (\name -> elemIndex name variables |> Maybe.map (\j -> ( j, i ))))
                |> List.concat
                |> List.filterMap identity

        graph : Graph (TypedNode TypedLetDeclaration) ()
        graph =
            fromNodeLabelsAndEdgePairs declarations dependences

        acyclicGraph : Result (List DeadEnd) (AcyclicGraph (TypedNode TypedLetDeclaration) ())
        acyclicGraph =
            checkAcyclic graph |> Result.mapError (\_ -> [ DeadEnd 0 0 (Problem "Type: Cyclic dependency") ])

        sortedDeclaration : Result (List DeadEnd) (List (TypedNode TypedLetDeclaration))
        sortedDeclaration =
            acyclicGraph |> Result.map topologicalSort |> Result.map (\nodes -> nodes |> List.map .node |> List.map .label)
    in
    sortedDeclaration


fromLetBlock : Meta -> LetBlock -> Result (List DeadEnd) ( Meta, TypedLetBlock )
fromLetBlock meta_ letBlock =
    let
        declarationsWithMeta : Result (List DeadEnd) ( List Meta, List (TypedNode TypedLetDeclaration) )
        declarationsWithMeta =
            inferLetDeclarations meta_ letBlock.declarations |> Result.map List.unzip

        declslastMeta : Result (List DeadEnd) Meta
        declslastMeta =
            declarationsWithMeta |> Result.map (Tuple.first >> List.last >> Maybe.withDefault meta_)

        sortedDeclarations : Result (List DeadEnd) (List (TypedNode TypedLetDeclaration))
        sortedDeclarations =
            declarationsWithMeta |> Result.andThen sortLetDeclarations

        variables : Result (List DeadEnd) (Dict.Dict String VarInfo)
        variables =
            declslastMeta |> Result.map .variables

        declsMeta : Result (List DeadEnd) Meta
        declsMeta =
            Result.map2 (\meta__ variables_ -> { meta__ | variables = variables_ } |> resetRequiredVariables) declslastMeta variables

        expression : Result (List DeadEnd) ( Meta, TypedNode TypedExpression )
        expression =
            declsMeta |> Result.andThen (\meta__ -> fromNodeExpression meta__ letBlock.expression)

        lastMeta : Result (List DeadEnd) Meta
        lastMeta =
            expression |> Result.map Tuple.first
    in
    case ( lastMeta, sortedDeclarations, expression ) of
        ( Ok lastMeta_, Ok decls_, Ok ( _, expr_ ) ) ->
            Ok ( lastMeta_, { declarations = decls_, expression = expr_ } )

        ( Err err, _, _ ) ->
            Err err

        ( _, Err err, _ ) ->
            Err err

        ( _, _, Err err ) ->
            Err err


fromNodeLetDeclaration : Meta -> Node LetDeclaration -> Result (List DeadEnd) ( Meta, TypedNode TypedLetDeclaration )
fromNodeLetDeclaration meta_ (Node range_ letdecl) =
    let
        { row, column } =
            range_.start
    in
    case letdecl of
        LetFunction func ->
            let
                typedFunction : Result (List DeadEnd) ( Meta, TypedFunction )
                typedFunction =
                    fromFunction meta_ func

                funcMeta : Result (List DeadEnd) Meta
                funcMeta =
                    typedFunction |> Result.map Tuple.first

                lastMeta : Result (List DeadEnd) Meta
                lastMeta =
                    funcMeta |> Result.map (\meta__ -> { meta__ | label = meta__.label + 1, range = range_ })
            in
            case ( lastMeta, typedFunction ) of
                ( Ok lastMeta_, Ok ( _, func_ ) ) ->
                    Ok
                        ( lastMeta_
                        , TypedNode lastMeta_ (TypedLetFunction func_)
                        )

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

        _ ->
            Err [ DeadEnd row column (Problem "Type: Unsupported let declaration") ]


fromNodeExpression : Meta -> Node Expression -> Result (List DeadEnd) ( Meta, TypedNode TypedExpression )
fromNodeExpression meta_ (Node range_ expr) =
    let
        { row, column } =
            range_.start
    in
    case expr of
        UnitExpr ->
            let
                lastMeta =
                    { meta_ | label = meta_.label + 1, range = range_, type_ = Unit }
            in
            Ok ( lastMeta, TypedNode lastMeta TypedUnitExpr )

        OperatorApplication op dir left right ->
            let
                leftNode : Result (List DeadEnd) ( Meta, TypedNode TypedExpression )
                leftNode =
                    fromNodeExpression meta_ left

                leftMeta : Result (List DeadEnd) Meta
                leftMeta =
                    leftNode |> Result.map Tuple.first

                rightNode : Result (List DeadEnd) ( Meta, TypedNode TypedExpression )
                rightNode =
                    leftMeta
                        |> Result.andThen (\meta__ -> fromNodeExpression meta__ right)

                rightMeta : Result (List DeadEnd) Meta
                rightMeta =
                    rightNode |> Result.map Tuple.first

                lastMeta : Result (List DeadEnd) Meta
                lastMeta =
                    rightMeta |> Result.map (\meta__ -> { meta__ | label = meta__.label + 1, range = range_ })
            in
            case ( lastMeta, leftNode, rightNode ) of
                ( Ok lastMeta_, Ok ( _, TypedNode lm lhs ), Ok ( _, TypedNode rm rhs ) ) ->
                    case ( lm.type_, rm.type_ ) of
                        ( Int, Int ) ->
                            if List.member op [ "+", "-", "*", "/" ] then
                                Ok
                                    ( lastMeta_
                                    , TypedNode { lastMeta_ | type_ = Int }
                                        (TypedOperatorApplication op dir (TypedNode lm lhs) (TypedNode rm rhs))
                                    )

                            else if List.member op [ "==", "/=", "<", ">", "<=", ">=" ] then
                                Ok
                                    ( lastMeta_
                                    , TypedNode { lastMeta_ | type_ = Bool }
                                        (TypedOperatorApplication op dir (TypedNode lm lhs) (TypedNode rm rhs))
                                    )

                            else
                                Err [ DeadEnd row column (Problem "Type: Unsupported operator application") ]

                        ( Bool, Bool ) ->
                            if List.member op [ "==", "/=" ] then
                                Ok
                                    ( lastMeta_
                                    , TypedNode { lastMeta_ | type_ = Bool }
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
                    lastMeta : Meta
                    lastMeta =
                        { meta_ | label = meta_.label + 1, range = range_, type_ = Bool }
                in
                Ok ( lastMeta, TypedNode lastMeta (TypedFunctionOrValue moduleName name) )

            else
                let
                    lastMeta : Meta
                    lastMeta =
                        { meta_ | label = meta_.label + 1, range = range_ } |> addRequiredVariable name
                in
                Ok ( lastMeta, TypedNode lastMeta (TypedFunctionOrValue moduleName name) )

        IfBlock cond then_ else_ ->
            let
                condNode : Result (List DeadEnd) ( Meta, TypedNode TypedExpression )
                condNode =
                    fromNodeExpression meta_ cond

                condMeta : Result (List DeadEnd) Meta
                condMeta =
                    condNode |> Result.map Tuple.first

                thenNode : Result (List DeadEnd) ( Meta, TypedNode TypedExpression )
                thenNode =
                    condMeta |> Result.andThen (\meta__ -> fromNodeExpression meta__ then_)

                thenMeta : Result (List DeadEnd) Meta
                thenMeta =
                    thenNode |> Result.map Tuple.first

                elseNode : Result (List DeadEnd) ( Meta, TypedNode TypedExpression )
                elseNode =
                    thenMeta |> Result.andThen (\meta__ -> fromNodeExpression meta__ else_)

                elseMeta : Result (List DeadEnd) Meta
                elseMeta =
                    elseNode |> Result.map Tuple.first

                lastMeta : Result (List DeadEnd) Meta
                lastMeta =
                    elseMeta |> Result.map (\meta__ -> { meta__ | label = meta__.label + 1, range = range_ })
            in
            case ( condNode, thenNode, elseNode ) of
                ( Ok ( _, TypedNode cm cond_ ), Ok ( _, TypedNode tm then__ ), Ok ( _, TypedNode em else__ ) ) ->
                    if cm.type_ == Bool && tm.type_ == em.type_ then
                        lastMeta
                            |> Result.map
                                (\lastMeta_ ->
                                    ( lastMeta_
                                    , TypedNode { lastMeta_ | type_ = tm.type_ } (TypedIfBlock (TypedNode cm cond_) (TypedNode tm then__) (TypedNode em else__))
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
                lastMeta : Meta
                lastMeta =
                    { meta_ | range = range_, type_ = Int, label = meta_.label + 1 }
            in
            Ok ( lastMeta, TypedNode lastMeta (TypedInteger int) )

        Negation node ->
            let
                typedExpression : Result (List DeadEnd) ( Meta, TypedNode TypedExpression )
                typedExpression =
                    fromNodeExpression meta_ node

                exprMeta : Result (List DeadEnd) Meta
                exprMeta =
                    typedExpression |> Result.map (\( meta__, _ ) -> meta__)

                lastMeta : Result (List DeadEnd) Meta
                lastMeta =
                    exprMeta |> Result.map (\meta__ -> { meta__ | label = meta__.label + 1, range = range_, type_ = Int })
            in
            case ( lastMeta, typedExpression ) of
                ( Ok lastMeta_, Ok ( _, TypedNode nm node_ ) ) ->
                    if nm.type_ == Int then
                        Ok
                            ( lastMeta_, TypedNode lastMeta_ (TypedNegation (TypedNode nm node_)) )

                    else
                        Err [ DeadEnd row column (Problem "Type: Negation must be applied to an integer") ]

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

        ParenthesizedExpression node ->
            let
                typedExpression : Result (List DeadEnd) ( Meta, TypedNode TypedExpression )
                typedExpression =
                    fromNodeExpression meta_ node

                exprMeta : Result (List DeadEnd) Meta
                exprMeta =
                    typedExpression |> Result.map Tuple.first

                lastMeta : Result (List DeadEnd) Meta
                lastMeta =
                    exprMeta |> Result.map (\meta__ -> { meta__ | label = meta__.label + 1, range = range_ })
            in
            case ( lastMeta, typedExpression ) of
                ( Ok lastMeta_, Ok ( _, TypedNode nm node_ ) ) ->
                    Ok
                        ( lastMeta_
                        , TypedNode lastMeta_ (TypedParenthesizedExpression (TypedNode nm node_))
                        )

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

        LetExpression letblock ->
            let
                typedLetBlock : Result (List DeadEnd) ( Meta, TypedLetBlock )
                typedLetBlock =
                    fromLetBlock meta_ letblock

                letBlockMeta : Result (List DeadEnd) Meta
                letBlockMeta =
                    typedLetBlock |> Result.map (\( meta__, _ ) -> meta__)

                lastMeta : Result (List DeadEnd) Meta
                lastMeta =
                    letBlockMeta |> Result.map (\meta__ -> { meta__ | label = meta__.label + 1, range = range_ } |> resetRequiredVariables)
            in
            case ( lastMeta, typedLetBlock ) of
                ( Ok lastMeta_, Ok ( _, typedLetBlock_ ) ) ->
                    Ok
                        ( lastMeta_, TypedNode lastMeta_ (TypedLetExpression typedLetBlock_) )

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

        _ ->
            Err [ DeadEnd row column (Problem "Type: Unsupported expression") ]


fromFunction : Meta -> Function -> Result (List DeadEnd) ( Meta, TypedFunction )
fromFunction meta_ func =
    let
        typedFunction : Result (List DeadEnd) ( Meta, TypedNode TypedFunctionImplementation )
        typedFunction =
            fromNodeFunctionImplementation meta_ func.declaration
    in
    typedFunction
        |> Result.map (\( lastMeta, decl ) -> ( lastMeta, { declaration = decl } ))


fromNodeFunctionImplementation : Meta -> Node FunctionImplementation -> Result (List DeadEnd) ( Meta, TypedNode TypedFunctionImplementation )
fromNodeFunctionImplementation meta_ (Node range_ node) =
    let
        exprNode : Result (List DeadEnd) ( Meta, TypedNode TypedExpression )
        exprNode =
            fromNodeExpression meta_ node.expression

        exprMeta : Result (List DeadEnd) Meta
        exprMeta =
            exprNode |> Result.map Tuple.first

        nameNode : Result (List DeadEnd) ( Meta, TypedNode String )
        nameNode =
            exprMeta |> Result.andThen (\meta__ -> fromNodeString meta__ node.name)

        nameMeta : Result (List DeadEnd) Meta
        nameMeta =
            nameNode |> Result.map Tuple.first

        lastMeta : Result (List DeadEnd) Meta
        lastMeta =
            nameMeta |> Result.map (\meta__ -> { meta__ | label = meta__.label + 1, range = range_ })
    in
    Result.map3
        (\( _, name_ ) meta__ ( _, expr_ ) ->
            ( meta__, TypedNode meta__ (TypedFunctionImplementation name_ expr_) )
        )
        nameNode
        lastMeta
        exprNode


fromNodeString : Meta -> Node String -> Result (List DeadEnd) ( Meta, TypedNode String )
fromNodeString meta_ (Node range_ str) =
    let
        lastMeta : Meta
        lastMeta =
            { meta_ | label = meta_.label + 1, range = range_ }
    in
    Ok ( lastMeta, TypedNode lastMeta str )
