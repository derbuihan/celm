module TypedAST exposing (..)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Parser exposing (DeadEnd, Problem(..))


type Type
    = Int
    | Bool
    | Unit


type alias Meta =
    { range : Range
    , type_ : Type
    }


type TypedNode a
    = TypedNode Meta a


range : TypedNode a -> Range
range (TypedNode m _) =
    m.range


type_ : TypedNode a -> Type
type_ (TypedNode m _) =
    m.type_


value : TypedNode a -> a
value (TypedNode _ v) =
    v


meta : TypedNode a -> Meta
meta (TypedNode m _) =
    m


type alias TypedFile =
    { declarations : List (TypedNode TypedDeclaration)
    }


type TypedDeclaration
    = TypedDestructuring (TypedNode TypedPattern) (TypedNode TypedExpression)
    | TypedFunctionDeclaration TypedFunction


type TypedPattern
    = TypedIntPattern Int


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
fromFunctionImplementation range_ funcimpl =
    let
        typedExpression =
            fromNodeExpression funcimpl.expression
    in
    Result.map (\expr_ -> TypedFunctionImplementation expr_) typedExpression


fromNodeString : Node String -> Result (List DeadEnd) (TypedNode String)
fromNodeString (Node range_ node) =
    Ok (TypedNode { range = range_, type_ = Int } node)


fromNodeDeclaration : Node Declaration -> Result (List DeadEnd) (TypedNode TypedDeclaration)
fromNodeDeclaration (Node range_ node) =
    let
        typedDeclaration =
            fromDeclaration range_ node
    in
    Result.map (\d -> TypedNode { range = range_, type_ = Unit } d) typedDeclaration


getMetaNodePattern : Node Pattern -> Result (List DeadEnd) Meta
getMetaNodePattern (Node r p) =
    case p of
        IntPattern _ ->
            Ok { range = r, type_ = Int }

        _ ->
            Err [ DeadEnd r.start.row r.start.column (Problem "Unsupported pattern") ]


fromPattern : Range -> Pattern -> Result (List DeadEnd) TypedPattern
fromPattern range_ pattern =
    case pattern of
        IntPattern int ->
            Ok (TypedIntPattern int)

        _ ->
            Err [ DeadEnd range_.start.row range_.start.column (Problem "Unsupported pattern") ]


fromNodePattern : Node Pattern -> Result (List DeadEnd) (TypedNode TypedPattern)
fromNodePattern (Node range_ node) =
    let
        meta_ : Result (List DeadEnd) Meta
        meta_ =
            getMetaNodePattern (Node range_ node)

        typedPattern : Result (List DeadEnd) TypedPattern
        typedPattern =
            fromPattern range_ node
    in
    Result.map2 (\m p -> TypedNode m p) meta_ typedPattern


inferType : File -> Result (List DeadEnd) TypedFile
inferType file =
    case file.declarations of
        [] ->
            Err [ DeadEnd 0 0 (Problem "Empty file") ]

        decl :: _ ->
            let
                typedDeclaration =
                    fromNodeDeclaration decl
            in
            Result.map (\d -> { declarations = [ d ] }) typedDeclaration
