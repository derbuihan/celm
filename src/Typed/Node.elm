module Typed.Node exposing (Meta, Type(..), TypedNode(..), VarInfo, addRequiredVariable, getLastEnv, inferNodes, initMeta, insertVariable, meta, range, resetRequiredVariables, resetVariables, type_, value)

import Dict
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Range as Range exposing (Range)
import Parser exposing (DeadEnd)


type Type
    = Int
    | Bool
    | Unit


type alias VarInfo =
    { offset : Int
    , type_ : Type
    }


type alias Meta =
    { range : Range
    , type_ : Type
    , label : Int
    , required_variables : Dict.Dict String Type
    , variables : Dict.Dict String VarInfo
    }


type TypedNode a
    = TypedNode Meta a


range : TypedNode a -> Range
range (TypedNode m _) =
    m.range


type_ : TypedNode a -> Type
type_ (TypedNode m _) =
    m.type_


label : TypedNode a -> Int
label (TypedNode m _) =
    m.label


required_variables : TypedNode a -> Dict.Dict String Type
required_variables (TypedNode m _) =
    m.required_variables


variables : TypedNode a -> Dict.Dict String VarInfo
variables (TypedNode m _) =
    m.variables


value : TypedNode a -> a
value (TypedNode _ v) =
    v


meta : TypedNode a -> Meta
meta (TypedNode m _) =
    m


initMeta : Meta
initMeta =
    { range = Range.empty
    , type_ = Unit
    , label = 0
    , required_variables = Dict.empty
    , variables = Dict.fromList []
    }


addRequiredVariable : String -> Meta -> Meta
addRequiredVariable name meta_ =
    let
        type__ : Type
        type__ =
            meta_.type_
    in
    { meta_ | required_variables = Dict.insert name type__ meta_.required_variables }


resetRequiredVariables : Meta -> Meta
resetRequiredVariables meta_ =
    { meta_ | required_variables = Dict.empty }


insertVariable : String -> Meta -> Meta
insertVariable name meta_ =
    let
        offset : Int
        offset =
            (1 + Dict.size meta_.variables) * 16

        varInfo : VarInfo
        varInfo =
            { offset = offset
            , type_ = meta_.type_
            }
    in
    { meta_ | variables = Dict.insert name varInfo meta_.variables }


resetVariables : Meta -> Meta
resetVariables meta_ =
    { meta_ | variables = Dict.fromList [] }


inferNodes : (Meta -> Node a -> Result (List DeadEnd) (TypedNode b)) -> (Meta -> Meta) -> Meta -> List (Node a) -> Result (List DeadEnd) (List (TypedNode b))
inferNodes fromNodeFunc updateMeta meta_ nodes =
    case nodes of
        [] ->
            Ok []

        node :: nodes_ ->
            let
                typedNode : Result (List DeadEnd) (TypedNode b)
                typedNode =
                    fromNodeFunc meta_ node
            in
            case typedNode of
                Ok typedNode_ ->
                    Result.map (\typedNodes_ -> typedNode_ :: typedNodes_)
                        (inferNodes fromNodeFunc updateMeta (typedNode_ |> meta |> updateMeta) nodes_)

                Err deadEnds ->
                    Err deadEnds


getLastEnv : List (TypedNode a) -> Maybe Meta
getLastEnv nodes =
    nodes
        |> List.reverse
        |> List.head
        |> Maybe.map (\node -> node |> meta)
