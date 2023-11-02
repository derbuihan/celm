module Typed.Node exposing (Env, Meta, Type(..), TypedNode(..), addRequiredVariable, countLabel, env, getLastEnv, inferNodes, initEnv, insertOffset, meta, range, resetOffsets, resetRequiredVariables, type_, value)

import Dict
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Parser exposing (DeadEnd)


type Type
    = Int
    | Bool
    | Unit


type alias Env =
    { label : Int
    , required_variables : Dict.Dict String Type
    , offsets : Dict.Dict String Int
    }


type alias Meta =
    { range : Range
    , type_ : Type
    , env : Env
    }


type TypedNode a
    = TypedNode Meta a


range : TypedNode a -> Range
range (TypedNode m _) =
    m.range


type_ : TypedNode a -> Type
type_ (TypedNode m _) =
    m.type_


env : TypedNode a -> Env
env (TypedNode m _) =
    m.env


value : TypedNode a -> a
value (TypedNode _ v) =
    v


meta : TypedNode a -> Meta
meta (TypedNode m _) =
    m


initEnv : Env
initEnv =
    { label = 0
    , required_variables = Dict.empty
    , offsets = Dict.fromList []
    }


countLabel : Env -> Env
countLabel env_ =
    { env_ | label = env_.label + 1 }


addRequiredVariable : String -> Type -> Env -> Env
addRequiredVariable name t env_ =
    { env_ | required_variables = Dict.insert name t env_.required_variables }


resetRequiredVariables : Env -> Env
resetRequiredVariables env_ =
    { env_ | required_variables = Dict.empty }


insertOffset : String -> Env -> Env
insertOffset name env_ =
    let
        offset : Int
        offset =
            (1 + Dict.size env_.offsets) * 16
    in
    { env_ | offsets = Dict.insert name offset env_.offsets }


resetOffsets : Env -> Env
resetOffsets env_ =
    { env_ | offsets = Dict.fromList [] }


inferNodes : (Env -> Node a -> Result (List DeadEnd) (TypedNode b)) -> (Env -> Env) -> Env -> List (Node a) -> Result (List DeadEnd) (List (TypedNode b))
inferNodes fromNodeFunc updateEnv env_ nodes =
    case nodes of
        [] ->
            Ok []

        node :: nodes_ ->
            let
                typedNode : Result (List DeadEnd) (TypedNode b)
                typedNode =
                    fromNodeFunc env_ node
            in
            case typedNode of
                Ok typedNode_ ->
                    Result.map (\typedNodes_ -> typedNode_ :: typedNodes_)
                        (inferNodes fromNodeFunc updateEnv (typedNode_ |> env |> updateEnv) nodes_)

                Err deadEnds ->
                    Err deadEnds


getLastEnv : List (TypedNode a) -> Maybe Env
getLastEnv nodes =
    nodes
        |> List.reverse
        |> List.head
        |> Maybe.map (\node -> node |> env)
