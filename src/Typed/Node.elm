module Typed.Node exposing (Env, Meta, Type(..), TypedNode(..), addRequiredVariable, countLabel, env, getLastEnv, inferNodes, initEnv, meta, range, resetRequiredVariables, type_, value)

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


inferNodes : (Env -> Node a -> Result (List DeadEnd) (TypedNode b)) -> Env -> List (Node a) -> Result (List DeadEnd) (List (TypedNode b))
inferNodes fromNodeFunc env_ nodes =
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
                        (inferNodes fromNodeFunc (typedNode_ |> env |> resetRequiredVariables) nodes_)

                Err deadEnds ->
                    Err deadEnds


getLastEnv : List (TypedNode a) -> Maybe Env
getLastEnv nodes =
    nodes
        |> List.reverse
        |> List.head
        |> Maybe.map (\node -> node |> env)
