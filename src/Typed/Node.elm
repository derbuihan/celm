module Typed.Node exposing (Env, Meta, Type(..), TypedNode(..), addRequiredVariable, countLabel, env, initEnv, meta, range, resetRequiredVariables, type_, value)

import Dict
import Elm.Syntax.Range exposing (Range)


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
