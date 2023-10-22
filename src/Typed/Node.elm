module Typed.Node exposing (Env, Meta, Type(..), TypedNode(..), countEnv, env, meta, range, type_, value)

import Elm.Syntax.Range exposing (Range)


type Type
    = Int
    | Bool
    | Unit


type alias Env =
    { label : Int
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


countEnv : Env -> Env
countEnv env_ =
    { env_ | label = env_.label + 1 }
