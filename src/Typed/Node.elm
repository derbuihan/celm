module Typed.Node exposing (Meta, Type(..), TypedNode(..), meta, range, type_, value)

import Elm.Syntax.Range exposing (Range)


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
