module Typed.File exposing (..)

import Typed.Declaration exposing (TypedDeclaration)
import Typed.Node exposing (TypedNode)


type alias TypedFile =
    { declarations : List (TypedNode TypedDeclaration)
    }
