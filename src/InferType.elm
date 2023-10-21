module InferType exposing (inferType)

import Elm.Syntax.File exposing (File)
import Parser exposing (DeadEnd)
import Typed.File exposing (TypedFile, fromFile)


inferType : File -> Result (List DeadEnd) TypedFile
inferType =
    fromFile
