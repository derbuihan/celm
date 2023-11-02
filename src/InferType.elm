module InferType exposing (inferType)

import Elm.Syntax.File exposing (File)
import Parser exposing (DeadEnd)
import Typed.File exposing (TypedFile, fromFile)
import Typed.Node exposing (initEnv)


inferType : File -> Result (List DeadEnd) TypedFile
inferType file =
    case fromFile initEnv file of
        Ok typedFileWithFile ->
            let
                ( _, typedFile ) =
                    typedFileWithFile
            in
            Ok typedFile

        Err err ->
            Err err
