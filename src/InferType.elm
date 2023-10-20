module InferType exposing (inferType)

import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing (DeadEnd, Problem(..))
import Typed.Declaration exposing (TypedDeclaration(..), fromNodeDeclaration)
import Typed.File exposing (TypedFile)
import Typed.Node exposing (Type(..), TypedNode(..))


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
