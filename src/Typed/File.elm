module Typed.File exposing (TypedFile, fromFile)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import Parser exposing (DeadEnd)
import Typed.Declaration exposing (TypedDeclaration, fromNodeDeclaration)
import Typed.Node exposing (Env, TypedNode, inferNodes, initEnv)


type alias TypedFile =
    { declarations : List (TypedNode TypedDeclaration)
    }


fromFile : File -> Result (List DeadEnd) TypedFile
fromFile file =
    let
        declarations : List (Node Declaration)
        declarations =
            file.declarations

        typedDeclarations : Result (List DeadEnd) (List (TypedNode TypedDeclaration))
        typedDeclarations =
            inferNodes fromNodeDeclaration initEnv declarations
    in
    case typedDeclarations of
        Ok decls ->
            Ok
                { declarations = decls
                }

        Err deadEnds ->
            Err deadEnds
