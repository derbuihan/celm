module Typed.File exposing (TypedFile, fromFile)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import Parser exposing (DeadEnd)
import Typed.Declaration exposing (TypedDeclaration, fromNodeDeclaration)
import Typed.Node exposing (Env, TypedNode, countLabel, env, initEnv)


type alias TypedFile =
    { declarations : List (TypedNode TypedDeclaration)
    }


fromFile : File -> Result (List DeadEnd) TypedFile
fromFile file =
    let
        declarations : List (Node Declaration)
        declarations =
            file.declarations

        inferDeclarations : Env -> List (Node Declaration) -> Result (List DeadEnd) (List (TypedNode TypedDeclaration))
        inferDeclarations env_ decls =
            case decls of
                [] ->
                    Ok []

                decl :: decls_ ->
                    let
                        typedDecl : Result (List DeadEnd) (TypedNode TypedDeclaration)
                        typedDecl =
                            fromNodeDeclaration env_ decl
                    in
                    case typedDecl of
                        Ok typedDecl_ ->
                            Result.map (\typedDecls_ -> typedDecl_ :: typedDecls_)
                                (inferDeclarations (typedDecl_ |> env |> countLabel) decls_)

                        Err deadEnds ->
                            Err deadEnds

        typedDeclarations : Result (List DeadEnd) (List (TypedNode TypedDeclaration))
        typedDeclarations =
            declarations |> inferDeclarations initEnv
    in
    case typedDeclarations of
        Ok decls ->
            Ok
                { declarations = decls
                }

        Err deadEnds ->
            Err deadEnds
