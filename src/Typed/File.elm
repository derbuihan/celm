module Typed.File exposing (TypedFile, fromFile)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import Parser exposing (DeadEnd)
import Typed.Declaration exposing (TypedDeclaration, fromNodeDeclaration)
import Typed.Node exposing (TypedNode)


type alias TypedFile =
    { declarations : List (TypedNode TypedDeclaration)
    }


fromFile : File -> Result (List DeadEnd) TypedFile
fromFile file =
    let
        declarations : List (Node Declaration)
        declarations =
            file.declarations

        declarationList : List (Result (List DeadEnd) (TypedNode TypedDeclaration))
        declarationList =
            declarations |> List.map fromNodeDeclaration

        typedDeclarations : List (TypedNode TypedDeclaration)
        typedDeclarations =
            declarationList
                |> List.filterMap
                    (\x ->
                        case x of
                            Ok declaration ->
                                Just declaration

                            Err _ ->
                                Nothing
                    )

        deadEndList : List DeadEnd
        deadEndList =
            declarationList
                |> List.filterMap
                    (\x ->
                        case x of
                            Ok _ ->
                                Nothing

                            Err err ->
                                Just err
                    )
                |> List.concat
    in
    case deadEndList of
        [] ->
            Ok
                { declarations = typedDeclarations
                }

        _ ->
            Err deadEndList
