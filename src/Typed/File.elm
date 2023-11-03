module Typed.File exposing (TypedFile, fromFile)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import Parser exposing (DeadEnd)
import Typed.Declaration exposing (TypedDeclaration, fromNodeDeclaration)
import Typed.Node exposing (Meta, TypedNode)


type alias TypedFile =
    { declarations : List (TypedNode TypedDeclaration)
    }


inferDeclarations : Meta -> List (Node Declaration) -> Result (List DeadEnd) (List ( Meta, TypedNode TypedDeclaration ))
inferDeclarations meta_ decls =
    case decls of
        [] ->
            Ok []

        decl :: decls_ ->
            let
                typedDeclWithEnv : Result (List DeadEnd) ( Meta, TypedNode TypedDeclaration )
                typedDeclWithEnv =
                    fromNodeDeclaration meta_ decl

                lastEnv : Result (List DeadEnd) Meta
                lastEnv =
                    typedDeclWithEnv
                        |> Result.map
                            (Tuple.first >> (\meta__ -> { meta__ | range = meta_.range, label = meta__.label + 1 }))
            in
            case ( lastEnv, typedDeclWithEnv ) of
                ( Ok lastEnv_, Ok typedDecl_ ) ->
                    inferDeclarations lastEnv_ decls_ |> Result.map (\typedDecls_ -> typedDecl_ :: typedDecls_)

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err


fromFile : Meta -> File -> Result (List DeadEnd) ( Meta, TypedFile )
fromFile meta_ file =
    let
        declarations : List (Node Declaration)
        declarations =
            file.declarations

        typedDeclarations : Result (List DeadEnd) (List ( Meta, TypedNode TypedDeclaration ))
        typedDeclarations =
            inferDeclarations meta_ declarations

        typedDecls : Result (List DeadEnd) (List (TypedNode TypedDeclaration))
        typedDecls =
            typedDeclarations
                |> Result.map (List.map Tuple.second)

        lastEnv : Result (List DeadEnd) Meta
        lastEnv =
            typedDeclarations
                |> Result.map (List.map Tuple.first >> List.reverse >> List.head >> Maybe.withDefault meta_)
    in
    case ( lastEnv, typedDecls ) of
        ( Ok lastEnv_, Ok typedDecls_ ) ->
            Ok ( lastEnv_, { declarations = typedDecls_ } )

        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err
