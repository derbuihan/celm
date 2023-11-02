module Typed.File exposing (TypedFile, fromFile)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import List.Extra exposing (count, last)
import Parser exposing (DeadEnd)
import Typed.Declaration exposing (TypedDeclaration, fromNodeDeclaration)
import Typed.Node exposing (Env, TypedNode, countLabel, env, inferNodes, initEnv, resetRequiredVariables)


type alias TypedFile =
    { declarations : List (TypedNode TypedDeclaration)
    }


inferDeclarations : Env -> List (Node Declaration) -> Result (List DeadEnd) (List ( Env, TypedNode TypedDeclaration ))
inferDeclarations env_ decls =
    case decls of
        [] ->
            Ok []

        decl :: decls_ ->
            let
                typedDeclWithEnv : Result (List DeadEnd) ( Env, TypedNode TypedDeclaration )
                typedDeclWithEnv =
                    fromNodeDeclaration env_ decl

                lastEnv : Result (List DeadEnd) Env
                lastEnv =
                    typedDeclWithEnv |> Result.map (Tuple.first >> countLabel)
            in
            case ( lastEnv, typedDeclWithEnv ) of
                ( Ok lastEnv_, Ok typedDecl_ ) ->
                    inferDeclarations lastEnv_ decls_ |> Result.map (\typedDecls_ -> typedDecl_ :: typedDecls_)

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err


fromFile : Env -> File -> Result (List DeadEnd) ( Env, TypedFile )
fromFile env_ file =
    let
        declarations : List (Node Declaration)
        declarations =
            file.declarations

        typedDeclarations : Result (List DeadEnd) (List ( Env, TypedNode TypedDeclaration ))
        typedDeclarations =
            inferDeclarations env_ declarations

        typedDecls : Result (List DeadEnd) (List (TypedNode TypedDeclaration))
        typedDecls =
            typedDeclarations
                |> Result.map (List.map Tuple.second)

        lastEnv : Result (List DeadEnd) Env
        lastEnv =
            typedDeclarations
                |> Result.map (List.map Tuple.first >> List.reverse >> List.head >> Maybe.withDefault env_)
    in
    case ( lastEnv, typedDecls ) of
        ( Ok lastEnv_, Ok typedDecls_ ) ->
            Ok ( lastEnv_, { declarations = typedDecls_ } )

        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err
