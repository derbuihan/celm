port module Main exposing (Flags, Model, Msg, main)

import Elm.Syntax.File exposing (File)
import Generate exposing (generate)
import InferType exposing (inferType)
import Parse exposing (parse)
import Parser exposing (DeadEnd)
import Platform exposing (Program)
import Result
import Typed.File exposing (TypedFile)


port get : (String -> msg) -> Sub msg


port putAST : String -> Cmd msg


port putTypedAST : String -> Cmd msg


port putCode : String -> Cmd msg


port debug : String -> Cmd msg


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    ()


type Msg
    = Input String


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            let
                ast : Result (List DeadEnd) File
                ast =
                    parse input

                typedast : Result (List DeadEnd) TypedFile
                typedast =
                    ast |> Result.andThen inferType

                code : Result (List DeadEnd) String
                code =
                    typedast |> Result.andThen generate
            in
            case ( ast, typedast, code ) of
                ( Ok ast_, Ok typedast_, Ok code_ ) ->
                    ( model
                    , Cmd.batch
                        [ putAST (ast_ |> Debug.toString)
                        , putTypedAST (typedast_ |> Debug.toString)
                        , putCode code_
                        ]
                    )

                ( Ok ast_, Ok typedast_, Err err_ ) ->
                    ( model
                    , Cmd.batch
                        [ putAST (ast_ |> Debug.toString)
                        , putTypedAST (typedast_ |> Debug.toString)
                        , debug (err_ |> Debug.toString)
                        ]
                    )

                ( Ok ast_, Err err_, _ ) ->
                    ( model
                    , Cmd.batch
                        [ putAST (ast_ |> Debug.toString)
                        , debug (err_ |> Debug.toString)
                        ]
                    )

                ( Err err_, _, _ ) ->
                    ( model, debug (err_ |> Debug.toString) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    get Input
