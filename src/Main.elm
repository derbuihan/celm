port module Main exposing (main)

import Elm.Syntax.File exposing (encode)
import Generate exposing (generate)
import Json.Encode
import Parse exposing (parse)
import Platform exposing (Program)


port get : (String -> msg) -> Sub msg


port putAST : String -> Cmd msg


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
                ast =
                    parse input

                code =
                    ast |> Result.map generate
            in
            case ( ast, code ) of
                ( Ok ast_, Ok code_ ) ->
                    ( model, Cmd.batch [ putAST (ast_ |> encode |> Json.Encode.encode 4), putCode code_ ] )

                ( Ok ast_, Err err_ ) ->
                    ( model, Cmd.batch [ putAST (ast_ |> encode |> Json.Encode.encode 4), debug (err_ |> Debug.toString) ] )

                ( Err err_, _ ) ->
                    ( model, debug (err_ |> Debug.toString) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    get Input
