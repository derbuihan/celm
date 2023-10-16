port module Main exposing (main)

import Generate exposing (generate)
import Parse exposing (parse)
import Platform exposing (Program)


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg


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
                    ( model, Cmd.batch [ debug (ast_ |> Debug.toString), put code_ ] )

                ( Ok _, Err err_ ) ->
                    ( model, debug (err_ |> Debug.toString) )

                ( Err err, _ ) ->
                    ( model, debug (err |> Debug.toString) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    get Input
