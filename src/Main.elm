port module Main exposing (main)

import Generate exposing (generate)
import Parse exposing (parse)
import Platform exposing (Program)


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg


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
            ( model, put (compile input) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    get Input


compile : String -> String
compile p =
    let
        ast =
            parse p
    in
    case ast of
        Ok x ->
            generate x

        Err x ->
            Debug.toString x ++ "\n" ++ Debug.toString ast
