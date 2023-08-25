port module Main exposing (main)

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
        Input input -> ( model, put (transform input))


subscriptions : Model -> Sub Msg
subscriptions _ =
    get Input

transform : String -> String
transform s = """
    .text
    .globl _main
    .p2align 2
_main:
""" ++ "    mov x0, " ++ s ++ """
    ret
"""
