port module Main exposing (main)

import Platform exposing (Program)


type alias InputType = Int
type alias OutputType = Int

port get : (InputType -> msg) -> Sub msg

port put : OutputType -> Cmd msg


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
    = Input Int


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

transform : InputType -> OutputType
transform k =
    if modBy 2 k == 0 then
        k // 2
    else
        3*k+ 1

