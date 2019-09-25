module Main exposing (main)

import Browser
import Html exposing (text)


type alias Flags =
    {}


type alias Model =
    { title : String }


init _ =
    ( { title = "" }, Cmd.none )


type Msg
    = NoOp


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions _ =
    Sub.none


view _ =
    text "hi"


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
