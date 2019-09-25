module Main exposing (main)

import Browser
import Html exposing (text)


type alias Model =
    {}


init _ =
    ( {}, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions _ =
    Sub.none


view _ =
    text "hi"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
