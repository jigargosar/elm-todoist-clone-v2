module Explorer exposing (main)

import Browser
import Html.Styled exposing (..)


type alias Model =
    {}


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


view _ =
    toUnstyled <|
        text "HH"


main : Program {} Model Msg
main =
    Browser.element
        { init = \_ -> ( Model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
