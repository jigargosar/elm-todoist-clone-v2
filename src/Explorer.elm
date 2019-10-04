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


view : Model -> Html Msg
view _ =
    text "HH"


main : Program {} Model Msg
main =
    Browser.element
        { init = \_ -> ( Model, Cmd.none )
        , view = toUnstyled << view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
