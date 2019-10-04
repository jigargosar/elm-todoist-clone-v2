module Explorer exposing (main)

import Browser
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)


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
    div [ class "pa3 flex items-center bg-washed-red ba bw1 ma3", style "min-height" "100vh" ]
        [ div [] [ text "Hello" ] ]


main : Program {} Model Msg
main =
    Browser.element
        { init = \_ -> ( Model, Cmd.none )
        , view = toUnstyled << view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
