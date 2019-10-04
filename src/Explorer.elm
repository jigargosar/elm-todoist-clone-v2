module Explorer exposing (main)

import Browser
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onCheck)
import Svg.Styled exposing (g, rect, svg)
import Svg.Styled.Attributes as SA
import UI


type alias Model =
    {}


type Msg
    = NoOp
    | OnCheck Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnCheck bool ->
            let
                _ =
                    Debug.log "onCheck" bool
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view _ =
    div [ class "pa3 flex items-center bg-washed-red ba bw1 ma3", style "min-height" "100vh" ]
        [ div [ class "shadow-1" ] [ viewTodoItem ] ]


row =
    UI.row


col =
    UI.col


viewTodoItem =
    row [ class "items-center" ]
        [ col [ class "relative flex pa1" ]
            [ svg
                [ SA.class "absolute top-0 left-0 w-100 h-100"
                , SA.viewBox "0 0 24 24"
                , SA.fill "none"
                , SA.strokeWidth "1"
                , SA.stroke "black"
                ]
                [ rect
                    [ SA.width "12", SA.height "12", SA.transform "translate(6,6)" ]
                    []
                ]
            , input
                [ class "ma0 pa0 o-0"
                , type_ "checkbox"
                , style "width" "24px"
                , style "height" "24px"

                --, style "visibility" "hidden"
                , onCheck OnCheck
                ]
                []
            ]
        , div [ class "pa1" ] [ text "Todo item title" ]
        ]


main : Program {} Model Msg
main =
    Browser.element
        { init = \_ -> ( Model, Cmd.none )
        , view = toUnstyled << view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
