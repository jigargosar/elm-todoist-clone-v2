module Explorer exposing (main)

import Browser
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onCheck)
import Svg.Styled exposing (g, line, rect, svg)
import Svg.Styled.Attributes as SA exposing (x1, x2)
import UI


type alias Model =
    { isChecked : Bool }


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
            ( { model | isChecked = bool }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "pa3 flex items-center bg-washed-red ba bw1 ma3", style "min-height" "100vh" ]
        [ div [ class "shadow-1" ] [ viewTodoItem model ] ]


row =
    UI.row


col =
    UI.col


viewTodoItem model =
    row [ class "items-center" ]
        [ viewCheckbox model.isChecked onCheck
        , div [ class "pa1" ] [ text "Todo item title" ]
        ]


viewCheckbox isChecked onCheck =
    col
        [ class "relative flex f2"
        , style "width" "1em"
        , style "height" "1em"
        ]
        [ svg
            [ SA.class "absolute"
            , SA.width "100%"
            , SA.height "100%"
            , SA.viewBox "0 0 24 24"
            , SA.fill "none"
            , SA.strokeWidth "1"
            , SA.stroke "black"
            , SA.strokeLinecap "round"
            , SA.strokeLinejoin "round"
            ]
            [ rect
                [ SA.width "12", SA.height "12", SA.transform "translate(6,6)" ]
                []
            , g []
                (if isChecked then
                    [ line [ x1 "0", x2 "8", SA.transform "translate(11,15), rotate(-50)" ] []
                    , line [ x1 "0", x2 "4", SA.transform "translate(11,15), rotate(-140)" ] []
                    ]

                 else
                    []
                )
            ]
        , input
            [ class "ma0 pa0 o-0"
            , type_ "checkbox"
            , style "width" "100%"
            , style "height" "100%"

            --, style "visibility" "hidden"
            , onCheck OnCheck
            ]
            []
        ]


main : Program {} Model Msg
main =
    Browser.element
        { init = \_ -> ( { isChecked = False }, Cmd.none )
        , view = toUnstyled << view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
