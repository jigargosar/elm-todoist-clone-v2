module SchedulePopup exposing (Model, Msg, Schedule, init, open, update, view)

import Date exposing (Date)
import Html.Styled as H
import Html.Styled.Attributes as A exposing (style)
import Html.Styled.Events as E
import Json.Decode as JD
import Maybe.Extra as MX
import Task
import UI exposing (btn2, col)


type alias Schedule =
    Maybe Date


type Model
    = Model (Maybe Schedule)


init : Model
init =
    Model Nothing


open : Schedule -> Msg
open =
    Open


type Msg
    = Open Schedule
    | Save
    | Cancel
    | Changed Schedule
    | NoOp


update : { a | saved : Schedule -> msg } -> Msg -> Model -> ( Model, Cmd msg )
update config message (Model model) =
    case message of
        NoOp ->
            ( Model model, Cmd.none )

        Open schedule ->
            ( Just schedule |> Model, Cmd.none )

        Save ->
            ( Model Nothing, model |> MX.unwrap Cmd.none (config.saved >> perform) )

        Cancel ->
            ( Model Nothing, Cmd.none )

        Changed schedule ->
            ( Just schedule |> Model, Cmd.none )


perform =
    Task.succeed >> Task.perform identity


onClickStopPropagation msg =
    E.stopPropagationOn "click" (JD.succeed ( msg, True ))


view : (Msg -> msg) -> ((Schedule -> Msg) -> H.Html Msg) -> Model -> H.Html msg
view toMsg viewTrigger (Model maybeSchedule) =
    col [ A.class " relative" ]
        [ maybeSchedule
            |> MX.unwrap (H.text "")
                (\schedule ->
                    col
                        [ A.class "absolute right-1 bg-white shadow-1 z-999"
                        , style "min-width" "150px"
                        ]
                        [ col [ onClickStopPropagation NoOp ]
                            [ H.text "Select Due Date"
                            , let
                                dateVal =
                                    schedule
                                        |> MX.unwrap "" Date.toIsoString
                              in
                              H.input
                                [ A.type_ "date"
                                , A.value dateVal
                                , E.onInput (Date.fromIsoString >> Result.toMaybe >> Changed)
                                ]
                                []
                            , btn2 "Save" Save
                            ]
                        ]
                )
            |> H.map toMsg
        , viewTrigger Open
            |> H.map toMsg
        ]
