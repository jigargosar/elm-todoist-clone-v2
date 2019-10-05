module SchedulePopup exposing (Model, Msg, Schedule, System, system)

import Basics.More exposing (perform)
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


type Msg
    = Open Schedule
    | Save
    | Cancel
    | Changed Schedule
    | NoOp


type alias System msg =
    { model : Model
    , update : Msg -> Model -> ( Model, Cmd msg )
    , view : ((Schedule -> Msg) -> H.Html Msg) -> Model -> H.Html msg
    }


system : { toMsg : Msg -> msg, onSave : Schedule -> msg } -> System msg
system { toMsg, onSave } =
    { model = Model Nothing
    , update = update { saved = onSave >> perform }
    , view = \viewTrigger model -> view viewTrigger model |> H.map toMsg
    }


update : { saved : Schedule -> Cmd msg } -> Msg -> Model -> ( Model, Cmd msg )
update config message (Model model) =
    case message of
        NoOp ->
            ( Model model, Cmd.none )

        Open schedule ->
            ( Just schedule |> Model, Cmd.none )

        Save ->
            ( Model Nothing, model |> MX.unwrap Cmd.none config.saved )

        Cancel ->
            ( Model Nothing, Cmd.none )

        Changed schedule ->
            ( Just schedule |> Model, Cmd.none )


onClickStopPropagation msg =
    E.stopPropagationOn "click" (JD.succeed ( msg, True ))


view : ((Schedule -> Msg) -> H.Html Msg) -> Model -> H.Html Msg
view viewTrigger (Model maybeSchedule) =
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
        , viewTrigger Open
        ]
