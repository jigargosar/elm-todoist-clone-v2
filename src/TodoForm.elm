module TodoForm exposing (Config, Partial, TodoForm, createConfig, fromPartial, getProjectSortIdx, initBy, setProjectSortIdx, toPartial, viewTodoForm)

import Date exposing (Date)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Maybe.Extra as MX
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Random
import UI exposing (btn2, col, ipt2, row)


type TodoForm
    = TodoForm Internal


type alias Internal =
    Partial {}


type alias InternalConstructor =
    { title : String
    , maybeProjectId : Maybe ProjectId
    , maybeDueDate : Maybe Date
    , projectSortIdx : Int
    }


type alias Partial a =
    { a
        | title : String
        , maybeProjectId : Maybe ProjectId
        , maybeDueDate : Maybe Date
        , projectSortIdx : Int
    }


toPartial : TodoForm -> Partial {}
toPartial (TodoForm internal) =
    internal


getProjectSortIdx : TodoForm -> Int
getProjectSortIdx =
    unwrap >> .projectSortIdx


unwrap (TodoForm internal) =
    internal


setProjectSortIdx : Int -> TodoForm -> TodoForm
setProjectSortIdx projectSortIdx =
    map (\f -> { f | projectSortIdx = projectSortIdx })


map : (Internal -> Internal) -> TodoForm -> TodoForm
map func (TodoForm internal) =
    TodoForm <| func internal


empty : Internal
empty =
    InternalConstructor "" Nothing Nothing Random.maxInt


initBy : (Internal -> Internal) -> TodoForm
initBy func =
    TodoForm <| func empty


fromPartial : Partial a -> TodoForm
fromPartial { title, maybeProjectId, maybeDueDate, projectSortIdx } =
    TodoForm <| InternalConstructor title maybeProjectId maybeDueDate projectSortIdx


type Config msg
    = Config { onSave : msg, onCancel : msg, toMsg : TodoForm -> msg }


createConfig : { onSave : msg, onCancel : msg, toMsg : TodoForm -> msg } -> Config msg
createConfig =
    Config


viewTodoForm : Config msg -> TodoForm -> H.Html msg
viewTodoForm (Config { onSave, onCancel, toMsg }) (TodoForm model) =
    let
        onChange =
            toMsg << TodoForm

        titleChanged title =
            onChange { model | title = title }

        projectChanged maybeProjectId =
            onChange { model | maybeProjectId = maybeProjectId }

        dueDateChanged maybeDueDate =
            onChange { model | maybeDueDate = maybeDueDate }
    in
    col [ A.class "pa1" ]
        [ col [ A.class "pv1" ]
            [ ipt2 model.title titleChanged
            ]
        , Project.viewSelectOne model.maybeProjectId projectChanged
        , viewDueDateInput model.maybeDueDate dueDateChanged
        , row [ A.class "pv1" ] [ btn2 "Save" onSave, btn2 "Cancel" onCancel ]
        ]


viewDueDateInput maybeDueDate dueDateChanged =
    let
        dateVal =
            maybeDueDate
                |> MX.unwrap "" Date.toIsoString
    in
    H.input
        [ A.type_ "date"
        , A.value dateVal
        , E.onInput (Date.fromIsoString >> Result.toMaybe >> dueDateChanged)
        ]
        []
