module TodoForm exposing (Config, TodoForm, createConfig, fromTodo, init, toPartial, viewTodoForm)

import Date exposing (Date)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Maybe.Extra as MX
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Random
import Todo exposing (Todo)
import UI exposing (btn2, col, ipt2, row)


type TodoForm
    = TodoForm { title : String, maybeProjectId : Maybe ProjectId, maybeDueDate : Maybe Date }


toPartial : TodoForm -> { title : String, maybeProjectId : Maybe ProjectId, maybeDueDate : Maybe Date }
toPartial (TodoForm partial) =
    partial


init : String -> Maybe ProjectId -> Maybe Date -> TodoForm
init title maybeProjectId maybeDueDate =
    TodoForm { title = title, maybeProjectId = maybeProjectId, maybeDueDate = maybeDueDate }


fromTodo : Todo -> TodoForm
fromTodo { title, maybeProjectId, maybeDueDate, projectSortIdx } =
    TodoForm { title = title, maybeProjectId = maybeProjectId, maybeDueDate = maybeDueDate }


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
