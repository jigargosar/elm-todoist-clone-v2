module TodoFormV2 exposing
    ( Config
    , TodoForm
    , createConfig
    , getPatches
    , viewTodoForm
    )

import Date exposing (Date)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Maybe.Extra as MX
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Random
import Todo exposing (Todo)
import UI exposing (btn2, col, ipt3, row, submit)


type TodoForm
    = TodoForm Fields Fields


type alias Fields =
    { title : String
    , maybeProjectId : Maybe ProjectId
    , maybeDueDate : Maybe Date
    , projectSortIdx : Int
    }


getPatches : TodoForm -> List Todo.Patch
getPatches (TodoForm _ current) =
    toPatches current


toPatches : Fields -> List Todo.Patch
toPatches m =
    [ Todo.Title m.title
    , Todo.Project m.maybeProjectId
    , Todo.DueDate m.maybeDueDate
    , Todo.ProjectSortIdx m.projectSortIdx
    ]


unwrap : TodoForm -> Fields
unwrap (TodoForm _ internal) =
    internal


empty : Fields
empty =
    Fields "" Nothing Nothing Random.maxInt


init : Fields -> TodoForm
init internal =
    TodoForm internal internal


type Config msg
    = Config { onSave : msg, onCancel : msg, toMsg : TodoForm -> msg }


createConfig : { onSave : msg, onCancel : msg, toMsg : TodoForm -> msg } -> Config msg
createConfig =
    Config


viewTodoForm : Config msg -> List Project -> TodoForm -> H.Html msg
viewTodoForm (Config { onSave, onCancel, toMsg }) projectList (TodoForm initial model) =
    let
        onChange =
            toMsg << TodoForm initial

        titleChanged title =
            onChange { model | title = title }

        projectChanged maybeProjectId =
            onChange { model | maybeProjectId = maybeProjectId }

        dueDateChanged maybeDueDate =
            onChange { model | maybeDueDate = maybeDueDate }
    in
    H.form [ A.class "flex flex-column pa1", E.onSubmit onSave ]
        [ col [ A.class "pv1" ]
            [ ipt3 model.title titleChanged [ A.autofocus True ]
            ]
        , Project.viewSelectOne model.maybeProjectId projectChanged projectList
        , viewDueDateInput model.maybeDueDate dueDateChanged
        , row [ A.class "pv1" ] [ submit "Save" [], btn2 "Cancel" onCancel ]
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
