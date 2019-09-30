module TodoForm exposing
    ( Config
    , Meta(..)
    , Partial
    , TodoForm
    , createConfig
    , getMeta
    , getProjectSortIdx
    , initAdd
    , initEdit
    , isAddingForInitialDueDate
    , isEditingTodoId
    , setProjectSortIdx
    , toPartialWithMeta
    , viewTodoForm
    )

import Basics.More exposing (propEq)
import Date exposing (Date)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Maybe.Extra as MX
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Random
import Todo exposing (Todo)
import TodoId exposing (TodoId)
import UI exposing (btn2, col, ipt2, row)


type Meta
    = Add
    | Edit TodoId


type TodoForm
    = TodoForm Meta Internal Internal


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


toPartialWithMeta : TodoForm -> ( Meta, Partial {} )
toPartialWithMeta (TodoForm meta _ internal) =
    ( meta, internal )


unwrapMeta : TodoForm -> Meta
unwrapMeta (TodoForm meta _ _) =
    meta


isEditingTodoId : TodoId -> TodoForm -> Bool
isEditingTodoId todoId =
    unwrapMeta >> (==) (Edit todoId)


getMeta : TodoForm -> Meta
getMeta =
    unwrapMeta


getProjectSortIdx : TodoForm -> Int
getProjectSortIdx =
    unwrap >> .projectSortIdx


isAddingForInitialDueDate : Date -> TodoForm -> Bool
isAddingForInitialDueDate dueDate =
    unwrapInitial >> propEq .maybeDueDate (Just dueDate)


unwrap : TodoForm -> Internal
unwrap (TodoForm _ _ internal) =
    internal


unwrapInitial : TodoForm -> Internal
unwrapInitial (TodoForm _ initial _) =
    initial


setProjectSortIdx : Int -> TodoForm -> TodoForm
setProjectSortIdx projectSortIdx =
    map (\f -> { f | projectSortIdx = projectSortIdx })


map : (Internal -> Internal) -> TodoForm -> TodoForm
map func (TodoForm meta initial internal) =
    TodoForm meta initial <| func internal


empty : Internal
empty =
    InternalConstructor "" Nothing Nothing Random.maxInt


initAdd : (Internal -> Internal) -> TodoForm
initAdd func =
    init Add <| func empty


init : Meta -> Internal -> TodoForm
init meta internal =
    TodoForm meta internal internal


initEdit : Todo -> TodoForm
initEdit { id, title, maybeProjectId, maybeDueDate, projectSortIdx } =
    init (Edit id) <| InternalConstructor title maybeProjectId maybeDueDate projectSortIdx


type Config msg
    = Config { onSave : msg, onCancel : msg, toMsg : TodoForm -> msg }


createConfig : { onSave : msg, onCancel : msg, toMsg : TodoForm -> msg } -> Config msg
createConfig =
    Config


viewTodoForm : Config msg -> TodoForm -> H.Html msg
viewTodoForm (Config { onSave, onCancel, toMsg }) (TodoForm meta initial model) =
    let
        onChange =
            toMsg << TodoForm meta initial

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
