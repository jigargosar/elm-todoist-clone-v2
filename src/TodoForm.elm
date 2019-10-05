module TodoForm exposing
    ( Meta(..)
    , Msg
    , System
    , TodoForm
    , getMeta
    , getProjectSortIdx
    , initAdd
    , initEdit
    , isAddingForInitialDueDate
    , isEditingFor
    , setProjectSortIdxIfAdding
    , system
    , toPatchesWithMeta
    , viewTodoForm
    )

import Basics.More exposing (allPass, propEq)
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
import UI exposing (btn2, col, ipt3, row, submit)


type Meta
    = Add
    | Edit TodoId


type TodoForm
    = TodoForm Meta Fields Fields


type alias Fields =
    { title : String
    , maybeProjectId : Maybe ProjectId
    , maybeDueDate : Maybe Date
    , projectSortIdx : Int
    }


toPatchesWithMeta : TodoForm -> ( Meta, List Todo.Patch )
toPatchesWithMeta (TodoForm meta _ current) =
    ( meta, toPatches current )


toPatches : Fields -> List Todo.Patch
toPatches m =
    [ Todo.Title m.title
    , Todo.Project m.maybeProjectId
    , Todo.DueDate m.maybeDueDate
    , Todo.ProjectSortIdx m.projectSortIdx
    ]


unwrapMeta : TodoForm -> Meta
unwrapMeta (TodoForm meta _ _) =
    meta


isEditingFor : TodoId -> TodoForm -> Bool
isEditingFor todoId =
    unwrapMeta >> (==) (Edit todoId)


getMeta : TodoForm -> Meta
getMeta =
    unwrapMeta


getProjectSortIdx : TodoForm -> Int
getProjectSortIdx =
    unwrap >> .projectSortIdx


isAddingForInitialDueDate : Date -> TodoForm -> Bool
isAddingForInitialDueDate dueDate =
    allPass
        [ unwrapMeta >> (==) Add
        , unwrapInitial >> propEq .maybeDueDate (Just dueDate)
        ]


unwrap : TodoForm -> Fields
unwrap (TodoForm _ _ internal) =
    internal


unwrapInitial : TodoForm -> Fields
unwrapInitial (TodoForm _ initial _) =
    initial


setProjectSortIdxIfAdding : Int -> TodoForm -> Maybe TodoForm
setProjectSortIdxIfAdding projectSortIdx =
    mapIfAdding (\f -> { f | projectSortIdx = projectSortIdx })


mapIfAdding : (Fields -> Fields) -> TodoForm -> Maybe TodoForm
mapIfAdding func (TodoForm meta initial current) =
    case meta of
        Add ->
            Just <| TodoForm meta initial <| func current

        _ ->
            Nothing


empty : Fields
empty =
    Fields "" Nothing Nothing Random.maxInt


initAdd : (Fields -> Fields) -> TodoForm
initAdd func =
    init Add <| func empty


init : Meta -> Fields -> TodoForm
init meta internal =
    TodoForm meta internal internal


initEdit : Todo -> TodoForm
initEdit { id, title, maybeProjectId, maybeDueDate, projectSortIdx } =
    init (Edit id) <| Fields title maybeProjectId maybeDueDate projectSortIdx


type Config msg
    = Config { onSave : msg, onCancel : msg, toMsg : Msg -> msg }


createConfig : { onSave : msg, onCancel : msg, toMsg : Msg -> msg } -> Config msg
createConfig =
    Config


type alias System msg =
    { view : List Project -> TodoForm -> H.Html msg
    , update : Msg -> TodoForm -> ( TodoForm, Cmd msg )
    }


mapCurrent : (Fields -> Fields) -> TodoForm -> TodoForm
mapCurrent func (TodoForm meta initial current) =
    TodoForm meta initial (func current)


type Msg
    = Patch TodoForm
    | TitleChanged String
    | ProjectChanged (Maybe ProjectId)
    | DueDateChanged (Maybe Date)
    | Save
    | Cancel


update : Msg -> TodoForm -> ( TodoForm, Cmd msg )
update message ((TodoForm meta initial current) as model) =
    case message of
        Patch m ->
            ( m, Cmd.none )

        TitleChanged title ->
            ( mapCurrent (\f -> { f | title = title }) model, Cmd.none )

        ProjectChanged maybeProjectId ->
            ( mapCurrent (\f -> { f | maybeProjectId = maybeProjectId }) model, Cmd.none )

        DueDateChanged maybeDueDate ->
            ( mapCurrent (\f -> { f | maybeDueDate = maybeDueDate }) model, Cmd.none )

        Save ->
            ( model, Cmd.none )

        Cancel ->
            ( model, Cmd.none )


system : { onSave : msg, onCancel : msg, toMsg : Msg -> msg } -> System msg
system { onSave, onCancel, toMsg } =
    { view = viewTodoForm <| createConfig { onSave = onSave, onCancel = onCancel, toMsg = toMsg }
    , update = update
    }


viewTodoForm : Config msg -> List Project -> TodoForm -> H.Html msg
viewTodoForm (Config { toMsg }) projectList (TodoForm meta initial model) =
    H.form [ A.class "flex flex-column pa1", E.onSubmit Save ]
        [ col [ A.class "pv1" ]
            [ ipt3 model.title TitleChanged [ A.autofocus True ]
            ]
        , Project.viewSelectOne model.maybeProjectId ProjectChanged projectList
        , viewDueDateInput model.maybeDueDate DueDateChanged
        , row [ A.class "pv1" ] [ submit "Save" [], btn2 "Cancel" Cancel ]
        ]
        |> H.map toMsg


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
