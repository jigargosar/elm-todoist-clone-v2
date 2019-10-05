module TodoForm exposing
    ( Meta(..)
    , Msg
    , System
    , TodoForm
    , getMeta
    , getProjectSortIdx
    , initAdd
    , initEdit
    , isAdding
    , isAddingForInitialDueDate
    , isEditing
    , isEditingFor
    , setProjectSortIdxIfAdding
    , system
    , viewTodoForm
    )

import Basics.More exposing (allPass, flip, perform, propEq)
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


isEditing : TodoForm -> Bool
isEditing =
    unwrapMeta >> isEdit


isAdding : TodoForm -> Bool
isAdding =
    unwrapMeta >> isAdd


isEdit : Meta -> Bool
isEdit meta =
    case meta of
        Edit _ ->
            True

        _ ->
            False


isAdd : Meta -> Bool
isAdd meta =
    case meta of
        Add ->
            True

        _ ->
            False


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


type alias Config msg =
    { onSave : Meta -> List Todo.Patch -> msg, onCancel : msg, toMsg : Msg -> msg }


type alias System msg =
    { view : List Project -> TodoForm -> H.Html msg
    , update : Msg -> TodoForm -> ( TodoForm, Cmd msg )
    , info : TodoForm -> Info
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


update :
    { onSave : Meta -> List Todo.Patch -> Cmd msg, onCancel : Cmd msg }
    -> Msg
    -> TodoForm
    -> ( TodoForm, Cmd msg )
update { onSave, onCancel } message ((TodoForm meta _ current) as model) =
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
            ( model, onSave meta (toPatches current) )

        Cancel ->
            ( model, onCancel )


system : Config msg -> System msg
system { onSave, onCancel, toMsg } =
    { view = viewTodoForm toMsg
    , update = update { onSave = \m p -> perform <| onSave m p, onCancel = perform onCancel }
    , info = info
    }


type alias Info =
    { isAdd : Bool, isEdit : Bool, isEditFor : TodoId -> Bool, isAddFor : Date -> Bool }


info : TodoForm -> Info
info model =
    { isAdd = isAdding model
    , isEdit = isEditing model
    , isEditFor = flip isEditingFor model
    , isAddFor = flip isAddingForInitialDueDate model
    }


viewTodoForm : (Msg -> msg) -> List Project -> TodoForm -> H.Html msg
viewTodoForm toMsg projectList (TodoForm _ _ { title, maybeProjectId, maybeDueDate }) =
    H.form [ A.class "flex flex-column pa1", E.onSubmit Save ]
        [ col [ A.class "pv1" ]
            [ ipt3 title TitleChanged [ A.autofocus True ]
            ]
        , Project.viewSelectOne maybeProjectId ProjectChanged projectList
        , viewDueDateInput maybeDueDate DueDateChanged
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
