module TodoForm exposing
    ( Meta(..)
    , Msg
    , System
    , TodoForm
    , getProjectSortIdx
    , isAdding
    , isAddingForInitialDueDate
    , isEditing
    , isEditingFor
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
    = TodoForm (Maybe Internal)


type alias Internal =
    ( Meta, Fields, Fields )


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


unwrapMeta_ : Internal -> Meta
unwrapMeta_ ( meta, _, _ ) =
    meta


unwrapCurrent_ : Internal -> Fields
unwrapCurrent_ ( _, _, current ) =
    current


unwrapMeta : TodoForm -> Maybe Meta
unwrapMeta =
    unwrap >> Maybe.map unwrapMeta_


unwrap : TodoForm -> Maybe Internal
unwrap (TodoForm model) =
    model


isEditingFor : TodoId -> TodoForm -> Bool
isEditingFor todoId =
    unwrap >> MX.unwrap False (unwrapMeta_ >> (==) (Edit todoId))


isEditing : TodoForm -> Bool
isEditing =
    unwrap >> MX.unwrap False (unwrapMeta_ >> isEdit)


isAdding : TodoForm -> Bool
isAdding =
    unwrap >> MX.unwrap False (unwrapMeta_ >> isAdd)


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


getProjectSortIdx : TodoForm -> Maybe Int
getProjectSortIdx =
    unwrapCurrent >> Maybe.map .projectSortIdx


isAddingForInitialDueDate : Date -> TodoForm -> Bool
isAddingForInitialDueDate dueDate =
    allPass
        [ unwrapMeta >> (==) (Just Add)
        , unwrapInitial >> MX.unwrap False (propEq .maybeDueDate (Just dueDate))
        ]


unwrapCurrent : TodoForm -> Maybe Fields
unwrapCurrent =
    unwrap >> Maybe.map unwrapCurrent_


unwrapInitial_ : Internal -> Fields
unwrapInitial_ ( _, initial, _ ) =
    initial


unwrapInitial : TodoForm -> Maybe Fields
unwrapInitial =
    unwrap >> Maybe.map unwrapInitial_


empty : Fields
empty =
    Fields "" Nothing Nothing Random.maxInt


initAdd : (Fields -> Fields) -> TodoForm
initAdd func =
    init Add <| func empty


init : Meta -> Fields -> TodoForm
init meta internal =
    TodoForm <| Just ( meta, internal, internal )


initEdit : Todo -> TodoForm
initEdit { id, title, maybeProjectId, maybeDueDate, projectSortIdx } =
    init (Edit id) <| Fields title maybeProjectId maybeDueDate projectSortIdx


type alias Config msg =
    { onSave : Meta -> List Todo.Patch -> msg, onCancel : msg, toMsg : Msg -> msg }


type alias System msg =
    { view : List Project -> TodoForm -> H.Html msg
    , update : Msg -> TodoForm -> ( TodoForm, Cmd msg )
    , info : TodoForm -> Info
    , initAddForProject : Maybe ProjectId -> Int -> TodoForm
    , initAddForDueDate : Date -> TodoForm
    , initEdit : Todo -> TodoForm
    , model : TodoForm
    }


system : Config msg -> System msg
system { onSave, onCancel, toMsg } =
    { view = viewTodoForm toMsg
    , update = update { onSave = \m p -> perform <| onSave m p, onCancel = perform onCancel }
    , info = info
    , initAddForProject = \maybeProjectId projectSortIdx -> initAdd (\d -> { d | maybeProjectId = maybeProjectId, projectSortIdx = projectSortIdx })
    , initAddForDueDate = \dueDate -> initAdd (\d -> { d | maybeDueDate = Just dueDate })
    , initEdit = initEdit
    , model = TodoForm Nothing
    }


mapCurrent_ : (Fields -> Fields) -> Internal -> Internal
mapCurrent_ func ( m, i, c ) =
    ( m, i, func c )


mapCurrent : (Fields -> Fields) -> TodoForm -> TodoForm
mapCurrent func =
    map (mapCurrent_ func)


map : (Internal -> Internal) -> TodoForm -> TodoForm
map func (TodoForm mi) =
    Maybe.map func mi |> TodoForm


type Msg
    = TitleChanged String
    | ProjectChanged (Maybe ProjectId)
    | DueDateChanged (Maybe Date)
    | Save
    | Cancel


update :
    { onSave : Meta -> List Todo.Patch -> Cmd msg, onCancel : Cmd msg }
    -> Msg
    -> TodoForm
    -> ( TodoForm, Cmd msg )
update { onSave, onCancel } message ((TodoForm mi) as model) =
    case message of
        TitleChanged title ->
            ( mapCurrent (\f -> { f | title = title }) model, Cmd.none )

        ProjectChanged maybeProjectId ->
            ( mapCurrent (\f -> { f | maybeProjectId = maybeProjectId }) model, Cmd.none )

        DueDateChanged maybeDueDate ->
            ( mapCurrent (\f -> { f | maybeDueDate = maybeDueDate }) model, Cmd.none )

        Save ->
            mi |> MX.unwrap ( model, Cmd.none ) (\( m, _, c ) -> ( TodoForm Nothing, onSave m (toPatches c) ))

        Cancel ->
            mi |> MX.unwrap ( model, Cmd.none ) (always ( TodoForm Nothing, onCancel ))


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
viewTodoForm toMsg projectList (TodoForm mi) =
    case mi of
        Just ( _, _, { title, maybeProjectId, maybeDueDate } ) ->
            H.form [ A.class "flex flex-column pa1", E.onSubmit Save ]
                [ col [ A.class "pv1" ]
                    [ ipt3 title TitleChanged [ A.autofocus True ]
                    ]
                , Project.viewSelectOne maybeProjectId ProjectChanged projectList
                , viewDueDateInput maybeDueDate DueDateChanged
                , row [ A.class "pv1" ] [ submit "Save" [], btn2 "Cancel" Cancel ]
                ]
                |> H.map toMsg

        Nothing ->
            H.text ""


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
