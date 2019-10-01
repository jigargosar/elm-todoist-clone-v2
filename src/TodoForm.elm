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
    , isEditingFor
    , setProjectSortIdxIfAdding
    , toPartialWithMeta
    , viewTodoForm
    )

import Basics.More exposing (allPass, ifElse, propEq)
import Date exposing (Date)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JDX
import Json.Decode.Pipeline exposing (required)
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


unwrap : TodoForm -> Internal
unwrap (TodoForm _ _ internal) =
    internal


unwrapInitial : TodoForm -> Internal
unwrapInitial (TodoForm _ initial _) =
    initial


setProjectSortIdxIfAdding : Int -> TodoForm -> Maybe TodoForm
setProjectSortIdxIfAdding projectSortIdx =
    mapIfAdding (\f -> { f | projectSortIdx = projectSortIdx })


mapIfAdding : (Internal -> Internal) -> TodoForm -> Maybe TodoForm
mapIfAdding func (TodoForm meta initial current) =
    case meta of
        Add ->
            Just <| TodoForm meta initial <| func current

        _ ->
            Nothing


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


succeedIf : (a -> Bool) -> Decoder a -> Decoder a
succeedIf pred =
    JD.andThen
        (\val ->
            if pred val then
                JD.succeed val

            else
                JD.fail "pred failed"
        )


succeedIfEq : a -> Decoder a -> Decoder a
succeedIfEq val =
    succeedIf ((==) val)


enter : a -> Decoder a
enter msg =
    JD.succeed (\_ _ _ _ _ -> msg)
        |> required "ctrlKey" (succeedIfEq False JD.bool)
        |> required "shiftKey" (succeedIfEq False JD.bool)
        |> required "altKey" (succeedIfEq False JD.bool)
        |> required "metaKey" (succeedIfEq False JD.bool)
        |> required "key" (succeedIfEq "Enter" JD.string)


onKeyDown : List (Decoder a) -> H.Attribute a
onKeyDown =
    E.on "keydown" << JD.oneOf


viewTodoForm : Config msg -> List Project -> TodoForm -> H.Html msg
viewTodoForm (Config { onSave, onCancel, toMsg }) projectList (TodoForm meta initial model) =
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
    col [ A.class "pa1", onKeyDown [ enter onSave ] ]
        [ col [ A.class "pv1" ]
            [ ipt2 model.title titleChanged
            ]
        , Project.viewSelectOne model.maybeProjectId projectChanged projectList
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
