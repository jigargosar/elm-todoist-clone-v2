port module Main exposing (main)

import Basics.More exposing (HasId, propEq, updateWhenIdEq, upsertById)
import Browser
import Date exposing (Date)
import Html
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE exposing (Value, encode, object)
import List.Extra as LX
import Maybe.Extra as MX
import Random
import Task
import Time
import UI exposing (btn2, checkbox3, col, ipt2, row)


type Route
    = RouteInbox
    | RouteToday
    | RouteProject ProjectId



--    | Next7Days
--    | Search
-- PROJECT


type ProjectId
    = ProjectId String


projectIdFromString : String -> Maybe ProjectId
projectIdFromString str =
    if str |> String.trim |> String.isEmpty then
        Nothing

    else
        Just <| ProjectId (String.trim str)


projectIdEncoder (ProjectId str) =
    JE.string str


projectIdToValueAttr : ProjectId -> H.Attribute msg
projectIdToValueAttr (ProjectId str) =
    A.value str


type alias Project =
    { id : ProjectId
    , title : String
    , isDeleted : Bool
    }


createMockProject : String -> String -> Project
createMockProject id title =
    Project (ProjectId id) title False


initialProjectList =
    [ createMockProject "1" "Build Utils"
    , createMockProject "2" "Publish Post"
    , createMockProject "3" "Complete Story"
    , createMockProject "4" "Exam Prep"
    ]



-- TODO_


type TodoId
    = TodoId String


todoIdGen : Random.Generator TodoId
todoIdGen =
    Random.int (10 ^ 3) (10 ^ 5)
        |> Random.map (String.fromInt >> (++) "TodoId-" >> TodoId)


todoIdEncoder (TodoId v) =
    JE.string v


type alias Todo =
    { id : TodoId
    , title : String
    , isDone : Bool
    , isDeleted : Bool
    , maybeProjectId : Maybe ProjectId
    , maybeDueDate : Maybe Date
    }


createMockTodo : String -> String -> Todo
createMockTodo id title =
    Todo (TodoId id) title False False Nothing Nothing


todoFromFields : AddTodoFields -> Todo
todoFromFields { newTodoId, title, maybeProjectId, maybeDueDate } =
    Todo newTodoId title False False maybeProjectId maybeDueDate


initialTodoList =
    [ createMockTodo "1" "Get Milk!!"
    , createMockTodo "2" "Submit assignment"
    , createMockTodo "3" "Check Facebook"
    , createMockTodo "4" "Go to movies"
    , createMockTodo "5" "Get Milk!!"
    ]


type TodoPatch
    = SetDone Bool
    | SetDeleted Bool
    | TodoPatches (List TodoPatch)


patchTodo : TodoPatch -> Todo -> Todo
patchTodo patch todo =
    case patch of
        SetDone isDone ->
            { todo | isDone = isDone }

        TodoPatches patches ->
            List.foldl patchTodo todo patches

        SetDeleted isDeleted ->
            { todo | isDeleted = isDeleted }



-- CACHE


port setCache : String -> Cmd msg


type alias Cache =
    { todoList : List Todo
    }


defaultCacheValue : Cache
defaultCacheValue =
    { todoList = initialTodoList }


cacheDecoder : JD.Decoder Cache
cacheDecoder =
    let
        todoDecoder : JD.Decoder Todo
        todoDecoder =
            JD.succeed Todo
                |> required "id" (JD.string |> JD.map TodoId)
                |> required "title" JD.string
                |> required "isDone" JD.bool
                |> optional "isDeleted" JD.bool False
                |> optional "maybeProjectId"
                    (JD.string |> JD.map (ProjectId >> Just))
                    Nothing
                |> optional "maybeDueDate" (JD.string |> JD.map (Date.fromIsoString >> Result.toMaybe)) Nothing
    in
    JD.succeed Cache
        |> optional "todoList" (JD.list todoDecoder) initialTodoList


cacheModel : Model -> Cmd msg
cacheModel model =
    let
        maybeEncoder =
            MX.unwrap JE.null

        todoEncoder : Todo -> Value
        todoEncoder { id, title, isDone, isDeleted, maybeProjectId, maybeDueDate } =
            object
                [ ( "id", todoIdEncoder id )
                , ( "title", JE.string title )
                , ( "isDone", JE.bool isDone )
                , ( "isDeleted", JE.bool isDeleted )
                , ( "maybeProjectId", maybeEncoder projectIdEncoder maybeProjectId )
                , ( "maybeDueDate", maybeEncoder (Date.toIsoString >> JE.string) maybeDueDate )
                ]

        modelEncoder : Model -> Value
        modelEncoder { todoList } =
            object
                [ ( "todoList", JE.list todoEncoder todoList )
                ]

        modelValue =
            modelEncoder model
    in
    setCache <| encode 0 modelValue


stringOrValueDecoder : JD.Decoder a -> JD.Decoder a
stringOrValueDecoder decoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\str ->
                    case JD.decodeString decoder str of
                        Err err ->
                            JD.fail <| JD.errorToString err

                        Ok cache ->
                            JD.succeed cache
                )
        , decoder
        ]



-- MODEL


type alias Flags =
    { cache : Value
    , now : Int
    }


type alias AddTodoFields =
    { newTodoId : TodoId
    , title : String
    , maybeProjectId : Maybe ProjectId
    , maybeDueDate : Maybe Date
    }


type TodoForm
    = AddTodoForm AddTodoFields
    | EditTodoForm Todo


type alias Model =
    { todoList : List Todo
    , maybeTodoForm : Maybe TodoForm
    , route : Route
    , zone : Time.Zone
    , today : Date
    }


defaultModel : Model
defaultModel =
    { todoList = initialTodoList
    , maybeTodoForm = Nothing
    , route = RouteProject (ProjectId "1")
    , zone = Time.utc
    , today = Date.fromRataDie 0
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        cache =
            flags.cache
                |> JD.decodeValue (stringOrValueDecoder cacheDecoder)
                |> Result.withDefault defaultCacheValue

        model : Model
        model =
            { defaultModel | todoList = cache.todoList }
    in
    initModel model


initModel : Model -> ( Model, Cmd Msg )
initModel model =
    ( model
    , Cmd.batch [ cacheModel model, getZone, getToday ]
    )


getZone : Cmd Msg
getZone =
    Time.here |> Task.perform GotZone


getToday =
    Date.today |> Task.perform GotToday


mapTodoList : (small -> small) -> { big | todoList : small } -> { big | todoList : small }
mapTodoList func model =
    { model | todoList = func model.todoList }



-- UPDATE


type Msg
    = NoOp
    | PatchTodo TodoId TodoPatch
    | AddTodoFormClicked (Maybe ProjectId)
    | SetMaybeTodoForm (Maybe TodoForm)
    | Save
    | ChangeRouteTo Route
    | ResetModel
    | GotZone Time.Zone
    | GotToday Date


setTodoForm : TodoForm -> Msg
setTodoForm form =
    SetMaybeTodoForm (Just form)


editTodoClicked : Todo -> Msg
editTodoClicked todo =
    setTodoForm (EditTodoForm todo)


closeForm =
    SetMaybeTodoForm Nothing


doneChecked : TodoId -> Bool -> Msg
doneChecked todoId isDone =
    PatchTodo todoId (SetDone isDone)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ResetModel ->
            initModel defaultModel

        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )

        GotToday today ->
            ( { model | today = today }, Cmd.none )

        ChangeRouteTo route ->
            initModel { model | route = route, maybeTodoForm = Nothing }

        PatchTodo todoId todoPatch ->
            let
                newModel =
                    model
                        |> mapTodoList (updateWhenIdEq todoId (patchTodo todoPatch))
            in
            ( newModel, cacheModel newModel )

        AddTodoFormClicked maybeProjectId ->
            ( model
            , todoIdGen
                |> Random.generate
                    (\todoId ->
                        setTodoForm
                            (AddTodoForm
                                { newTodoId = todoId
                                , title = ""
                                , maybeProjectId = maybeProjectId
                                , maybeDueDate = Nothing
                                }
                            )
                    )
            )

        SetMaybeTodoForm addTodo ->
            let
                newModel =
                    { model | maybeTodoForm = addTodo }
            in
            ( newModel, cacheModel newModel )

        Save ->
            case model.maybeTodoForm of
                Just form ->
                    handleSave form model

                Nothing ->
                    ( model, Cmd.none )


handleSave form model =
    let
        todo =
            case form of
                AddTodoForm fields ->
                    todoFromFields fields

                EditTodoForm editingTodo ->
                    editingTodo

        newModel =
            { model
                | todoList = upsertById todo model.todoList
                , maybeTodoForm = Nothing
            }
    in
    ( newModel, cacheModel newModel )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Html.Html Msg
view model =
    H.toUnstyled <|
        col [ A.class "sans-serif ph4" ]
            [ row [ A.class "bg-black-10 pa1" ] [ btn2 "Reset" ResetModel ]
            , row []
                [ col [ A.class "pa2" ] (viewNav model)
                , col [ A.class "pa2 flex-grow-1 bl b--black-10 measure-wide" ] (viewRoute model model.route)
                ]
            ]


viewNav : Model -> List (H.Html Msg)
viewNav { route } =
    let
        navItems =
            [ NavInbox
            , NavToday
            , NavProjects
                (initialProjectList |> List.map NavProject)
            ]
    in
    navItems |> List.map (viewNavItem route)


type NavProject
    = NavProject Project


type NavItem
    = NavInbox
    | NavToday
    | NavProjects (List NavProject)


viewNavItem : Route -> NavItem -> H.Html Msg
viewNavItem route item =
    let
        navBtnHelp title toRoute =
            navBtn (route == toRoute) title (ChangeRouteTo toRoute)
    in
    case item of
        NavInbox ->
            navBtnHelp "Inbox" RouteInbox

        NavToday ->
            navBtnHelp "Today" RouteToday

        NavProjects list ->
            col []
                [ row [ A.class "pv2" ] [ H.text "Projects:" ]
                , col [ A.class "pl2" ]
                    (list
                        |> List.map
                            (\(NavProject { id, title }) ->
                                navBtnHelp title (RouteProject id)
                            )
                    )
                ]


navBtn : Bool -> String -> msg -> H.Html msg
navBtn isActive title msg =
    H.button
        [ A.class "tl pa1"
        , A.classList
            [ ( "white bg-primary", isActive ), ( "color-primary", not isActive ) ]
        , E.onClick msg
        ]
        [ H.text title ]


viewRoute : Model -> Route -> List (H.Html Msg)
viewRoute model route =
    case route of
        RouteInbox ->
            maybeProjectIdViewModel Nothing model |> viewTodoListItems

        RouteToday ->
            viewRoute model RouteInbox

        RouteProject projectId ->
            maybeProjectIdViewModel (Just projectId) model |> viewTodoListItems


type TodoListItem
    = TodoItem Todo
    | EditTodoItem Todo
    | ProjectAddTodoBtn (Maybe ProjectId)
    | ProjectAddTodoForm AddTodoFields


maybeProjectIdViewModel : Maybe ProjectId -> Model -> List TodoListItem
maybeProjectIdViewModel maybeProjectId { maybeTodoForm, todoList } =
    let
        filteredTodoList =
            List.filter (propEq .maybeProjectId maybeProjectId) todoList
    in
    todoItemsFromList maybeTodoForm filteredTodoList
        ++ [ case maybeTodoForm of
                Just (AddTodoForm fields) ->
                    ProjectAddTodoForm fields

                _ ->
                    ProjectAddTodoBtn maybeProjectId
           ]


todoItemsFromList maybeTodoForm todoList =
    List.map
        (\todo ->
            case maybeTodoForm of
                Just (EditTodoForm editTodo) ->
                    if todo.id == editTodo.id then
                        EditTodoItem editTodo

                    else
                        TodoItem todo

                _ ->
                    TodoItem todo
        )
        todoList


viewTodoListItems =
    let
        viewItem item =
            case item of
                TodoItem todo ->
                    viewTodo todo

                EditTodoItem todo ->
                    viewEditTodoForm todo

                ProjectAddTodoBtn maybeProjectId ->
                    row [ A.class "pa1" ] [ btn2 "add todo" (AddTodoFormClicked maybeProjectId) ]

                ProjectAddTodoForm fields ->
                    viewAddTodoForm fields
    in
    List.map viewItem


viewTodo : Todo -> H.Html Msg
viewTodo todo =
    row []
        [ row [ A.class "pa1" ]
            [ checkbox3 todo.isDone (doneChecked todo.id) [ A.class "sz-24" ]
            ]
        , row
            [ A.class "pa1 flex-grow-1"
            , E.onClick (editTodoClicked todo)
            ]
            [ H.text todo.title ]
        , todo.maybeDueDate
            |> MX.unwrap (row [ A.class "self-start pa1 f7 code" ] [ H.text "[]" ])
                (\dueDate ->
                    row [ A.class "self-start pa1 f7 code" ] [ H.text (Date.toIsoString dueDate) ]
                )
        , row [ A.class "self-start lh-solid pa1 f7 ba br-pill bg-black-10" ] [ H.text <| todoProjectTitle todo ]
        ]


todoProjectTitle { maybeProjectId } =
    initialProjectList
        |> LX.find (.id >> (\id -> Just id == maybeProjectId))
        |> MX.unwrap "Inbox" .title


viewEditTodoForm fields =
    let
        setForm =
            setTodoForm << EditTodoForm

        config =
            { titleChanged = \title -> setForm { fields | title = title }
            , projectIdChanged = \maybeProjectId -> setForm { fields | maybeProjectId = maybeProjectId }
            , dueDateChanged = \maybeDueDate -> setForm { fields | maybeDueDate = maybeDueDate }
            }
    in
    viewTodoForm config fields


viewAddTodoForm fields =
    let
        setForm =
            setTodoForm << AddTodoForm

        config =
            { titleChanged = \title -> setForm { fields | title = title }
            , projectIdChanged = \maybeProjectId -> setForm { fields | maybeProjectId = maybeProjectId }
            , dueDateChanged = \maybeDueDate -> setForm { fields | maybeDueDate = maybeDueDate }
            }
    in
    viewTodoForm config fields


viewTodoForm config { title, maybeProjectId, maybeDueDate } =
    col [ A.class "pa1" ]
        [ col [ A.class "pv1" ]
            [ ipt2 title config.titleChanged
            ]
        , viewProjectSelect maybeProjectId config.projectIdChanged
        , viewDueDateInput maybeDueDate config.dueDateChanged
        , row [ A.class "pv1" ] [ btn2 "Save" Save, btn2 "Cancel" closeForm ]
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


viewProjectSelect maybeProjectId projectIdChanged =
    let
        viewOpt { id, title } =
            H.option
                [ A.selected (maybeProjectId == Just id)
                , projectIdToValueAttr id
                ]
                [ H.text title ]
    in
    H.select [ E.onInput (projectIdFromString >> projectIdChanged) ]
        (H.option [] [ H.text "Inbox" ]
            :: List.map viewOpt initialProjectList
        )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
