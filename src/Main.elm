port module Main exposing (main)

import Basics.More exposing (HasId, updateWhenIdEq, upsertById)
import Browser
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
    }


createMockTodo : String -> String -> Todo
createMockTodo id title =
    Todo (TodoId id) title False False Nothing


todoFromFields : AddTodoFields -> Todo
todoFromFields { newTodoId, title, maybeProjectId } =
    Todo newTodoId title False False maybeProjectId


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
    in
    JD.succeed Cache
        |> optional "todoList" (JD.list todoDecoder) initialTodoList


cacheModel : Model -> Cmd msg
cacheModel model =
    let
        maybeEncoder =
            MX.unwrap JE.null

        todoEncoder : Todo -> Value
        todoEncoder { id, title, isDone, isDeleted, maybeProjectId } =
            object
                [ ( "id", todoIdEncoder id )
                , ( "title", JE.string title )
                , ( "isDone", JE.bool isDone )
                , ( "isDeleted", JE.bool isDeleted )
                , ( "maybeProjectId", maybeEncoder projectIdEncoder maybeProjectId )
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
    }


type TodoForm
    = AddTodoForm AddTodoFields
    | EditTodoForm Todo


type alias Model =
    { todoList : List Todo
    , maybeTodoForm : Maybe TodoForm
    , route : Route
    , zone : Time.Zone
    , today : Time.Posix
    }


defaultModel : Model
defaultModel =
    { todoList = initialTodoList
    , maybeTodoForm = Nothing
    , route = RouteProject (ProjectId "1")
    , zone = Time.utc
    , today = Time.millisToPosix 0
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
    Time.now |> Task.perform GotToday


mapTodoList : (small -> small) -> { big | todoList : small } -> { big | todoList : small }
mapTodoList func model =
    { model | todoList = func model.todoList }



-- UPDATE


type Msg
    = NoOp
    | PatchTodo TodoId TodoPatch
    | AddTodoFormClicked
    | SetMaybeTodoForm (Maybe TodoForm)
    | Save
    | ChangeRouteTo Route
    | ResetModel
    | GotZone Time.Zone
    | GotToday Time.Posix


setTodoForm : TodoForm -> Msg
setTodoForm form =
    SetMaybeTodoForm (Just form)


addTodoFormClicked : Msg
addTodoFormClicked =
    --    setTodoForm (AddTodoForm { title = "" })
    AddTodoFormClicked


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

        AddTodoFormClicked ->
            ( model
            , todoIdGen
                |> Random.generate
                    (\todoId ->
                        setTodoForm (AddTodoForm { newTodoId = todoId, title = "", maybeProjectId = Nothing })
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
                , col [ A.class "pa2 flex-grow-1 bl b--black-10 measure-wide" ] (viewPage model model.route)
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


viewPage : Model -> Route -> List (H.Html Msg)
viewPage model route =
    case route of
        RouteInbox ->
            [ viewTodoList model
            , viewAddTodo model.maybeTodoForm
            ]

        RouteToday ->
            viewPage model RouteInbox

        RouteProject _ ->
            viewPage model RouteInbox


viewTodoList : { a | maybeTodoForm : Maybe TodoForm, todoList : List Todo } -> H.Html Msg
viewTodoList { maybeTodoForm, todoList } =
    col []
        (List.map
            (\todo ->
                case maybeTodoForm of
                    Just (EditTodoForm editTodo) ->
                        if todo.id == editTodo.id then
                            viewEditTodoForm editTodo

                        else
                            viewTodo todo

                    _ ->
                        viewTodo todo
            )
            todoList
        )


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
        , row [ A.class " " ] [ H.text <| todoProjectTitle todo ]
        ]


todoProjectTitle { maybeProjectId } =
    initialProjectList
        |> LX.find (.id >> (\id -> Just id == maybeProjectId))
        |> MX.unwrap "Inbox" .title


viewAddTodo : Maybe TodoForm -> H.Html Msg
viewAddTodo addTodo =
    case addTodo of
        Just (AddTodoForm fields) ->
            viewAddTodoForm fields

        Nothing ->
            row [ A.class "pa1" ] [ btn2 "add todo" addTodoFormClicked ]

        _ ->
            H.text ""


viewEditTodoForm fields =
    let
        setForm =
            setTodoForm << EditTodoForm

        config =
            { titleChanged = \title -> setForm { fields | title = title }
            , projectIdChanged = \maybeProjectId -> setForm { fields | maybeProjectId = maybeProjectId }
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
            }
    in
    viewTodoForm config fields


viewTodoForm config { title, maybeProjectId } =
    col [ A.class "pa1" ]
        [ col [ A.class "pv1" ]
            [ ipt2 title config.titleChanged
            ]
        , viewProjectSelect maybeProjectId config.projectIdChanged
        , row [ A.class "pv1" ] [ btn2 "Save" Save, btn2 "Cancel" closeForm ]
        ]


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
