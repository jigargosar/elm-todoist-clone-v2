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
import Maybe.Extra as MX
import Random
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


type alias Project =
    { id : ProjectId
    , title : String
    , isDeleted : Bool
    }


createProject : String -> String -> Project
createProject id title =
    Project (ProjectId id) title False


initialProjectList =
    [ createProject "1" "Build Utils"
    , createProject "2" "Publish Post"
    , createProject "3" "Complete Story"
    , createProject "4" "Exam Prep"
    ]



-- TODO_


type TodoId
    = TodoId String


todoIdGen : Random.Generator TodoId
todoIdGen =
    Random.int (10 ^ 3) (10 ^ 5)
        |> Random.map (String.fromInt >> (++) "TodoId-" >> TodoId)


type alias Todo =
    { id : TodoId
    , title : String
    , isDone : Bool
    , isDeleted : Bool
    }


createMockTodo : String -> String -> Todo
createMockTodo id title =
    Todo (TodoId id) title False False


todoFromFields : AddTodoFields -> Todo
todoFromFields { newTodoId, title } =
    Todo newTodoId title False False


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
    in
    JD.succeed Cache
        |> optional "todoList" (JD.list todoDecoder) initialTodoList


cacheModel : Model -> Cmd msg
cacheModel model =
    let
        todoEncoder : Todo -> Value
        todoEncoder { id, title, isDone, isDeleted } =
            let
                unwrapId (TodoId v) =
                    v
            in
            object
                [ ( "id", JE.string <| unwrapId id )
                , ( "title", JE.string title )
                , ( "isDone", JE.bool isDone )
                , ( "isDeleted", JE.bool isDeleted )
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
    { newTodoId : TodoId, title : String }


type TodoForm
    = AddTodoForm AddTodoFields
    | EditTodoForm Todo


type alias Model =
    { todoList : List Todo
    , maybeTodoForm : Maybe TodoForm
    , route : Route
    }


defaultModel =
    { todoList = initialTodoList
    , maybeTodoForm = Nothing
    , route = RouteProject (ProjectId "1")
    }


init : Flags -> ( Model, Cmd msg )
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
    ( model
    , cacheModel model
    )


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
            ( defaultModel, cacheModel defaultModel )

        ChangeRouteTo route ->
            ( { model | route = route }, Cmd.none )

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
                        setTodoForm (AddTodoForm { newTodoId = todoId, title = "" })
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
        ]


viewAddTodo : Maybe TodoForm -> H.Html Msg
viewAddTodo addTodo =
    case addTodo of
        Just (AddTodoForm fields) ->
            viewAddTodoForm fields

        Nothing ->
            row [ A.class "pa1" ] [ btn2 "add todo" addTodoFormClicked ]

        _ ->
            H.text ""


viewEditTodoForm todo =
    let
        setForm =
            setTodoForm << EditTodoForm

        config =
            { titleChanged = \title -> setForm { todo | title = title }
            }
    in
    viewTodoForm config todo


viewAddTodoForm fields =
    let
        setForm =
            setTodoForm << AddTodoForm

        config =
            { titleChanged = \title -> setForm { fields | title = title } }
    in
    viewTodoForm config fields


viewTodoForm config { title } =
    col [ A.class "pa1" ]
        [ col [ A.class "pv1" ]
            [ ipt2 title config.titleChanged
            ]
        , row [ A.class "pv1" ] [ btn2 "Save" Save, btn2 "Cancel" closeForm ]
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
