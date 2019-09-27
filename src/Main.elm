port module Main exposing (main)

import Basics.More exposing (updateWhenIdEq)
import Browser
import Html
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE exposing (Value, encode, object)
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


type alias Todo =
    { id : TodoId
    , title : String
    , isDone : Bool
    , isDeleted : Bool
    }


createTodo : String -> String -> Todo
createTodo id title =
    Todo (TodoId id) title False False


initialTodoList =
    [ createTodo "1" "Get Milk!!"
    , createTodo "2" "Submit assignment"
    , createTodo "3" "Check Facebook"
    , createTodo "4" "Go to movies"
    , createTodo "5" "Get Milk!!"
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
    , addTodo : Toggle AddTodoForm
    }


defaultCacheValue : Cache
defaultCacheValue =
    { todoList = initialTodoList, addTodo = Off }


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

        fieldsDecoder : Decoder TodoFormFields
        fieldsDecoder =
            JD.succeed TodoFormFields
                |> required "title" JD.string

        todoFormDecoder : Decoder AddTodoForm
        todoFormDecoder =
            JD.succeed AddTodoForm
                |> required "fields" fieldsDecoder
    in
    JD.succeed Cache
        |> optional "todoList" (JD.list todoDecoder) initialTodoList
        |> optional "addTodo" (todoFormDecoder |> JD.map On) Off


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

        addTodoEncoder : AddTodoForm -> Value
        addTodoEncoder { fields } =
            let
                fieldsEncoder { title } =
                    object [ ( "title", JE.string title ) ]
            in
            object
                [ ( "fields", fieldsEncoder fields )
                ]

        modelEncoder : Model -> Value
        modelEncoder { todoList, addTodo } =
            object
                [ ( "todoList", JE.list todoEncoder todoList )
                , ( "addTodo"
                  , case addTodo of
                        On form ->
                            addTodoEncoder form

                        Off ->
                            JE.null
                  )
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


type alias TodoFormFields =
    { title : String }


type alias AddTodoForm =
    { fields : TodoFormFields }


type Toggle a
    = On a
    | Off


unpackToggle : (() -> a) -> (b -> a) -> Toggle b -> a
unpackToggle default func toggle =
    case toggle of
        On a ->
            func a

        Off ->
            default ()


unwrapToggle : a -> (b -> a) -> Toggle b -> a
unwrapToggle default func toggle =
    unpackToggle (always default) func toggle


type alias Model =
    { todoList : List Todo
    , addTodo : Toggle AddTodoForm
    , route : Route
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
            { todoList = cache.todoList
            , addTodo = cache.addTodo
            , route = RouteProject (ProjectId "1")
            }
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
    | SetAddTodoToggle (Toggle AddTodoForm)
    | Save
    | ChangeRouteTo Route


setAddTodoForm : AddTodoForm -> Msg
setAddTodoForm form =
    SetAddTodoToggle (On form)


closeForm =
    SetAddTodoToggle Off


doneChecked : TodoId -> Bool -> Msg
doneChecked todoId isDone =
    PatchTodo todoId (SetDone isDone)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PatchTodo todoId todoPatch ->
            let
                newModel =
                    model
                        |> mapTodoList (updateWhenIdEq todoId (patchTodo todoPatch))
            in
            ( newModel, cacheModel newModel )

        SetAddTodoToggle addTodo ->
            let
                newModel =
                    { model | addTodo = addTodo }
            in
            ( newModel, cacheModel newModel )

        Save ->
            model.addTodo
                |> unwrapToggle ( model, Cmd.none )
                    (\{ fields } ->
                        let
                            newModel =
                                { model
                                    | todoList = createTodo "" fields.title :: model.todoList
                                    , addTodo = Off
                                }
                        in
                        ( newModel, cacheModel newModel )
                    )

        ChangeRouteTo route ->
            ( { model | route = route }, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Html.Html Msg
view model =
    H.toUnstyled <|
        row [ A.class "sans-serif ph4" ]
            [ col [ A.class "pa2 measure-wide" ] (viewNav model)
            , col [ A.class "pa2 flex-grow-1 bl b--black-10 measure-wide" ] (viewPage model model.route)
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
    case item of
        NavInbox ->
            navBtn route "Inbox" RouteInbox

        NavToday ->
            navBtn route "Today" RouteToday

        NavProjects list ->
            col []
                [ row [ A.class "pv2" ] [ H.text "Projects:" ]
                , col [ A.class "pl2" ] (list |> List.map (\(NavProject { id, title }) -> navBtn route title (RouteProject id)))
                ]


navBtn : Route -> String -> Route -> H.Html Msg
navBtn currentRoute title toRoute =
    navBtnHelp (currentRoute == toRoute) title (ChangeRouteTo toRoute)


navBtnHelp : Bool -> String -> msg -> H.Html msg
navBtnHelp isActive title msg =
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
            [ viewTodoList model.todoList
            , viewAddTodo model.addTodo
            ]

        RouteToday ->
            viewPage model RouteInbox

        RouteProject _ ->
            viewPage model RouteInbox


viewTodoList : List Todo -> H.Html Msg
viewTodoList list =
    col [] (List.map viewTodo list)


viewTodo : Todo -> H.Html Msg
viewTodo todo =
    row []
        [ row [ A.class "pa1" ]
            [ checkbox3 todo.isDone (doneChecked todo.id) [ A.class "sz-24" ]
            ]
        , row [ A.class "pa1 flex-grow-1" ] [ H.text todo.title ]
        ]


addTodoFormClicked : Msg
addTodoFormClicked =
    AddTodoForm { title = "" } |> setAddTodoForm


patchAddTodoTitle : AddTodoForm -> String -> Msg
patchAddTodoTitle { fields } title =
    AddTodoForm { fields | title = title } |> setAddTodoForm


viewAddTodo : Toggle AddTodoForm -> H.Html Msg
viewAddTodo addTodo =
    case addTodo of
        On ({ fields } as form) ->
            col [ A.class "pa1" ]
                [ col [ A.class "pv1" ] [ ipt2 fields.title (patchAddTodoTitle form) ]
                , row [ A.class "pv1" ] [ btn2 "Save" Save, btn2 "Cancel" closeForm ]
                ]

        Off ->
            row [ A.class "pa1" ] [ btn2 "add todo" addTodoFormClicked ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
