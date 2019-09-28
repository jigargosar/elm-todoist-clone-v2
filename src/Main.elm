port module Main exposing (main)

import Basics.More exposing (HasId, allPass, anyPass, idEq, propEq, updateWhenIdEq, upsertById)
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
    | RouteNext7Days



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


todoFromFields : TodoId -> AddTodoFields -> Todo
todoFromFields id { title, maybeProjectId, maybeDueDate } =
    Todo id title False False maybeProjectId maybeDueDate


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
    { title : String
    , maybeProjectId : Maybe ProjectId
    , maybeDueDate : Maybe Date
    , initialDueDate : Maybe Date
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

    --    , route = RouteProject (ProjectId "1")
    , route = RouteNext7Days
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
    | AddTodoClicked (Maybe ProjectId) (Maybe Date)
    | SetMaybeTodoForm (Maybe TodoForm)
    | Save
    | UpsertTodoOnSaveClicked Todo
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

        AddTodoClicked maybeProjectId maybeDueDate ->
            ( { model
                | maybeTodoForm =
                    Just <|
                        AddTodoForm
                            { title = ""
                            , maybeProjectId = maybeProjectId
                            , maybeDueDate = maybeDueDate
                            , initialDueDate = maybeDueDate
                            }
              }
            , Cmd.none
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

        UpsertTodoOnSaveClicked todo ->
            upsertTodoOnSaveClicked todo model


handleSave form model =
    case form of
        AddTodoForm fields ->
            ( model
            , Random.generate
                (\todoId ->
                    todoFromFields todoId fields |> UpsertTodoOnSaveClicked
                )
                todoIdGen
            )

        EditTodoForm editingTodo ->
            upsertTodoOnSaveClicked editingTodo model


upsertTodoOnSaveClicked todo model =
    let
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
            , NavNext7Days
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
    | NavNext7Days
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

        NavNext7Days ->
            navBtnHelp "Next 7 Days" RouteNext7Days

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
            viewTodoListForMaybeProjectId Nothing model

        RouteToday ->
            viewTodoListDueTodayWithOverDue model

        RouteProject projectId ->
            viewTodoListForMaybeProjectId (Just projectId) model

        RouteNext7Days ->
            viewNext7DaysTodoList model


getEditTodoForm : Maybe TodoForm -> Maybe Todo
getEditTodoForm maybeForm =
    case maybeForm of
        Just (EditTodoForm editTodo) ->
            Just editTodo

        _ ->
            Nothing


getEditTodoFormTodoId : TodoId -> Maybe TodoForm -> Maybe Todo
getEditTodoFormTodoId todoId =
    getEditTodoForm >> MX.filter (idEq todoId)


getAddTodoForm : Maybe TodoForm -> Maybe AddTodoFields
getAddTodoForm maybeForm =
    case maybeForm of
        Just (AddTodoForm fields) ->
            Just fields

        _ ->
            Nothing


getAddTodoFormWithInitialDueDateEq : Date -> Maybe TodoForm -> Maybe AddTodoFields
getAddTodoFormWithInitialDueDateEq date =
    getAddTodoForm >> MX.filter (propEq .initialDueDate (Just date))


viewNext7DaysTodoList : Model -> List (H.Html Msg)
viewNext7DaysTodoList model =
    let
        dateRange : Int -> Int -> List Date
        dateRange from to =
            List.range from to
                |> List.map (\ct -> Date.add Date.Days ct model.today)
    in
    dateRange 0 6 |> List.concatMap (\date -> viewTodoListDueOn date model)


viewTodoListDueTodayWithOverDue model =
    viewOverDueTodoList model
        ++ viewTodoListDueOn model.today model


viewOverDueTodoList : Model -> List (H.Html Msg)
viewOverDueTodoList ({ today, todoList, maybeTodoForm } as model) =
    let
        filterPredicate =
            allPass
                [ .maybeDueDate >> MX.unwrap False (\dueDate -> Date.compare dueDate today == LT)
                , propEq .isDone False
                ]

        filteredTodoList =
            todoList |> List.filter filterPredicate
    in
    col [] [ H.text "OverDue" ]
        :: viewInlineEditableTodoList DueDateItemLayout model filteredTodoList


viewTodoListDueOn : Date -> Model -> List (H.Html Msg)
viewTodoListDueOn dueDate ({ today, todoList, maybeTodoForm } as model) =
    let
        filterPredicate =
            allPass
                [ propEq .maybeDueDate (Just dueDate)
                , propEq .isDone False
                ]

        filteredTodoList =
            todoList |> List.filter filterPredicate
    in
    col [ A.class "ph1 pb1 pt3" ] [ H.text <| humanDate dueDate today ]
        :: viewInlineEditableTodoList DueDateItemLayout model filteredTodoList
        ++ [ getAddTodoFormWithInitialDueDateEq dueDate maybeTodoForm
                |> MX.unwrap (viewAddTodoButton (AddTodoClicked Nothing (Just dueDate))) viewAddTodoForm
           ]


humanDate date today =
    let
        addDays ct =
            Date.add Date.Days ct today
    in
    if date == addDays -1 then
        "Yesterday"

    else if date == addDays 0 then
        "Today"

    else if date == addDays 1 then
        "Tomorrow"

    else if Date.year date == Date.year today then
        Date.format "ddd MMM" date

    else
        Date.format "ddd MMM YYYY" date


viewTodoListForMaybeProjectId : Maybe ProjectId -> Model -> List (H.Html Msg)
viewTodoListForMaybeProjectId maybeProjectId ({ maybeTodoForm, todoList } as model) =
    let
        filteredTodoList =
            List.filter (propEq .maybeProjectId maybeProjectId) todoList
    in
    viewInlineEditableTodoList ProjectItemLayout model filteredTodoList
        ++ [ getAddTodoForm maybeTodoForm
                |> MX.unwrap (viewAddTodoButton (AddTodoClicked maybeProjectId Nothing)) viewAddTodoForm
           ]


viewInlineEditableTodoList : TodoItemLayout -> Model -> List Todo -> List (H.Html Msg)
viewInlineEditableTodoList layout { today, maybeTodoForm } =
    List.map
        (\todo ->
            getEditTodoFormTodoId todo.id maybeTodoForm
                |> MX.unpack (\_ -> viewTodo today layout todo) viewEditTodoForm
        )


type TodoItemLayout
    = ProjectItemLayout
    | DueDateItemLayout


todoProjectTitle : { a | maybeProjectId : Maybe ProjectId } -> String
todoProjectTitle { maybeProjectId } =
    initialProjectList
        |> LX.find (.id >> (\id -> Just id == maybeProjectId))
        |> MX.unwrap "Inbox" .title


viewTodo : Date -> TodoItemLayout -> Todo -> H.Html Msg
viewTodo today layout todo =
    row []
        [ row [ A.class "pa1" ]
            [ checkbox3 todo.isDone (doneChecked todo.id) [ A.class "sz-24" ]
            ]
        , row
            [ A.class "pa1 flex-grow-1"
            , E.onClick (editTodoClicked todo)
            ]
            [ H.text todo.title ]
        , case layout of
            ProjectItemLayout ->
                todo.maybeDueDate
                    |> MX.unwrap (row [ A.class "self-start pa1 f7 code" ] [ H.text "[]" ])
                        (\dueDate ->
                            row [ A.class "self-start pa1 f7 code" ] [ H.text (humanDate dueDate today) ]
                        )

            DueDateItemLayout ->
                row [ A.class "self-start lh-solid pa1 f7 ba br-pill bg-black-10" ]
                    [ H.text <| todoProjectTitle todo ]
        ]


viewAddTodoButton : Msg -> H.Html Msg
viewAddTodoButton onClick =
    row [ A.class "pa1" ] [ btn2 "add todo" onClick ]


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


viewEditTodoForm : Todo -> H.Html Msg
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
