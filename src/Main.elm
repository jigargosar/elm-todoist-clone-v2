port module Main exposing (main)

import Basics.More exposing (HasId, allPass, appendOne, flip, idEq, insertAt, propEq, uncurry, updateWhenIdEq)
import Browser
import Browser.Events
import Date exposing (Date)
import DnDList
import Firebase.Auth as Auth exposing (AuthState)
import Firebase.Firestore as Firestore
import HasSeed
import Html
import Html.Attributes as HA
import Html.Styled as H
import Html.Styled.Attributes as A exposing (style)
import Html.Styled.Events as E
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as JE exposing (Value, encode, object)
import List.Extra as LX
import Maybe.Extra as MX
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Random
import Return
import SchedulePopup
import Tagged.Dict as TaggedDict exposing (TaggedDict)
import Task
import Time exposing (Posix)
import Todo exposing (Todo)
import TodoDict exposing (TodoDict)
import TodoForm exposing (TodoForm)
import TodoId exposing (TodoId, TodoIdDict)
import UI exposing (btn2, btnDisabled, checkbox3, col, ipt3, row)



-- ERROR


port logError : String -> Cmd msg



-- ROUTE


type Route
    = RouteInbox
    | RouteToday
    | RouteProject ProjectId
    | RouteNext7Days
    | RouteSearch String


routeEncoder : Route -> Value
routeEncoder route =
    let
        enc1 name =
            enc2 name JE.null

        enc2 name val =
            JE.object [ ( "tag", JE.string name ), ( "val", val ) ]
    in
    case route of
        RouteInbox ->
            enc1 "RouteInbox"

        RouteToday ->
            enc1 "RouteToday"

        RouteProject projectId ->
            enc2 "RouteProject" (ProjectId.encoder projectId)

        RouteNext7Days ->
            enc1 "RouteNext7Days"

        RouteSearch query ->
            enc2 "RouteSearch" (JE.string query)


routeDecoder : Decoder Route
routeDecoder =
    let
        decoderFromTag tag =
            case tag of
                "RouteInbox" ->
                    JD.succeed RouteInbox

                "RouteToday" ->
                    JD.succeed RouteToday

                "RouteProject" ->
                    ProjectId.decoder |> JD.map RouteProject

                "RouteNext7Days" ->
                    JD.succeed RouteNext7Days

                "RouteSearch" ->
                    JD.string |> JD.map RouteSearch

                _ ->
                    JD.fail <| "Invalid route tag: " ++ tag
    in
    JD.field "tag" JD.string |> JD.andThen (JD.field "val" << decoderFromTag)



-- CACHE


port setCache : String -> Cmd msg


type alias Cache =
    { todoList : List Todo
    , projectList : List Project
    , route : Route
    }


defaultCache : Cache
defaultCache =
    { todoList = []
    , projectList = []
    , route = RouteInbox
    }


cacheDecoder : JD.Decoder Cache
cacheDecoder =
    JD.succeed Cache
        |> optional "todoList" (JD.list Todo.decoder) defaultCache.todoList
        |> optional "projectList" (JD.list Project.decoder) defaultCache.projectList
        |> optional "route" routeDecoder defaultCache.route


cacheModel_ : Model -> Cmd msg
cacheModel_ model =
    let
        modelEncoder : Model -> Value
        modelEncoder m =
            object
                [ ( "todoList", JE.list Todo.encoder (TaggedDict.values m.todoDict) )
                , ( "projectList", JE.list Project.encoder m.projectList )
                , ( "route", routeEncoder m.route )
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



-- DND SYSTEM


dndConfig : DnDList.Config Todo
dndConfig =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


dndSystem : DnDList.System Todo Msg
dndSystem =
    DnDList.create dndConfig DnDListMsg



-- MODEL


type alias Flags =
    { cache : Value
    , now : Int
    }


type alias TodoContextMenu =
    { todoId : TodoId
    , schedulePopup : SchedulePopup.Model
    }


type alias Model =
    { dnd : DnDList.Model
    , auth : AuthState
    , todoDict : TodoDict
    , projectList : List Project
    , todoForm : TodoForm
    , maybeTodoContextMenu : Maybe TodoContextMenu
    , route : Route
    , zone : Time.Zone
    , today : Date
    , seed : Random.Seed
    }


defaultModel : Model
defaultModel =
    { dnd = dndSystem.model
    , auth = Auth.Unknown
    , todoDict = defaultCache.todoList |> TodoDict.fromList
    , projectList = defaultCache.projectList
    , todoForm = todoFormSys.model
    , maybeTodoContextMenu = Nothing
    , route = defaultCache.route
    , zone = Time.utc
    , today = Date.fromRataDie 0
    , seed = Random.initialSeed 0
    }


generateMockModel : Random.Seed -> Model
generateMockModel =
    let
        gen : Random.Generator Model
        gen =
            Random.map2
                (\todoList projectList ->
                    { defaultModel
                        | todoDict = TodoDict.fromList todoList
                        , projectList = projectList
                    }
                )
                Todo.mockListGenerator
                Project.mockListGenerator
    in
    Random.step gen
        >> (\( model, seed ) -> { model | seed = seed })


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        cache =
            flags.cache
                |> JD.decodeValue (stringOrValueDecoder cacheDecoder)
                |> Result.withDefault defaultCache

        model : Model
        model =
            { defaultModel
                | todoDict = TodoDict.fromList cache.todoList
                , projectList = cache.projectList
                , route = cache.route
                , seed = Random.initialSeed flags.now
            }
    in
    refreshModel model


refreshModel : Model -> ( Model, Cmd Msg )
refreshModel model =
    ( model, getToday )


getToday =
    Date.today |> Task.perform GotToday


mapTodoDict func model =
    { model | todoDict = func model.todoDict }


setTodoForm form model =
    { model | todoForm = form }



-- UPDATE


type Msg
    = NoOp
    | OnAuthStateChanged Value
    | OnFireTodoList Value
    | SignInClicked
    | SignOutClicked
    | PushAll
    | ClickOutsideDetected
    | DnDListMsg DnDList.Msg
    | OpenTodoContextMenu TodoId
    | TodoContextMenuScheduleMsg SchedulePopup.Msg
    | TodoContextMenuScheduleSaved (Maybe Date)
    | InsertTodoWithPatches (List Todo.Patch) Posix
    | ApplyTodoPatches TodoId (List Todo.Patch) Posix
    | SetTodoCompleted TodoId Bool
    | DeleteTodo TodoId
    | MoveUp TodoId
    | MoveDown TodoId
    | AddTodoOnDueDateClicked Date
    | InsertTodoInProjectAtClicked Int (Maybe ProjectId)
    | EditTodoClicked Todo
    | TodoFormMsg TodoForm.Msg
    | Save (Maybe TodoId) (List Todo.Patch)
    | Cancel
    | ChangeRouteTo Route
    | ResetModel
    | GotToday Date


spSystem : SchedulePopup.System Msg
spSystem =
    SchedulePopup.system
        { toMsg = TodoContextMenuScheduleMsg
        , onSave = TodoContextMenuScheduleSaved
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        SignInClicked ->
            ( model, Auth.signIn () )

        SignOutClicked ->
            ( mapTodoDict (always TaggedDict.empty) model, Auth.signOut () )

        PushAll ->
            ( model, Firestore.pushTodoList (TaggedDict.values model.todoDict) )

        OnFireTodoList value ->
            Firestore.decodeTodoList value
                |> Tuple.mapBoth
                    (TodoDict.upsertNewer >> flip mapTodoDict model)
                    (List.map logError >> Cmd.batch)

        OnAuthStateChanged value ->
            case JD.decodeValue Auth.decoder value of
                Err err ->
                    ( model, logError (JD.errorToString err) )

                Ok auth ->
                    ( { model | auth = auth }, Cmd.none )

        OpenTodoContextMenu todoId ->
            ( { model | maybeTodoContextMenu = Just <| TodoContextMenu todoId spSystem.model }
            , Cmd.none
            )

        TodoContextMenuScheduleMsg msg_ ->
            model.maybeTodoContextMenu
                |> Maybe.map
                    (\{ todoId, schedulePopup } ->
                        let
                            ( sp, spCmd ) =
                                spSystem.update msg_ schedulePopup
                        in
                        ( { model | maybeTodoContextMenu = Just <| TodoContextMenu todoId sp }
                        , spCmd
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        TodoContextMenuScheduleSaved maybeDueDate ->
            model.maybeTodoContextMenu
                |> MX.unwrap ( model, Cmd.none )
                    (\{ todoId } ->
                        ( { model | maybeTodoContextMenu = Nothing }
                        , patchTodo todoId [ Todo.DueDate maybeDueDate ]
                        )
                    )

        ClickOutsideDetected ->
            ( { model | maybeTodoContextMenu = Nothing }, Cmd.none )

        DnDListMsg dndMsg ->
            let
                maybeMaybeProjectId =
                    case model.route of
                        RouteProject projectId ->
                            Just (Just projectId)

                        RouteInbox ->
                            Just Nothing

                        _ ->
                            Nothing
            in
            case maybeMaybeProjectId of
                Just maybeProjectId ->
                    let
                        projectTodoList =
                            sortedTodoListForMaybeProjectId maybeProjectId (TaggedDict.values model.todoDict)

                        ( draggable, newProjectTodoList ) =
                            dndSystem.update dndMsg model.dnd projectTodoList

                        updatedTodoList =
                            newProjectTodoList |> List.indexedMap (\i t -> { t | projectSortIdx = i })
                    in
                    ( { model
                        | dnd = draggable
                        , todoDict = List.foldr (\t -> TaggedDict.insert t.id t) model.todoDict updatedTodoList
                      }
                    , dndSystem.commands model.dnd
                    )

                Nothing ->
                    ( model, Cmd.none )

        ResetModel ->
            refreshModel (generateMockModel model.seed)

        GotToday today ->
            ( { model | today = today }, Cmd.none )

        ChangeRouteTo route ->
            refreshModel { model | route = route, todoForm = todoFormSys.model }

        DeleteTodo todoId ->
            ( model |> mapTodoDict (TaggedDict.filter (\_ -> idEq todoId >> not))
            , Firestore.deleteTodo todoId
            )

        InsertTodoWithPatches patches now ->
            HasSeed.step (Todo.generator now patches) model |> uncurry insertTodo

        ApplyTodoPatches todoId patches now ->
            applyTodoPatchesWithNow todoId now patches model

        MoveUp todoId ->
            ( model, patchTodoProjectSortIdxBy -1 todoId model )

        MoveDown todoId ->
            ( model, patchTodoProjectSortIdxBy 1 todoId model )

        SetTodoCompleted todoId isDone ->
            ( model, patchTodo todoId [ Todo.Completed isDone ] )

        AddTodoOnDueDateClicked dueDate ->
            ( setTodoForm (todoFormSys.initAddForDueDate dueDate) model
            , Cmd.none
            )

        InsertTodoInProjectAtClicked idx maybeProjectId ->
            ( setTodoForm (todoFormSys.initAddForProject maybeProjectId idx) model
            , Cmd.none
            )

        EditTodoClicked todo ->
            ( model |> setTodoForm (todoFormSys.initEdit todo), Cmd.none )

        TodoFormMsg msg ->
            let
                ( tf, c ) =
                    todoFormSys.update msg model.todoForm
            in
            ( model |> setTodoForm tf, c )

        Save maybeTodoId patches ->
            ( model
            , case maybeTodoId of
                Nothing ->
                    Time.now |> Task.perform (InsertTodoWithPatches patches)

                Just todoId ->
                    patchTodo todoId patches
            )

        Cancel ->
            ( model, Cmd.none )


patchTodoProjectSortIdxBy : Int -> TodoId -> Model -> Cmd Msg
patchTodoProjectSortIdxBy offset todoId model =
    TaggedDict.get todoId model.todoDict
        |> Maybe.map (.projectSortIdx >> (+) offset >> Todo.ProjectSortIdx >> List.singleton >> patchTodo todoId)
        |> Maybe.withDefault Cmd.none


patchTodo : TodoId -> List Todo.Patch -> Cmd Msg
patchTodo todoId patches =
    Time.now |> Task.perform (ApplyTodoPatches todoId patches)


sortedTodoListForMaybeProjectId : Maybe ProjectId -> List Todo -> List Todo
sortedTodoListForMaybeProjectId maybeProjectId =
    List.filter (propEq .maybeProjectId maybeProjectId)
        >> List.sortBy .projectSortIdx


applyTodoPatchesWithNow : TodoId -> Posix -> List Todo.Patch -> Model -> ( Model, Cmd Msg )
applyTodoPatchesWithNow todoId now patches model =
    case
        TaggedDict.get todoId model.todoDict
            |> Maybe.andThen (Todo.update now patches)
    of
        Nothing ->
            ( model, Cmd.none )

        Just ( old, new ) ->
            let
                projectTodoList =
                    if old.maybeProjectId /= new.maybeProjectId then
                        (new
                            :: sortedTodoListForMaybeProjectId new.maybeProjectId (TaggedDict.values model.todoDict)
                        )
                            |> List.indexedMap (\idx t -> { t | projectSortIdx = idx })

                    else
                        TaggedDict.values model.todoDict
                            |> sortedTodoListForMaybeProjectId new.maybeProjectId
                            |> updateWhenIdEq new.id (always new)
                            |> LX.swapAt old.projectSortIdx new.projectSortIdx
                            |> List.indexedMap (\idx t -> { t | projectSortIdx = idx })

                todoListWithoutOldTodo =
                    TaggedDict.remove old.id model.todoDict
            in
            ( { model
                | todoDict = List.foldl (\t -> TaggedDict.insert t.id t) todoListWithoutOldTodo projectTodoList
              }
            , Firestore.pushTodoList projectTodoList
            )


insertTodo todo model =
    let
        projectTodoList =
            sortedTodoListForMaybeProjectId todo.maybeProjectId (TaggedDict.values model.todoDict)
                |> LX.splitAt todo.projectSortIdx
                |> (\( l, r ) -> l ++ [ todo ] ++ r)
                |> List.indexedMap (\idx t -> { t | projectSortIdx = idx })
    in
    ( { model
        | todoDict = List.foldl (\t -> TaggedDict.insert t.id t) model.todoDict projectTodoList
      }
    , Firestore.pushTodoList projectTodoList
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dndSystem.subscriptions model.dnd
        , model.maybeTodoContextMenu
            |> MX.unwrap Sub.none
                (\_ ->
                    Browser.Events.onClick <|
                        JD.succeed ClickOutsideDetected
                )
        , Auth.onAuthStateChanged OnAuthStateChanged
        , Firestore.onTodoListReceived OnFireTodoList
        ]


getSearchQuery : Route -> String
getSearchQuery route =
    case route of
        RouteSearch qs ->
            qs

        _ ->
            ""


view : Model -> Html.Html Msg
view model =
    H.toUnstyled <|
        col [ A.class "sans-serif ph4" ]
            [ row [ A.class "bg-black-10 pa1" ]
                [ btn2 "Reset" ResetModel
                , row []
                    [ ipt3 (getSearchQuery model.route)
                        (ChangeRouteTo << RouteSearch)
                        [ A.placeholder "Search" ]
                    ]
                , row [ A.class "flex-grow-1" ] []
                , case model.auth of
                    Auth.Unknown ->
                        row [] [ btnDisabled "SignIn" ]

                    Auth.SignedOut ->
                        row [] [ btn2 "SignIn" SignInClicked ]

                    Auth.SignedIn { displayName } ->
                        row [ A.class "items-center" ]
                            [ col [] [ H.text displayName ]
                            , btn2 "SignOut" SignOutClicked
                            , btn2 "PushAll" PushAll
                            ]
                ]
            , row []
                [ col [ A.class "pa2" ] (viewNav model)
                , col [ A.class "pa2 flex-grow-1 bl b--black-10 measure-wide" ] (viewRoute model model.route)
                ]
            ]


viewNav : Model -> List (H.Html Msg)
viewNav { route, projectList } =
    let
        navItems =
            [ NavInbox
            , NavToday
            , NavNext7Days
            , NavProjects
                (projectList |> List.map NavProject)
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



-- TodoListView


viewRoute : Model -> Route -> List (H.Html Msg)
viewRoute model route =
    let
        viewTodoList : TodoListKind -> List (H.Html Msg)
        viewTodoList kind =
            viewTodoListSection kind model
    in
    case route of
        RouteInbox ->
            viewTodoList (ProjectTodoList <| Nothing)

        RouteProject projectId ->
            viewTodoList (ProjectTodoList <| Just projectId)

        RouteToday ->
            [ OverDueTodoList, DueAtTodoList model.today ]
                |> List.concatMap viewTodoList

        RouteNext7Days ->
            dateRange 0 6 model.today
                |> List.concatMap (\date -> viewTodoList <| DueAtTodoList date)

        RouteSearch query ->
            viewTodoList (SearchResultTodoList query)


type TodoListKind
    = OverDueTodoList
    | DueAtTodoList Date
    | ProjectTodoList (Maybe ProjectId)
    | SearchResultTodoList String


viewTodoListSection :
    TodoListKind
    -> Model
    -> List (H.Html Msg)
viewTodoListSection kind model =
    let
        todoList : List Todo
        todoList =
            todoListFor kind model (TaggedDict.values model.todoDict)

        hideWhenEmpty =
            case kind of
                OverDueTodoList ->
                    True

                DueAtTodoList _ ->
                    False

                ProjectTodoList _ ->
                    False

                SearchResultTodoList _ ->
                    False
    in
    if hideWhenEmpty && todoList == [] then
        []

    else
        viewTodoListTitle kind model
            :: viewTodoListContent kind model todoList


todoListFor : TodoListKind -> { a | today : Date } -> List Todo -> List Todo
todoListFor kind model =
    let
        overDuePred today =
            allPass
                [ propEq .isDone False
                , .maybeDueDate >> MX.unwrap False (\dueDate -> Date.compare dueDate today == LT)
                ]

        dueOnPred dueDate =
            allPass
                [ propEq .isDone False
                , propEq .maybeDueDate (Just dueDate)
                ]

        sortByCreatedAt =
            List.sortBy (.createdAt >> Time.posixToMillis)
    in
    case kind of
        OverDueTodoList ->
            List.filter (overDuePred model.today) >> sortByCreatedAt

        DueAtTodoList dueDate ->
            List.filter (dueOnPred dueDate) >> sortByCreatedAt

        ProjectTodoList maybeProjectId ->
            sortedTodoListForMaybeProjectId maybeProjectId

        SearchResultTodoList query ->
            let
                pred : Todo -> Bool
                pred =
                    if query |> String.isEmpty then
                        always True

                    else
                        .title >> String.contains query
            in
            List.filter pred


viewTodoListTitle : TodoListKind -> Model -> H.Html Msg
viewTodoListTitle kind model =
    case kind of
        OverDueTodoList ->
            col [] [ H.text "OverDue" ]

        DueAtTodoList dueDate ->
            col [ A.class "ph1 pb1 pt3" ] [ H.text <| humanDate model.today dueDate ]

        ProjectTodoList maybeProjectId ->
            col [] [ H.text <| displayProjectTitle model.projectList maybeProjectId ]

        SearchResultTodoList _ ->
            col [] [ H.text "Tasks" ]


viewAddTodoButtonFor : TodoListKind -> List (H.Html Msg)
viewAddTodoButtonFor kind =
    let
        viewAddTodoButton : Msg -> H.Html Msg
        viewAddTodoButton onClick =
            row [ A.class "pa1" ] [ btn2 "add task" onClick ]
    in
    case kind of
        OverDueTodoList ->
            []

        DueAtTodoList dueDate ->
            [ viewAddTodoButton (AddTodoOnDueDateClicked dueDate) ]

        ProjectTodoList maybeProjectId ->
            [ viewAddTodoButton (InsertTodoInProjectAtClicked Random.maxInt maybeProjectId) ]

        SearchResultTodoList _ ->
            []


todoFormSys : TodoForm.System Msg
todoFormSys =
    TodoForm.system { onSave = Save, onCancel = Cancel, toMsg = TodoFormMsg }


viewTodoListContent :
    TodoListKind
    -> Model
    -> List Todo
    -> List (H.Html Msg)
viewTodoListContent kind model todoList =
    let
        form =
            model.todoForm

        viewTodoItem =
            viewTodoListItem kind model

        formHtml =
            todoFormSys.view model.projectList form

        info =
            todoFormSys.info form

        defaultHtmlList =
            List.map viewTodoItem todoList

        htmlListWithAddBtn =
            defaultHtmlList ++ viewAddTodoButtonFor kind

        maybeHtmlListForEdit =
            Maybe.map
                (\editTodoId ->
                    List.map
                        (\todo ->
                            if editTodoId == todo.id then
                                formHtml

                            else
                                viewTodoItem todo
                        )
                        todoList
                )
                info.edit
    in
    case kind of
        OverDueTodoList ->
            maybeHtmlListForEdit
                |> Maybe.withDefault defaultHtmlList

        SearchResultTodoList _ ->
            maybeHtmlListForEdit
                |> Maybe.withDefault defaultHtmlList

        DueAtTodoList dueDate ->
            let
                maybeHtmlListForAdd =
                    Maybe.map (\_ -> appendOne formHtml defaultHtmlList) info.add
            in
            info.initialDueDate
                |> MX.filter ((==) dueDate)
                |> Maybe.andThen
                    (\_ ->
                        maybeHtmlListForAdd
                            |> MX.orElse maybeHtmlListForEdit
                    )
                |> Maybe.withDefault htmlListWithAddBtn

        ProjectTodoList _ ->
            let
                maybeHtmlListForAdd =
                    Maybe.map (\projectSortIdx -> insertAt projectSortIdx formHtml defaultHtmlList) info.add
            in
            maybeHtmlListForAdd
                |> MX.orElse maybeHtmlListForEdit
                |> Maybe.withDefault htmlListWithAddBtn


viewTodoListItem : TodoListKind -> Model -> Todo -> H.Html Msg
viewTodoListItem kind model todo =
    let
        viewDueDateTodoItem =
            row [ A.class "hide-child relative" ]
                [ viewTodoCheckbox todo
                , viewTodoTitle todo
                , viewTodoProjectPill model.projectList todo
                , viewMoreMenuTriggerWithPopup
                ]

        viewMoreMenuTriggerWithPopup =
            viewTodoMoreMenuTriggerWithPopup kind
                todo
                model
    in
    case kind of
        OverDueTodoList ->
            viewDueDateTodoItem

        DueAtTodoList _ ->
            viewDueDateTodoItem

        ProjectTodoList _ ->
            viewProjectTodoItem model viewMoreMenuTriggerWithPopup todo

        SearchResultTodoList _ ->
            let
                viewSearchTodoItem =
                    row [ A.class "hide-child relative" ]
                        [ viewTodoCheckbox todo
                        , viewTodoTitle todo
                        , viewTodoDueDate model.today todo
                        , viewTodoProjectPill model.projectList todo
                        , viewMoreMenuTriggerWithPopup
                        ]
            in
            viewSearchTodoItem


onClickStopPropagation msg =
    E.stopPropagationOn "click" (JD.succeed ( msg, True ))


viewTodoMoreMenuTriggerWithPopup : TodoListKind -> Todo -> Model -> H.Html Msg
viewTodoMoreMenuTriggerWithPopup kind todo model =
    let
        insertAt offset =
            InsertTodoInProjectAtClicked (todo.projectSortIdx + offset) todo.maybeProjectId

        viewItems =
            case kind of
                ProjectTodoList _ ->
                    [ viewMI ( "Edit", EditTodoClicked todo )
                    , viewScheduleMI
                    , viewMI ( "Insert Above", insertAt 0 )
                    , viewMI ( "Insert Below", insertAt 1 )
                    , viewMI ( "Delete", DeleteTodo todo.id )
                    ]

                _ ->
                    [ viewMI ( "Edit", EditTodoClicked todo )
                    , viewScheduleMI
                    , viewMI ( "Delete", DeleteTodo todo.id )
                    ]

        viewScheduleMI =
            model.maybeTodoContextMenu
                |> MX.unwrap spSystem.model .schedulePopup
                |> spSystem.view
                    (\open ->
                        col
                            [ A.class "pa1"
                            , onClickStopPropagation <|
                                open todo.maybeDueDate
                            ]
                            [ H.text "Schedule" ]
                    )

        viewMI ( title, msg ) =
            col [ A.class "pa1", E.onClick msg ] [ H.text title ]
    in
    row [ A.class " relative" ]
        [ model.maybeTodoContextMenu
            |> MX.filter (.todoId >> (==) todo.id)
            |> MX.unwrap (H.text "")
                (\_ ->
                    col
                        [ A.class "absolute right-0 bg-white shadow-1 z-999"
                        , style "min-width" "150px"
                        ]
                        viewItems
                )
        , col
            [ A.class "opacity-transition-none child pointer"
            , onClickStopPropagation (OpenTodoContextMenu todo.id)
            ]
            [ H.text "..." ]
        ]


viewProjectTodoItem model viewContextMenu todo =
    let
        today =
            model.today

        domId =
            TodoId.toString todo.id

        projectSortIdx =
            todo.projectSortIdx

        viewHelp { rootAttrs, handleClass, actionsClass } =
            row
                (A.class "hide-child relative"
                    :: A.id domId
                    :: rootAttrs
                )
                [ row
                    ([ A.class "opacity-transition-none bg-white-90 pointer b code"
                     ]
                        ++ (dndSystem.dragEvents projectSortIdx domId
                                |> List.map A.fromUnstyled
                           )
                        ++ [ A.class handleClass ]
                    )
                    [ H.text "::" ]
                , viewTodoCheckbox todo
                , viewTodoTitle todo
                , viewTodoDueDate today todo
                , viewContextMenu
                ]

        viewDropTarget rootClass =
            viewHelp
                { rootAttrs =
                    [ A.class rootClass ]
                        ++ (dndSystem.dropEvents projectSortIdx domId
                                |> List.map A.fromUnstyled
                           )
                , handleClass = "hidden"
                , actionsClass = "hidden"
                }
    in
    case dndSystem.info model.dnd of
        Nothing ->
            -- viewDraggable
            viewHelp
                { rootAttrs = []
                , handleClass = "child"
                , actionsClass = ""
                }

        Just { dragIndex } ->
            if dragIndex == projectSortIdx then
                col []
                    [ viewDropTarget "o-0"
                    , -- viewGhost
                      viewHelp
                        { rootAttrs =
                            HA.class "z-999"
                                :: dndSystem.ghostStyles model.dnd
                                |> List.map A.fromUnstyled
                        , handleClass = ""
                        , actionsClass = "hidden"
                        }
                    ]

            else
                viewDropTarget ""


viewTodoCheckbox : Todo -> H.Html Msg
viewTodoCheckbox todo =
    row [ A.class "pa1" ]
        [ checkbox3 todo.isDone (SetTodoCompleted todo.id) [ A.class "sz-24" ]
        ]


viewTodoTitle : Todo -> H.Html Msg
viewTodoTitle todo =
    row
        [ A.class "pa1 flex-grow-1"
        , E.onClick (EditTodoClicked todo)
        ]
        [ H.text todo.title ]


displayProjectTitle : List Project -> Maybe ProjectId -> String
displayProjectTitle projectList maybeProjectId =
    projectList
        |> LX.find (.id >> (\id -> Just id == maybeProjectId))
        |> MX.unwrap "Inbox" .title


viewTodoProjectPill projectList todo =
    let
        todoProjectTitle { maybeProjectId } =
            displayProjectTitle projectList maybeProjectId
    in
    row [ A.class "self-start lh-solid pa1 f7 ba br-pill bg-black-10" ]
        [ H.text <| todoProjectTitle todo ]


viewTodoDueDate : Date -> Todo -> H.Html msg
viewTodoDueDate today todo =
    todo.maybeDueDate
        |> MX.unwrap (row [ A.class "self-start pa1 f7 code" ] [ H.text "[]" ])
            (\dueDate ->
                row [ A.class "self-start pa1 f7 code" ] [ H.text (humanDate today dueDate) ]
            )



-- DATE HELPERS


dateRange : Int -> Int -> Date -> List Date
dateRange from to refDate =
    List.range from to
        |> List.map (\ct -> Date.add Date.Days ct refDate)


humanDate : Date -> Date -> String
humanDate today date =
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



-- VIEW HELPERS


main : Program Flags Model Msg
main =
    Browser.element
        { init = \f -> init f |> Return.effect_ cacheModel_
        , update = \msg model -> update msg model |> Return.effect_ cacheModel_
        , view = view
        , subscriptions = subscriptions
        }
