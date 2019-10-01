port module Main exposing (main)

import Basics.More exposing (HasId, allPass, findById, idEq, ifElse, insertAt, propEq, uncurry, updateWhenIdEq, upsertById)
import Browser
import Date exposing (Date)
import HasSeed
import Html
import Html.Styled as H
import Html.Styled.Attributes as A
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
import Task
import Time exposing (Posix)
import Todo exposing (Todo)
import TodoForm exposing (TodoForm)
import TodoId exposing (TodoId)
import UI exposing (btn2, checkbox3, col, ipt3, row)


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


defaultCacheValue : Cache
defaultCacheValue =
    { todoList = []
    , projectList = []
    , route = RouteInbox
    }


cacheDecoder : JD.Decoder Cache
cacheDecoder =
    JD.succeed Cache
        |> optional "todoList" (JD.list Todo.decoder) defaultCacheValue.todoList
        |> optional "projectList" (JD.list Project.decoder) defaultCacheValue.projectList
        |> optional "route" routeDecoder defaultCacheValue.route


cacheModel_ : Model -> Cmd msg
cacheModel_ model =
    let
        modelEncoder : Model -> Value
        modelEncoder m =
            object
                [ ( "todoList", JE.list Todo.encoder m.todoList )
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



-- MODEL


type alias Flags =
    { cache : Value
    , now : Int
    }


type alias Model =
    { todoList : List Todo
    , projectList : List Project
    , maybeTodoForm : Maybe TodoForm
    , route : Route
    , zone : Time.Zone
    , today : Date
    , seed : Random.Seed
    }


defaultModel : Model
defaultModel =
    { todoList = defaultCacheValue.todoList
    , projectList = defaultCacheValue.projectList
    , maybeTodoForm = Nothing
    , route = defaultCacheValue.route
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
                    { defaultModel | todoList = todoList, projectList = projectList }
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
                |> Result.withDefault defaultCacheValue

        model : Model
        model =
            { defaultModel
                | todoList = cache.todoList
                , projectList = cache.projectList
                , route = cache.route
                , seed = Random.initialSeed flags.now
            }
    in
    refreshModel model
        |> Return.andThen resetIfFirstTime


resetIfFirstTime : Model -> ( Model, Cmd Msg )
resetIfFirstTime model =
    if model.projectList == [] then
        update ResetModel model

    else
        ( model, Cmd.none )


refreshModel : Model -> ( Model, Cmd Msg )
refreshModel model =
    ( model, getToday )


getToday =
    Date.today |> Task.perform GotToday


mapTodoList : (small -> small) -> { big | todoList : small } -> { big | todoList : small }
mapTodoList func model =
    { model | todoList = func model.todoList }


setTodoForm form model =
    { model | maybeTodoForm = Just form }



-- UPDATE


type Msg
    = NoOp
    | InsertNewTodoWithPatches (List Todo.Patch) Posix
    | ApplyTodoPatches TodoId (List Todo.Patch) Posix
    | SetTodoCompleted TodoId Bool
    | DeleteTodo TodoId
    | MoveUp TodoId
    | MoveDown TodoId
    | AddTodoOnDueDateClicked Date
    | InsertTodoInProjectAtClicked Int (Maybe ProjectId)
    | EditTodoClicked Todo
    | PatchTodoForm TodoForm
    | Save
    | Cancel
    | ChangeRouteTo Route
    | ResetModel
    | GotToday Date


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ResetModel ->
            refreshModel (generateMockModel model.seed)

        GotToday today ->
            ( { model | today = today }, Cmd.none )

        ChangeRouteTo route ->
            refreshModel { model | route = route, maybeTodoForm = Nothing }

        DeleteTodo todoId ->
            ( model |> mapTodoList (List.filter (idEq todoId >> not))
            , Cmd.none
            )

        InsertNewTodoWithPatches patches now ->
            ( HasSeed.step (Todo.generator now patches) model |> uncurry insertTodo
            , Cmd.none
            )

        ApplyTodoPatches todoId patches now ->
            ( applyTodoPatchesWithNow todoId now patches model, Cmd.none )

        MoveUp todoId ->
            ( model, patchTodoProjectSortIdxBy -1 todoId model )

        MoveDown todoId ->
            ( model, patchTodoProjectSortIdxBy 1 todoId model )

        SetTodoCompleted todoId isDone ->
            ( model, patchTodo todoId (Todo.Completed isDone) )

        AddTodoOnDueDateClicked dueDate ->
            ( model
                |> setTodoForm
                    (TodoForm.initAdd
                        (\d -> { d | maybeDueDate = Just dueDate })
                    )
            , Cmd.none
            )

        InsertTodoInProjectAtClicked idx maybeProjectId ->
            ( model
                |> setTodoForm
                    (model.maybeTodoForm
                        |> Maybe.andThen (TodoForm.setProjectSortIdxIfAdding idx)
                        |> Maybe.withDefault
                            (TodoForm.initAdd
                                (\d ->
                                    { d
                                        | maybeProjectId = maybeProjectId
                                        , projectSortIdx = idx
                                    }
                                )
                            )
                    )
            , Cmd.none
            )

        EditTodoClicked todo ->
            ( model |> setTodoForm (TodoForm.initEdit todo), Cmd.none )

        PatchTodoForm todoForm ->
            ( model |> setTodoForm todoForm, Cmd.none )

        Save ->
            case model.maybeTodoForm of
                Just form ->
                    saveTodoForm form model

                Nothing ->
                    ( model, Cmd.none )

        Cancel ->
            ( { model | maybeTodoForm = Nothing }, Cmd.none )


patchTodoProjectSortIdxBy : Int -> TodoId -> Model -> Cmd Msg
patchTodoProjectSortIdxBy offset todoId model =
    findById todoId model.todoList
        |> Maybe.map (.projectSortIdx >> (+) offset >> Todo.ProjectSortIdx >> patchTodo todoId)
        |> Maybe.withDefault Cmd.none


patchTodo : TodoId -> Todo.Patch -> Cmd Msg
patchTodo todoId patch =
    applyTodoPatches todoId [ patch ]


applyTodoPatches : TodoId -> List Todo.Patch -> Cmd Msg
applyTodoPatches todoId todoPatches =
    Time.now |> Task.perform (ApplyTodoPatches todoId todoPatches)


saveTodoForm : TodoForm -> Model -> ( Model, Cmd Msg )
saveTodoForm form model =
    let
        ( meta, patches ) =
            TodoForm.toPatchesWithMeta form
    in
    ( { model | maybeTodoForm = Nothing }
    , case meta of
        TodoForm.Add ->
            Time.now |> Task.perform (InsertNewTodoWithPatches patches)

        TodoForm.Edit todoId ->
            applyTodoPatches todoId patches
    )


sortedTodoListForMaybeProjectId : Maybe ProjectId -> List Todo -> List Todo
sortedTodoListForMaybeProjectId maybeProjectId =
    List.filter (propEq .maybeProjectId maybeProjectId)
        >> List.sortBy .projectSortIdx


applyTodoPatchesWithNow : TodoId -> Posix -> List Todo.Patch -> Model -> Model
applyTodoPatchesWithNow todoId now patches model =
    findById todoId model.todoList
        |> Maybe.map
            (patchTodoWithNowHelp now patches model)
        |> Maybe.withDefault model


patchTodoWithNowHelp now patches model oldTodo =
    let
        newTodo =
            Todo.applyPatches now patches oldTodo

        projectTodoList =
            if oldTodo.maybeProjectId /= newTodo.maybeProjectId then
                (newTodo
                    :: sortedTodoListForMaybeProjectId newTodo.maybeProjectId model.todoList
                )
                    |> List.indexedMap (\idx t -> { t | projectSortIdx = idx })

            else
                model.todoList
                    |> sortedTodoListForMaybeProjectId newTodo.maybeProjectId
                    |> updateWhenIdEq newTodo.id (always newTodo)
                    |> LX.swapAt oldTodo.projectSortIdx newTodo.projectSortIdx
                    |> List.indexedMap (\idx t -> { t | projectSortIdx = idx })

        todoListWithoutOldTodo =
            List.filter (idEq oldTodo.id >> not) model.todoList
    in
    { model
        | todoList = List.foldl upsertById todoListWithoutOldTodo projectTodoList
    }


insertTodo todo model =
    let
        projectTodoList =
            sortedTodoListForMaybeProjectId todo.maybeProjectId model.todoList
                |> LX.splitAt todo.projectSortIdx
                |> (\( l, r ) -> l ++ [ todo ] ++ r)
                |> List.indexedMap (\idx t -> { t | projectSortIdx = idx })
    in
    { model
        | todoList = List.foldl upsertById model.todoList projectTodoList
    }


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


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
    -> { a | todoList : List Todo, today : Date, projectList : List Project, maybeTodoForm : Maybe TodoForm }
    -> List (H.Html Msg)
viewTodoListSection kind model =
    let
        todoList : List Todo
        todoList =
            todoListFor kind model model.todoList

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
            :: viewTodoListContent kind model model.maybeTodoForm todoList


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
            List.filter (overDuePred model.today)
                >> sortByCreatedAt

        DueAtTodoList dueDate ->
            List.filter (dueOnPred dueDate)
                >> sortByCreatedAt

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


viewTodoListTitle : TodoListKind -> { a | today : Date, projectList : List Project } -> H.Html Msg
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


todoFormConfig : TodoForm.Config Msg
todoFormConfig =
    TodoForm.createConfig { onSave = Save, onCancel = Cancel, toMsg = PatchTodoForm }


viewTodoListContent :
    TodoListKind
    -> { a | projectList : List Project, today : Date }
    -> Maybe TodoForm
    -> List Todo
    -> List (H.Html Msg)
viewTodoListContent kind model maybeTodoForm todoList =
    let
        viewTodoItem =
            viewTodoListItem kind model

        viewAddBtn =
            viewAddTodoButtonFor kind

        viewForm : TodoForm -> H.Html Msg
        viewForm =
            TodoForm.viewTodoForm todoFormConfig model.projectList

        editFormForTodoId todoId =
            maybeTodoForm
                |> MX.filter (TodoForm.isEditingFor todoId)
    in
    case kind of
        OverDueTodoList ->
            List.map
                (\todo ->
                    editFormForTodoId todo.id
                        |> MX.unpack (\_ -> viewTodoItem todo) viewForm
                )
                todoList

        SearchResultTodoList _ ->
            List.map
                (\todo ->
                    editFormForTodoId todo.id
                        |> MX.unpack (\_ -> viewTodoItem todo) viewForm
                )
                todoList

        DueAtTodoList dueDate ->
            case maybeTodoForm of
                Just form ->
                    let
                        showAddForm =
                            TodoForm.isAddingForInitialDueDate dueDate form
                    in
                    case TodoForm.getMeta form of
                        TodoForm.Add ->
                            List.map viewTodoItem todoList
                                ++ (if showAddForm then
                                        [ viewForm form ]

                                    else
                                        viewAddBtn
                                   )

                        TodoForm.Edit todoId ->
                            List.map
                                (ifElse (idEq todoId) (\_ -> viewForm form) viewTodoItem)
                                todoList

                Nothing ->
                    List.map viewTodoItem todoList ++ viewAddBtn

        ProjectTodoList _ ->
            case maybeTodoForm of
                Just form ->
                    case TodoForm.getMeta form of
                        TodoForm.Add ->
                            List.map viewTodoItem todoList
                                |> insertAt (TodoForm.getProjectSortIdx form) (viewForm form)

                        TodoForm.Edit todoId ->
                            List.map
                                (ifElse (idEq todoId) (\_ -> viewForm form) viewTodoItem)
                                todoList

                Nothing ->
                    List.map viewTodoItem todoList ++ viewAddBtn



-- VIEW TODO_FORM
-- VIEW TODO_LIST_ITEM


viewTodoListItem : TodoListKind -> { a | projectList : List Project, today : Date } -> Todo -> H.Html Msg
viewTodoListItem kind model =
    let
        viewDueDateTodoItem : List Project -> Todo -> H.Html Msg
        viewDueDateTodoItem projectList todo =
            row [ A.class "hide-child relative" ]
                [ viewTodoCheckbox todo
                , viewTodoTitle todo
                , viewTodoProjectPill projectList todo
                , row [ A.class "child absolute right-0 bg-white-90" ]
                    [ btn2 "X" (DeleteTodo todo.id) ]
                ]
    in
    case kind of
        OverDueTodoList ->
            viewDueDateTodoItem model.projectList

        DueAtTodoList _ ->
            viewDueDateTodoItem model.projectList

        ProjectTodoList _ ->
            let
                viewProjectTodoItem : Date -> Todo -> H.Html Msg
                viewProjectTodoItem today todo =
                    row [ A.class "hide-child relative" ]
                        [ viewTodoCheckbox todo
                        , viewTodoTitle todo
                        , viewTodoDueDate today todo
                        , row [ A.class "child absolute right-0 bg-white-90" ]
                            [ btn2 "Insert Above" (InsertTodoInProjectAtClicked todo.projectSortIdx todo.maybeProjectId)
                            , btn2 "Insert Below" (InsertTodoInProjectAtClicked (todo.projectSortIdx + 1) todo.maybeProjectId)
                            , btn2 "UP" (MoveUp todo.id)
                            , btn2 "DN" (MoveDown todo.id)
                            , btn2 "X" (DeleteTodo todo.id)
                            ]
                        ]
            in
            viewProjectTodoItem model.today

        SearchResultTodoList _ ->
            let
                viewSearchTodoItem : Date -> List Project -> Todo -> H.Html Msg
                viewSearchTodoItem today projectList todo =
                    row [ A.class "hide-child relative" ]
                        [ viewTodoCheckbox todo
                        , viewTodoTitle todo
                        , viewTodoDueDate today todo
                        , viewTodoProjectPill projectList todo
                        , row [ A.class "child absolute right-0 bg-white-90" ]
                            [ btn2 "X" (DeleteTodo todo.id) ]
                        ]
            in
            viewSearchTodoItem model.today model.projectList


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
