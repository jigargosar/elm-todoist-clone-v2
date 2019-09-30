port module Main exposing (main, viewSearchResults)

import Basics.More exposing (HasId, allPass, clampListLength, idEq, propEq, uncurry, updateWhenIdEq, upsertById)
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
import Time
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
            enc1 "RouteInbox"

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
    | SetTodoIsDone TodoId Bool
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

        MoveUp todoId ->
            ( updateTodoWithIdBy todoId (\todo -> { todo | projectSortIdx = todo.projectSortIdx - 1 }) model
            , Cmd.none
            )

        MoveDown todoId ->
            ( updateTodoWithIdBy todoId (\todo -> { todo | projectSortIdx = todo.projectSortIdx + 1 }) model
            , Cmd.none
            )

        SetTodoIsDone todoId isDone ->
            ( model |> mapTodoList (updateWhenIdEq todoId (\todo -> { todo | isDone = isDone }))
            , Cmd.none
            )

        AddTodoOnDueDateClicked dueDate ->
            ( model
                |> setTodoForm
                    (TodoForm.initAdd (\d -> { d | maybeDueDate = Just dueDate }))
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


saveTodoForm : TodoForm -> Model -> ( Model, Cmd Msg )
saveTodoForm form model =
    let
        ( meta, partial ) =
            TodoForm.toPartialWithMeta form

        newModel =
            case meta of
                TodoForm.Add ->
                    HasSeed.step (Todo.generatorFromPartial partial) model
                        |> uncurry insertTodo

                TodoForm.Edit todoId ->
                    updateTodoWithIdBy todoId (Todo.patchWithPartial partial) model
    in
    ( { newModel | maybeTodoForm = Nothing }
    , Cmd.none
    )


sortedTodoListForMaybeProjectId : Maybe ProjectId -> List Todo -> List Todo
sortedTodoListForMaybeProjectId maybeProjectId =
    List.filter (propEq .maybeProjectId maybeProjectId)
        >> List.sortBy .projectSortIdx


updateTodoWithIdBy : TodoId -> (Todo -> Todo) -> Model -> Model
updateTodoWithIdBy id func model =
    LX.find (idEq id) model.todoList
        |> Maybe.map
            (\todo ->
                updateTodo todo (func todo) model
            )
        |> Maybe.withDefault model


updateTodo : Todo -> Todo -> Model -> Model
updateTodo oldTodo newTodo model =
    let
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
                |> LX.splitAt (todo.projectSortIdx |> Debug.log "projectSortIdx")
                |> (\( l, r ) -> l ++ [ todo ] ++ r)
                |> List.indexedMap (\idx t -> { t | projectSortIdx = idx })
    in
    { model
        | todoList = List.foldl upsertById model.todoList projectTodoList
    }



--    ( model, Cmd.none )


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


viewRoute : Model -> Route -> List (H.Html Msg)
viewRoute model route =
    case route of
        RouteInbox ->
            viewTodoListForMaybeProjectId Nothing model

        RouteToday ->
            viewDueTodayAndOverdueTodoList model

        RouteProject projectId ->
            viewTodoListForMaybeProjectId (Just projectId) model

        RouteNext7Days ->
            viewNext7DaysTodoList model

        RouteSearch query ->
            viewSearchResults query model



-- VIEW DUE_DATE TODO_ROUTES


viewNext7DaysTodoList : Model -> List (H.Html Msg)
viewNext7DaysTodoList model =
    let
        dateRange : Int -> Int -> List Date
        dateRange from to =
            List.range from to
                |> List.map (\ct -> Date.add Date.Days ct model.today)
    in
    dateRange 0 6 |> List.concatMap (\date -> viewTodoListDueOn date model)


viewDueTodayAndOverdueTodoList model =
    viewOverDueTodoList model ++ viewTodoListDueOn model.today model


overDuePred today =
    allPass
        [ propEq .isDone False
        , .maybeDueDate >> MX.unwrap False (\dueDate -> Date.compare dueDate today == LT)
        ]


dueOnPred dueDate =
    allPass
        [ propEq .maybeDueDate (Just dueDate)
        , propEq .isDone False
        ]


unlessEmpty func list =
    if List.isEmpty list then
        []

    else
        func list


viewOverDueTodoList : Model -> List (H.Html Msg)
viewOverDueTodoList model =
    unlessEmpty
        (\todoList ->
            col [] [ H.text "OverDue" ] :: viewEditableTodoList viewDueDateTodoItem model todoList
        )
        (filterTodoList (overDuePred model.today) model)


filterTodoList : (Todo -> Bool) -> { b | todoList : List Todo } -> List Todo
filterTodoList pred =
    .todoList >> List.filter pred


viewTodoListDueOn : Date -> Model -> List (H.Html Msg)
viewTodoListDueOn dueDate model =
    let
        titleHtml =
            col [ A.class "ph1 pb1 pt3" ] [ H.text <| humanDate model dueDate ]

        contentHtml =
            viewEditableTodoList viewDueDateTodoItem
                model
                (filterTodoList (dueOnPred dueDate) model)

        footerHtml =
            model.maybeTodoForm
                |> MX.filter (TodoForm.isAddingForInitialDueDate dueDate)
                >> MX.unpack (\_ -> viewAddTodoButton (AddTodoOnDueDateClicked dueDate))
                    (viewTodoForm model.projectList)
    in
    [ titleHtml ] ++ contentHtml ++ [ footerHtml ]


unpackAddTodoFormForDueDate dueDate viewOtherwise viewForm maybeTodoForm =
    maybeTodoForm
        |> MX.filter (TodoForm.isAddingForInitialDueDate dueDate)
        >> MX.unpack viewOtherwise viewForm


viewDueDateTodoItem : { a | projectList : List Project } -> Todo -> H.Html Msg
viewDueDateTodoItem model todo =
    row [ A.class "hide-child relative" ]
        [ viewTodoCheckbox todo
        , viewTodoTitle todo
        , viewTodoProjectPill model todo
        , row [ A.class "child absolute right-0 bg-white-90" ]
            [ btn2 "X" (DeleteTodo todo.id) ]
        ]



-- VIEW PROJECT_TODO_LIST ROUTE


viewTodoListForMaybeProjectId : Maybe ProjectId -> Model -> List (H.Html Msg)
viewTodoListForMaybeProjectId maybeProjectId model =
    let
        filteredTodoList =
            sortedTodoListForMaybeProjectId maybeProjectId model.todoList

        viewTodoItem : Todo -> H.Html Msg
        viewTodoItem =
            viewProjectTodoItem model
    in
    case model.maybeTodoForm of
        Just form ->
            case TodoForm.getMeta form of
                TodoForm.Add ->
                    let
                        formHtml =
                            viewTodoForm model.projectList form

                        formIdx =
                            clampListLength filteredTodoList (TodoForm.getProjectSortIdx form)
                    in
                    LX.splitAt formIdx filteredTodoList
                        |> (\( l, r ) ->
                                List.map viewTodoItem l
                                    ++ [ formHtml ]
                                    ++ List.map viewTodoItem r
                           )

                TodoForm.Edit _ ->
                    viewEditableTodoList viewProjectTodoItem model filteredTodoList

        Nothing ->
            List.map viewTodoItem filteredTodoList
                ++ [ viewAddTodoButton (InsertTodoInProjectAtClicked Random.maxInt maybeProjectId) ]


viewProjectTodoItem : { a | today : Date } -> Todo -> H.Html Msg
viewProjectTodoItem model todo =
    row [ A.class "hide-child relative" ]
        [ viewTodoCheckbox todo
        , viewTodoTitle todo
        , viewTodoDueDate model todo
        , row [ A.class "child absolute right-0 bg-white-90" ]
            [ btn2 "Insert Above" (InsertTodoInProjectAtClicked todo.projectSortIdx todo.maybeProjectId)
            , btn2 "Insert Below" (InsertTodoInProjectAtClicked (todo.projectSortIdx + 1) todo.maybeProjectId)
            , btn2 "UP" (MoveUp todo.id)
            , btn2 "DN" (MoveDown todo.id)
            , btn2 "X" (DeleteTodo todo.id)
            ]
        ]



-- VIEW SEARCH ROUTE


viewSearchResults : String -> Model -> List (H.Html Msg)
viewSearchResults query model =
    let
        filteredTodoList =
            model.todoList |> List.filter pred

        filteredProjects =
            model.projectList
                |> List.filter pred

        viewProject { title } =
            col [ A.class "pv1 ph2" ] [ H.text title ]

        pred : { a | title : String } -> Bool
        pred =
            if query |> String.isEmpty then
                always True

            else
                .title >> String.contains query
    in
    (col [] [ H.text "Tasks" ]
        :: viewEditableTodoList viewSearchTodoItem model filteredTodoList
    )
        ++ (col [ A.class "pt3 pb1" ] [ H.text "Projects" ] :: List.map viewProject filteredProjects)


viewSearchTodoItem : { a | today : Date, projectList : List Project } -> Todo -> H.Html Msg
viewSearchTodoItem model todo =
    row [ A.class "hide-child relative" ]
        [ viewTodoCheckbox todo
        , viewTodoTitle todo
        , viewTodoDueDate model todo
        , viewTodoProjectPill model todo
        , row [ A.class "child absolute right-0 bg-white-90" ]
            [ btn2 "X" (DeleteTodo todo.id) ]
        ]



-- VIEW TODO_FORM HELPERS


viewEditTodoFormOr : (Model -> Todo -> H.Html Msg) -> Model -> Todo -> H.Html Msg
viewEditTodoFormOr viewFunc model todo =
    model.maybeTodoForm
        |> MX.filter (TodoForm.isEditingFor todo.id)
        |> MX.unpack (\_ -> viewFunc model todo) (viewTodoForm model.projectList)


viewEditableTodoList : (Model -> Todo -> H.Html Msg) -> Model -> List Todo -> List (H.Html Msg)
viewEditableTodoList viewFunc model todoList =
    List.map (viewEditTodoFormOr viewFunc model) todoList


todoFormConfig : TodoForm.Config Msg
todoFormConfig =
    TodoForm.createConfig { onSave = Save, onCancel = Cancel, toMsg = PatchTodoForm }


viewTodoForm : List Project -> TodoForm -> H.Html Msg
viewTodoForm =
    TodoForm.viewTodoForm todoFormConfig



-- VIEW TODO_ITEM HELPERS


viewTodoCheckbox : Todo -> H.Html Msg
viewTodoCheckbox todo =
    row [ A.class "pa1" ]
        [ checkbox3 todo.isDone (SetTodoIsDone todo.id) [ A.class "sz-24" ]
        ]


viewTodoTitle : Todo -> H.Html Msg
viewTodoTitle todo =
    row
        [ A.class "pa1 flex-grow-1"
        , E.onClick (EditTodoClicked todo)
        ]
        [ H.text todo.title ]


viewTodoProjectPill { projectList } todo =
    let
        todoProjectTitle { maybeProjectId } =
            projectList
                |> LX.find (.id >> (\id -> Just id == maybeProjectId))
                |> MX.unwrap "Inbox" .title
    in
    row [ A.class "self-start lh-solid pa1 f7 ba br-pill bg-black-10" ]
        [ H.text <| todoProjectTitle todo ]


viewTodoDueDate : { a | today : Date } -> Todo -> H.Html msg
viewTodoDueDate model todo =
    todo.maybeDueDate
        |> MX.unwrap (row [ A.class "self-start pa1 f7 code" ] [ H.text "[]" ])
            (\dueDate ->
                row [ A.class "self-start pa1 f7 code" ] [ H.text (humanDate model dueDate) ]
            )


humanDate : { a | today : Date } -> Date -> String
humanDate { today } date =
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


viewAddTodoButton : Msg -> H.Html Msg
viewAddTodoButton onClick =
    row [ A.class "pa1" ] [ btn2 "add task" onClick ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = \f -> init f |> Return.effect_ cacheModel_
        , update = \msg model -> update msg model |> Return.effect_ cacheModel_
        , view = view
        , subscriptions = subscriptions
        }
