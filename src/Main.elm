port module Main exposing (main)

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
import UI exposing (btn2, checkbox3, col, ipt2, ipt3, row)


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
    , route : Route
    }


defaultCacheValue : Cache
defaultCacheValue =
    { todoList = Todo.mockList, route = RouteInbox }


cacheDecoder : JD.Decoder Cache
cacheDecoder =
    JD.succeed Cache
        |> optional "todoList" (JD.list Todo.decoder) defaultCacheValue.todoList
        |> optional "route" routeDecoder defaultCacheValue.route


cacheModel_ : Model -> Cmd msg
cacheModel_ model =
    let
        modelEncoder : Model -> Value
        modelEncoder { todoList, route } =
            object
                [ ( "todoList", JE.list Todo.encoder todoList )
                , ( "route", routeEncoder route )
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
    , maybeTodoForm : Maybe TodoForm
    , route : Route
    , zone : Time.Zone
    , today : Date
    , seed : Random.Seed
    }


defaultModel : Model
defaultModel =
    { todoList = defaultCacheValue.todoList
    , maybeTodoForm = Nothing
    , route = defaultCacheValue.route
    , zone = Time.utc
    , today = Date.fromRataDie 0
    , seed = Random.initialSeed 0
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
            { defaultModel
                | todoList = cache.todoList
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
    | AddTodoInProjectAtClicked Int (Maybe ProjectId)
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
            refreshModel defaultModel

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

        AddTodoInProjectAtClicked idx maybeProjectId ->
            ( model
                |> setTodoForm
                    (model.maybeTodoForm
                        |> Maybe.map (TodoForm.setProjectSortIdx idx)
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
            ( model
                |> setTodoForm (TodoForm.initEdit todo)
            , Cmd.none
            )

        PatchTodoForm todoForm ->
            ( model |> mapTodoForm (always todoForm)
            , Cmd.none
            )

        Save ->
            case model.maybeTodoForm of
                Just form ->
                    saveTodoForm form model

                Nothing ->
                    ( model, Cmd.none )

        Cancel ->
            ( { model | maybeTodoForm = Nothing }
            , Cmd.none
            )


mapTodoForm : (a -> a) -> { b | maybeTodoForm : Maybe a } -> { b | maybeTodoForm : Maybe a }
mapTodoForm func model =
    { model | maybeTodoForm = model.maybeTodoForm |> Maybe.map func }


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
viewNav { route } =
    let
        navItems =
            [ NavInbox
            , NavToday
            , NavNext7Days
            , NavProjects
                (Project.mockProjects |> List.map NavProject)
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
            let
                pred : { a | title : String } -> Bool
                pred =
                    if query |> String.isEmpty then
                        always True

                    else
                        .title >> String.contains query

                filtered =
                    model.todoList |> List.filter pred

                viewItem =
                    viewSearchTodoItem model.today
            in
            [ col []
                (List.map
                    (\todo ->
                        case model.maybeTodoForm of
                            Just form ->
                                if TodoForm.isEditingFor todo.id form then
                                    viewTodoForm form

                                else
                                    viewItem todo

                            _ ->
                                viewItem todo
                    )
                    filtered
                )
            ]



-- VIEW DUE_DATE TODO_LIST


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


viewOverDueTodoList : Model -> List (H.Html Msg)
viewOverDueTodoList { today, todoList, maybeTodoForm } =
    let
        filterPredicate =
            allPass
                [ .maybeDueDate >> MX.unwrap False (\dueDate -> Date.compare dueDate today == LT)
                , propEq .isDone False
                ]

        filteredTodoList =
            todoList |> List.filter filterPredicate
    in
    if filteredTodoList |> List.isEmpty then
        []

    else
        col [] [ H.text "OverDue" ]
            :: List.map (viewEditingFormForTodoOr viewDueDateTodoItem maybeTodoForm) filteredTodoList


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

        addButtonHtml =
            viewAddTodoButton (AddTodoOnDueDateClicked dueDate)
    in
    col [ A.class "ph1 pb1 pt3" ] [ H.text <| humanDate dueDate today ]
        :: List.map (viewEditingFormForTodoOr viewDueDateTodoItem maybeTodoForm) filteredTodoList
        ++ [ viewAddTodoFormForInitialDueDate dueDate maybeTodoForm
                |> Maybe.withDefault addButtonHtml
           ]


viewAddTodoFormForInitialDueDate : Date -> Maybe TodoForm -> Maybe (H.Html Msg)
viewAddTodoFormForInitialDueDate dueDate =
    MX.filter (TodoForm.isAddingForInitialDueDate dueDate)
        >> Maybe.map viewTodoForm


viewEditFormForTodoId : TodoId -> Maybe TodoForm -> Maybe (H.Html Msg)
viewEditFormForTodoId todoId =
    MX.filter (TodoForm.isEditingFor todoId)
        >> Maybe.map viewTodoForm


viewEditingFormForTodoOr viewFunc maybeTodoForm todo =
    maybeTodoForm
        |> viewEditFormForTodoId todo.id
        |> Maybe.withDefault (viewFunc todo)


viewDueDateTodoItem : Todo -> H.Html Msg
viewDueDateTodoItem todo =
    row [ A.class "hide-child relative" ]
        [ row [ A.class "pa1" ]
            [ checkbox3 todo.isDone (SetTodoIsDone todo.id) [ A.class "sz-24" ]
            ]
        , row
            [ A.class "pa1 flex-grow-1"
            , E.onClick (EditTodoClicked todo)
            ]
            [ H.text todo.title ]
        , row [ A.class "self-start lh-solid pa1 f7 ba br-pill bg-black-10" ]
            [ H.text <| todoProjectTitle todo ]
        , row [ A.class "child absolute right-0 bg-white-90" ]
            [ btn2 "X" (DeleteTodo todo.id) ]
        ]



-- VIEW PROJECT TODO_LIST


viewTodoListForMaybeProjectId : Maybe ProjectId -> Model -> List (H.Html Msg)
viewTodoListForMaybeProjectId maybeProjectId ({ maybeTodoForm, todoList } as model) =
    let
        filteredTodoList =
            sortedTodoListForMaybeProjectId maybeProjectId model.todoList

        viewTodoItem : Todo -> H.Html Msg
        viewTodoItem =
            viewProjectTodoItem model.today
    in
    case maybeTodoForm of
        Just form ->
            case TodoForm.getMeta form of
                TodoForm.Add ->
                    let
                        formHtml =
                            viewTodoForm form

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
                    List.map (viewEditingFormForTodoOr viewTodoItem maybeTodoForm)
                        filteredTodoList

        Nothing ->
            List.map viewTodoItem filteredTodoList
                ++ [ viewAddTodoButton (AddTodoInProjectAtClicked Random.maxInt maybeProjectId) ]


viewProjectTodoItem : Date -> Todo -> H.Html Msg
viewProjectTodoItem today todo =
    row [ A.class "hide-child relative" ]
        [ row [ A.class "pa1" ]
            [ checkbox3 todo.isDone (SetTodoIsDone todo.id) [ A.class "sz-24" ]
            ]
        , row
            [ A.class "pa1 flex-grow-1"
            , E.onClick (EditTodoClicked todo)
            ]
            [ H.text todo.title ]
        , todo.maybeDueDate
            |> MX.unwrap (row [ A.class "self-start pa1 f7 code" ] [ H.text "[]" ])
                (\dueDate ->
                    row [ A.class "self-start pa1 f7 code" ] [ H.text (humanDate dueDate today) ]
                )
        , row [ A.class "child absolute right-0 bg-white-90" ]
            [ btn2 "Insert Above" (AddTodoInProjectAtClicked todo.projectSortIdx todo.maybeProjectId)
            , btn2 "Insert Below" (AddTodoInProjectAtClicked (todo.projectSortIdx + 1) todo.maybeProjectId)
            , btn2 "UP" (MoveUp todo.id)
            , btn2 "DN" (MoveDown todo.id)
            , btn2 "X" (DeleteTodo todo.id)
            ]
        ]


viewSearchTodoItem : Date -> Todo -> H.Html Msg
viewSearchTodoItem today todo =
    row [ A.class "hide-child relative" ]
        [ row [ A.class "pa1" ]
            [ checkbox3 todo.isDone (SetTodoIsDone todo.id) [ A.class "sz-24" ]
            ]
        , row
            [ A.class "pa1 flex-grow-1"
            , E.onClick (EditTodoClicked todo)
            ]
            [ H.text todo.title ]
        , todo.maybeDueDate
            |> MX.unwrap (row [ A.class "self-start pa1 f7 code" ] [ H.text "[]" ])
                (\dueDate ->
                    row [ A.class "self-start pa1 f7 code" ] [ H.text (humanDate dueDate today) ]
                )
        , row [ A.class "self-start lh-solid pa1 f7 ba br-pill bg-black-10" ]
            [ H.text <| todoProjectTitle todo ]
        , row [ A.class "child absolute right-0 bg-white-90" ]
            [ btn2 "X" (DeleteTodo todo.id) ]
        ]



-- VIEW HELPERS


todoProjectTitle : { a | maybeProjectId : Maybe ProjectId } -> String
todoProjectTitle { maybeProjectId } =
    Project.mockProjects
        |> LX.find (.id >> (\id -> Just id == maybeProjectId))
        |> MX.unwrap "Inbox" .title


viewAddTodoButton : Msg -> H.Html Msg
viewAddTodoButton onClick =
    row [ A.class "pa1" ] [ btn2 "add todo" onClick ]


todoFormConfig : TodoForm.Config Msg
todoFormConfig =
    TodoForm.createConfig { onSave = Save, onCancel = Cancel, toMsg = PatchTodoForm }


viewTodoForm : TodoForm -> H.Html Msg
viewTodoForm =
    TodoForm.viewTodoForm todoFormConfig


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


main : Program Flags Model Msg
main =
    Browser.element
        { init = \f -> init f |> Return.effect_ cacheModel_
        , update = \msg model -> update msg model |> Return.effect_ cacheModel_
        , view = view
        , subscriptions = subscriptions
        }
