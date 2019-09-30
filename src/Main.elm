port module Main exposing (main)

import Basics.More exposing (HasId, allPass, idEq, propEq, uncurry, updateWhenIdEq, upsertById)
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
import UI exposing (btn2, checkbox3, col, row)


type Route
    = RouteInbox
    | RouteToday
    | RouteProject ProjectId
    | RouteNext7Days



-- CACHE


port setCache : String -> Cmd msg


type alias Cache =
    { todoList : List Todo
    }


defaultCacheValue : Cache
defaultCacheValue =
    { todoList = Todo.mockList }


cacheDecoder : JD.Decoder Cache
cacheDecoder =
    JD.succeed Cache
        |> optional "todoList" (JD.list Todo.decoder) Todo.mockList


cacheModel_ : Model -> Cmd msg
cacheModel_ model =
    let
        modelEncoder : Model -> Value
        modelEncoder { todoList } =
            object
                [ ( "todoList", JE.list Todo.encoder todoList )
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


type TodoFormMeta
    = AddDueAtTodoMeta Date
    | InsertTodoInProjectMeta Int
    | EditTodoMeta Todo


type alias TodoFormWithMeta =
    ( TodoForm, TodoFormMeta )


type alias Model =
    { todoList : List Todo
    , maybeTodoFormWithMeta : Maybe TodoFormWithMeta
    , route : Route
    , zone : Time.Zone
    , today : Date
    , seed : Random.Seed
    }


defaultModel : Model
defaultModel =
    { todoList = Todo.mockList
    , maybeTodoFormWithMeta = Nothing

    --    , route = RouteProject (ProjectId "1")
    , route = RouteNext7Days
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
            { defaultModel | todoList = cache.todoList, seed = Random.initialSeed flags.now }
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
    { model | maybeTodoFormWithMeta = Just form }



-- UPDATE


type Msg
    = NoOp
    | SetTodoIsDone TodoId Bool
    | DeleteTodo TodoId
    | MoveUp TodoId
    | MoveDown TodoId
    | AddTodoOnDueDateClicked Date
    | InsertTodoInProjectClicked Int (Maybe ProjectId)
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
            refreshModel { model | route = route, maybeTodoFormWithMeta = Nothing }

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
                    ( TodoForm.init "" Nothing (Just dueDate)
                    , AddDueAtTodoMeta dueDate
                    )
            , Cmd.none
            )

        InsertTodoInProjectClicked idx maybeProjectId ->
            ( model
                |> setTodoForm
                    ( getInsertTodoInProjectForm model.maybeTodoFormWithMeta
                        |> Maybe.withDefault (TodoForm.init "" maybeProjectId Nothing)
                    , InsertTodoInProjectMeta idx
                    )
            , Cmd.none
            )

        EditTodoClicked ({ id, title, maybeProjectId, maybeDueDate } as todo) ->
            ( model
                |> setTodoForm
                    ( TodoForm.init title maybeProjectId maybeDueDate
                    , EditTodoMeta todo
                    )
            , Cmd.none
            )

        PatchTodoForm todoForm ->
            ( model |> mapTodoForm (Tuple.mapFirst (always todoForm))
            , Cmd.none
            )

        Save ->
            case model.maybeTodoFormWithMeta of
                Just form ->
                    saveTodoForm form model

                Nothing ->
                    ( model, Cmd.none )

        Cancel ->
            ( { model | maybeTodoFormWithMeta = Nothing }
            , Cmd.none
            )


mapTodoForm : (a -> a) -> { b | maybeTodoFormWithMeta : Maybe a } -> { b | maybeTodoFormWithMeta : Maybe a }
mapTodoForm func model =
    { model | maybeTodoFormWithMeta = model.maybeTodoFormWithMeta |> Maybe.map func }


saveTodoForm : TodoFormWithMeta -> Model -> ( Model, Cmd Msg )
saveTodoForm ( form, meta ) model =
    let
        partial =
            TodoForm.toPartial form

        newModel =
            case meta of
                InsertTodoInProjectMeta idx ->
                    HasSeed.step (Todo.generatorFromPartial partial) model
                        |> Tuple.mapFirst (\t -> { t | projectSortIdx = idx })
                        |> uncurry insertTodo

                AddDueAtTodoMeta _ ->
                    HasSeed.step (Todo.generatorFromPartial partial) model
                        |> uncurry insertTodo

                EditTodoMeta { id } ->
                    updateTodoWithIdBy id (Todo.patchWithPartial (TodoForm.toPartial form)) model
    in
    ( { newModel | maybeTodoFormWithMeta = Nothing }
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
                |> (::) todo
                |> List.indexedMap (\idx t -> { t | projectSortIdx = idx })
    in
    { model
        | todoList = List.foldl upsertById model.todoList projectTodoList
    }



--    ( model, Cmd.none )


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


getInsertTodoInProjectForm : Maybe ( a, TodoFormMeta ) -> Maybe a
getInsertTodoInProjectForm maybeForm =
    case maybeForm of
        Just ( form, InsertTodoInProjectMeta _ ) ->
            Just form

        _ ->
            Nothing



-- DUE DATE TODO_LIST VIEWS


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
viewOverDueTodoList ({ today, todoList, maybeTodoFormWithMeta } as model) =
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
            :: viewEditableTodoList model filteredTodoList


viewTodoListDueOn : Date -> Model -> List (H.Html Msg)
viewTodoListDueOn dueDate ({ today, todoList, maybeTodoFormWithMeta } as model) =
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
        :: viewEditableTodoList model filteredTodoList
        ++ [ case maybeTodoFormWithMeta of
                Just ( form, AddDueAtTodoMeta date_ ) ->
                    if date_ == dueDate then
                        viewTodoForm form

                    else
                        addButtonHtml

                _ ->
                    addButtonHtml
           ]


viewEditableTodoList : Model -> List Todo -> List (H.Html Msg)
viewEditableTodoList { maybeTodoFormWithMeta } =
    List.map
        (\todo ->
            case maybeTodoFormWithMeta of
                Just ( form, EditTodoMeta { id } ) ->
                    if id == todo.id then
                        viewTodoForm form

                    else
                        viewDueDateTodoItem todo

                _ ->
                    viewDueDateTodoItem todo
        )


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



-- PROJECT TODO_LIST VIEWS


viewTodoListForMaybeProjectId : Maybe ProjectId -> Model -> List (H.Html Msg)
viewTodoListForMaybeProjectId maybeProjectId ({ maybeTodoFormWithMeta, todoList } as model) =
    let
        filteredTodoList =
            sortedTodoListForMaybeProjectId maybeProjectId model.todoList

        viewTodoItem : Int -> Todo -> H.Html Msg
        viewTodoItem =
            viewProjectTodoItem maybeProjectId model.today
    in
    case maybeTodoFormWithMeta of
        Just ( form, InsertTodoInProjectMeta formIdx ) ->
            let
                formHtml =
                    viewTodoForm form

                lastIndex =
                    List.length filteredTodoList - 1

                isFormIndexOutOfBounds =
                    formIdx < 0 || formIdx > lastIndex

                isLastIdx idx =
                    idx == lastIndex
            in
            filteredTodoList
                |> List.indexedMap
                    (\currentIdx todo ->
                        let
                            todoItemHtml =
                                viewTodoItem currentIdx todo
                        in
                        if currentIdx == formIdx then
                            [ formHtml, todoItemHtml ]

                        else if isLastIdx currentIdx && isFormIndexOutOfBounds then
                            [ todoItemHtml, formHtml ]

                        else
                            [ todoItemHtml ]
                    )
                |> List.concat

        Just ( form, EditTodoMeta editTodo ) ->
            filteredTodoList
                |> List.indexedMap
                    (\currentIdx todo ->
                        if editTodo.id == todo.id then
                            viewTodoForm form

                        else
                            viewTodoItem currentIdx todo
                    )

        _ ->
            List.indexedMap viewTodoItem filteredTodoList
                ++ [ viewAddTodoButton (InsertTodoInProjectClicked -1 maybeProjectId) ]


viewProjectTodoItem : Maybe ProjectId -> Date -> Int -> Todo -> H.Html Msg
viewProjectTodoItem maybeProject today idx todo =
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
            [ btn2 "Insert Above" (InsertTodoInProjectClicked idx maybeProject)
            , btn2 "Insert Below" (InsertTodoInProjectClicked (idx + 1) maybeProject)
            , btn2 "UP" (MoveUp todo.id)
            , btn2 "DN" (MoveDown todo.id)
            , btn2 "X" (DeleteTodo todo.id)
            ]
        ]


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
