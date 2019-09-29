port module Main exposing (main)

import Basics.More exposing (HasId, allPass, idEq, propEq, updateWhenIdEq, upsertById)
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
import UI exposing (btn2, checkbox3, col, colKeyed, row)


type Route
    = RouteInbox
    | RouteToday
    | RouteProject ProjectId
    | RouteNext7Days



--    | Next7Days
--    | Search
-- PROJECT
-- TODO_
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
    = AddTodoWithDueDateMeta Date
    | AddTodoInMaybeProjectIdMeta Int (Maybe ProjectId)
    | EditTodoMeta Todo


type alias TodoFormWithMeta =
    ( TodoForm, TodoFormMeta )


type alias Model =
    { todoList : List Todo
    , maybeTodoForm : Maybe TodoFormWithMeta
    , route : Route
    , zone : Time.Zone
    , today : Date
    , seed : Random.Seed
    }


defaultModel : Model
defaultModel =
    { todoList = Todo.mockList
    , maybeTodoForm = Nothing

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
    { model | maybeTodoForm = Just form }



-- UPDATE


type Msg
    = NoOp
    | SetTodoIsDone TodoId Bool
    | DeleteTodo TodoId
    | MoveUp TodoId
    | MoveDown TodoId
    | AddTodoOnDueDateClicked Date
    | InsertTodoInMaybeProjectIdClicked Int (Maybe ProjectId)
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
            let
                newModel =
                    LX.find (idEq todoId) model.todoList
                        |> Maybe.map
                            (\todo ->
                                let
                                    projectTodoList =
                                        sortedTodoListForMaybeProjectId todo.maybeProjectId model.todoList

                                    idx =
                                        LX.findIndex (idEq todo.id) projectTodoList |> Maybe.withDefault -1

                                    prevIdx =
                                        idx - 1

                                    updatedProjectTodoList =
                                        LX.swapAt idx prevIdx projectTodoList
                                            |> List.indexedMap (\i t -> { t | projectSortIdx = i })
                                in
                                { model
                                    | todoList = List.foldl upsertById model.todoList updatedProjectTodoList
                                }
                            )
                        |> Maybe.withDefault model
            in
            ( newModel, Cmd.none )

        MoveDown todoId ->
            let
                newModel =
                    LX.find (idEq todoId) model.todoList
                        |> Maybe.map
                            (\todo ->
                                let
                                    projectTodoList =
                                        sortedTodoListForMaybeProjectId todo.maybeProjectId model.todoList

                                    idx =
                                        LX.findIndex (idEq todo.id) projectTodoList |> Maybe.withDefault -1

                                    nextIdx =
                                        idx + 1

                                    updatedProjectTodoList =
                                        LX.swapAt idx nextIdx projectTodoList
                                            |> List.indexedMap (\i t -> { t | projectSortIdx = i })
                                in
                                { model
                                    | todoList = List.foldl upsertById model.todoList updatedProjectTodoList
                                }
                            )
                        |> Maybe.withDefault model
            in
            ( newModel, Cmd.none )

        SetTodoIsDone todoId isDone ->
            ( model |> mapTodoList (updateWhenIdEq todoId (\todo -> { todo | isDone = isDone }))
            , Cmd.none
            )

        AddTodoOnDueDateClicked dueDate ->
            ( model
                |> setTodoForm
                    ( TodoForm.init "" Nothing (Just dueDate)
                    , AddTodoWithDueDateMeta dueDate
                    )
            , Cmd.none
            )

        InsertTodoInMaybeProjectIdClicked idx maybeProjectId ->
            ( model
                |> setTodoForm
                    ( TodoForm.init "" maybeProjectId Nothing
                    , AddTodoInMaybeProjectIdMeta idx maybeProjectId
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


saveTodoForm : TodoFormWithMeta -> Model -> ( Model, Cmd Msg )
saveTodoForm ( form, meta ) model =
    let
        partial =
            TodoForm.toPartial form

        ( todo, newModel ) =
            case meta of
                AddTodoInMaybeProjectIdMeta _ _ ->
                    HasSeed.step (Todo.generatorFromPartial partial) model

                AddTodoWithDueDateMeta _ ->
                    HasSeed.step (Todo.generatorFromPartial partial) model

                EditTodoMeta editingTodo ->
                    ( Todo.patchWithPartial (TodoForm.toPartial form) editingTodo
                    , model
                    )
    in
    ( { newModel | maybeTodoForm = Nothing }
        |> upsertTodoAndUpdateSortIndices todo
    , Cmd.none
    )


sortedTodoListForMaybeProjectId : Maybe ProjectId -> List Todo -> List Todo
sortedTodoListForMaybeProjectId maybeProjectId =
    List.filter (propEq .maybeProjectId maybeProjectId)
        >> List.sortBy .projectSortIdx


upsertTodoAndUpdateSortIndices : Todo -> Model -> Model
upsertTodoAndUpdateSortIndices todo model =
    case LX.find (idEq todo.id) model.todoList of
        Just existingTodo ->
            updateTodo existingTodo todo model

        Nothing ->
            insertTodo todo model


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
            viewTodoListDueTodayWithOverDue model

        RouteProject projectId ->
            viewTodoListForMaybeProjectId (Just projectId) model

        RouteNext7Days ->
            viewNext7DaysTodoList model


getEditTodoFormForTodoId : TodoId -> Maybe ( a, TodoFormMeta ) -> Maybe a
getEditTodoFormForTodoId todoId maybeForm =
    case maybeForm of
        Just ( form, EditTodoMeta { id } ) ->
            if id == todoId then
                Just form

            else
                Nothing

        _ ->
            Nothing


getAddTodoFormWithInitialProjectId : Maybe ProjectId -> Maybe ( a, TodoFormMeta ) -> Maybe ( Int, a )
getAddTodoFormWithInitialProjectId maybeProjectId maybeForm =
    case maybeForm of
        Just ( form, AddTodoInMaybeProjectIdMeta idx maybeProjectId_ ) ->
            if maybeProjectId == maybeProjectId_ then
                Just ( idx, form )

            else
                Nothing

        _ ->
            Nothing


getAddTodoFormWithInitialDueDateEq : Date -> Maybe ( a, TodoFormMeta ) -> Maybe a
getAddTodoFormWithInitialDueDateEq date maybeForm =
    case maybeForm of
        Just ( form, AddTodoWithDueDateMeta date_ ) ->
            if date_ == date then
                Just form

            else
                Nothing

        _ ->
            Nothing


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
    viewOverDueTodoList model ++ viewTodoListDueOn model.today model


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
    if filteredTodoList |> List.isEmpty then
        []

    else
        col [] [ H.text "OverDue" ]
            :: viewEditableTodoList DueDateItemLayout model filteredTodoList


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
        :: viewEditableTodoList DueDateItemLayout model filteredTodoList
        ++ [ getAddTodoFormWithInitialDueDateEq dueDate maybeTodoForm
                |> MX.unwrap (viewAddTodoButton (AddTodoOnDueDateClicked dueDate)) viewTodoForm
           ]


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


viewTodoListForMaybeProjectId : Maybe ProjectId -> Model -> List (H.Html Msg)
viewTodoListForMaybeProjectId maybeProjectId ({ maybeTodoForm, todoList } as model) =
    let
        filteredTodoList =
            sortedTodoListForMaybeProjectId maybeProjectId model.todoList

        maybeAddTodoFormWithIndex =
            getAddTodoFormWithInitialProjectId maybeProjectId maybeTodoForm

        lastIndex =
            List.length filteredTodoList - 1

        isIndexOutOfBounds idx =
            idx < 0 || idx > lastIndex

        isLastIdx idx =
            idx == lastIndex
    in
    --    viewEditableTodoList ProjectItemLayout model filteredTodoList
    --        ++ (case maybeAddTodoFormWithIndex of
    --                Just ( i, form ) ->
    --                    if i == -1 || i >= List.length filteredTodoList then
    --                        [ viewTodoForm form ]
    --
    --                    else
    --                        []
    --
    --                Nothing ->
    --                    [ viewAddTodoButton (AddTodoInMaybeProjectIdClicked -1 maybeProjectId) ]
    --           )
    case maybeAddTodoFormWithIndex of
        Just ( formIdx, form ) ->
            let
                formHtml =
                    ( "viewAddTodoForm", viewTodoForm form )
            in
            viewKeyedEditableTodoItems ProjectItemLayout model filteredTodoList
                |> List.indexedMap
                    (\currentIdx html ->
                        if currentIdx == formIdx then
                            [ formHtml, html ]

                        else if isLastIdx currentIdx && isIndexOutOfBounds formIdx then
                            [ html, formHtml ]

                        else
                            [ html ]
                    )
                |> List.concat
                >> colKeyed []
                >> List.singleton

        Nothing ->
            viewEditableTodoList ProjectItemLayout model filteredTodoList
                ++ [ viewAddTodoButton (InsertTodoInMaybeProjectIdClicked -1 maybeProjectId) ]


viewEditableTodoList : TodoItemLayout -> Model -> List Todo -> List (H.Html Msg)
viewEditableTodoList layout model =
    keyed (.id >> TodoId.toString) (viewEditableTodoItem layout model)
        >> colKeyed []
        >> List.singleton


keyed : (item -> String) -> (item -> html) -> List item -> List ( String, html )
keyed keyF renderF =
    List.map (\item -> ( keyF item, renderF item ))


viewKeyedEditableTodoItems : TodoItemLayout -> Model -> List Todo -> List ( String, H.Html Msg )
viewKeyedEditableTodoItems layout model =
    keyed (.id >> TodoId.toString) (viewEditableTodoItem layout model)


viewEditableTodoItem : TodoItemLayout -> Model -> Todo -> H.Html Msg
viewEditableTodoItem layout { today, maybeTodoForm } todo =
    getEditTodoFormForTodoId todo.id maybeTodoForm
        |> MX.unpack (\_ -> viewTodo today layout todo) viewTodoForm


type TodoItemLayout
    = ProjectItemLayout
    | DueDateItemLayout


todoProjectTitle : { a | maybeProjectId : Maybe ProjectId } -> String
todoProjectTitle { maybeProjectId } =
    Project.mockProjects
        |> LX.find (.id >> (\id -> Just id == maybeProjectId))
        |> MX.unwrap "Inbox" .title


viewTodo : Date -> TodoItemLayout -> Todo -> H.Html Msg
viewTodo today layout todo =
    row [ A.class "hide-child relative" ]
        [ row [ A.class "pa1" ]
            [ checkbox3 todo.isDone (SetTodoIsDone todo.id) [ A.class "sz-24" ]
            ]
        , row
            [ A.class "pa1 flex-grow-1"
            , E.onClick (EditTodoClicked todo)
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
        , row [ A.class "child absolute right-0 bg-white-90" ]
            (case layout of
                ProjectItemLayout ->
                    [ btn2 "UP" (MoveUp todo.id), btn2 "DN" (MoveDown todo.id) ]

                DueDateItemLayout ->
                    []
                        ++ [ btn2 "X" (DeleteTodo todo.id) ]
            )
        ]


viewAddTodoButton : Msg -> H.Html Msg
viewAddTodoButton onClick =
    row [ A.class "pa1" ] [ btn2 "add todo" onClick ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = \f -> init f |> Return.effect_ cacheModel_
        , update = \msg model -> update msg model |> Return.effect_ cacheModel_
        , view = view
        , subscriptions = subscriptions
        }
