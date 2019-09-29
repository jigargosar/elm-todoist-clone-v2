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
import Json.Decode.Pipeline exposing (optional, required)
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
import TodoId exposing (TodoId)
import UI exposing (btn2, checkbox3, col, colKeyed, ipt2, row)


type Route
    = RouteInbox
    | RouteToday
    | RouteProject ProjectId
    | RouteNext7Days



--    | Next7Days
--    | Search
-- PROJECT


projectIdToValueAttr : ProjectId -> H.Attribute msg
projectIdToValueAttr =
    A.value << ProjectId.toString



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
    let
        todoDecoder : JD.Decoder Todo
        todoDecoder =
            JD.succeed Todo
                |> required "id" TodoId.decoder
                |> required "title" JD.string
                |> required "isDone" JD.bool
                |> optional "isDeleted" JD.bool False
                |> optional "maybeProjectId"
                    (ProjectId.decoder |> JD.map Just)
                    Nothing
                |> optional "maybeDueDate" (JD.string |> JD.map (Date.fromIsoString >> Result.toMaybe)) Nothing
    in
    JD.succeed Cache
        |> optional "todoList" (JD.list todoDecoder) Todo.mockList


cacheModel_ : Model -> Cmd msg
cacheModel_ model =
    let
        maybeEncoder =
            MX.unwrap JE.null

        todoEncoder : Todo -> Value
        todoEncoder { id, title, isDone, isDeleted, maybeProjectId, maybeDueDate } =
            object
                [ ( "id", TodoId.encoder id )
                , ( "title", JE.string title )
                , ( "isDone", JE.bool isDone )
                , ( "isDeleted", JE.bool isDeleted )
                , ( "maybeProjectId", maybeEncoder ProjectId.encoder maybeProjectId )
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


type alias HasTodoFormFields a =
    { a
        | title : String
        , maybeProjectId : Maybe ProjectId
        , maybeDueDate : Maybe Date
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


type TodoFormFieldPatch
    = TitleChanged String
    | DueDateChanged (Maybe Date)
    | ProjectIdChanged (Maybe ProjectId)


type Msg
    = NoOp
    | SetTodoIsDone TodoId Bool
    | DeleteTodo TodoId
    | AddTodoClicked (Maybe ProjectId) (Maybe Date)
    | EditTodoClicked Todo
    | PatchTodoFormField TodoFormFieldPatch
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

        SetTodoIsDone todoId isDone ->
            ( model |> mapTodoList (updateWhenIdEq todoId (\todo -> { todo | isDone = isDone }))
            , Cmd.none
            )

        AddTodoClicked maybeProjectId maybeDueDate ->
            ( model
                |> setTodoForm
                    (AddTodoForm
                        { title = ""
                        , maybeProjectId = maybeProjectId
                        , maybeDueDate = maybeDueDate
                        , initialDueDate = maybeDueDate
                        }
                    )
            , Cmd.none
            )

        EditTodoClicked todo ->
            ( model |> setTodoForm (EditTodoForm todo)
            , Cmd.none
            )

        PatchTodoFormField subMsg ->
            let
                mapperFunc =
                    updateTodoFormFields subMsg
            in
            ( model
                |> mapTodoForm
                    (\form ->
                        case form of
                            AddTodoForm addTodoFields ->
                                AddTodoForm <| mapperFunc addTodoFields

                            EditTodoForm todo ->
                                EditTodoForm <| mapperFunc todo
                    )
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


updateTodoFormFields : TodoFormFieldPatch -> HasTodoFormFields a -> HasTodoFormFields a
updateTodoFormFields msg fields =
    case msg of
        TitleChanged title ->
            { fields | title = title }

        DueDateChanged maybeDueDate ->
            { fields | maybeDueDate = maybeDueDate }

        ProjectIdChanged maybeProjectId ->
            { fields | maybeProjectId = maybeProjectId }


mapTodoForm : (a -> a) -> { b | maybeTodoForm : Maybe a } -> { b | maybeTodoForm : Maybe a }
mapTodoForm func model =
    { model | maybeTodoForm = model.maybeTodoForm |> Maybe.map func }


saveTodoForm : TodoForm -> Model -> ( Model, Cmd Msg )
saveTodoForm form model =
    let
        ( todo, newModel ) =
            case form of
                AddTodoForm fields ->
                    HasSeed.step (Todo.generatorFromPartial fields) model

                EditTodoForm editingTodo ->
                    ( editingTodo, model )
    in
    ( { newModel
        | todoList = upsertById todo newModel.todoList
        , maybeTodoForm = Nothing
      }
    , Cmd.none
    )


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


getEditTodoForm : Maybe TodoForm -> Maybe Todo
getEditTodoForm maybeForm =
    case maybeForm of
        Just (EditTodoForm editTodo) ->
            Just editTodo

        _ ->
            Nothing


getEditTodoFormForTodoId : TodoId -> Maybe TodoForm -> Maybe Todo
getEditTodoFormForTodoId todoId =
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
        ++ [ viewAddTodoItemForDueDate dueDate maybeTodoForm ]


viewAddTodoItemForDueDate date maybeTodoForm =
    getAddTodoFormWithInitialDueDateEq date maybeTodoForm
        |> MX.unwrap (viewAddTodoButton (AddTodoClicked Nothing (Just date))) viewTodoForm


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
    viewEditableTodoList ProjectItemLayout model filteredTodoList
        ++ [ viewAddTodoItemForProject maybeProjectId maybeTodoForm
           ]


viewAddTodoItemForProject maybeProjectId maybeTodoForm =
    getAddTodoForm maybeTodoForm
        |> MX.unwrap (viewAddTodoButton (AddTodoClicked maybeProjectId Nothing)) viewTodoForm


viewEditableTodoList : TodoItemLayout -> Model -> List Todo -> List (H.Html Msg)
viewEditableTodoList layout model =
    keyed (.id >> TodoId.toString) (viewEditableTodoItem layout model)
        >> colKeyed []
        >> List.singleton


keyed : (item -> String) -> (item -> html) -> List item -> List ( String, html )
keyed keyF renderF =
    List.map (\item -> ( keyF item, renderF item ))


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
        , row [ A.class "child absolute right-0 bg-white-90" ] [ btn2 "X" (DeleteTodo todo.id) ]
        ]


viewAddTodoButton : Msg -> H.Html Msg
viewAddTodoButton onClick =
    row [ A.class "pa1" ] [ btn2 "add todo" onClick ]


viewTodoForm { title, maybeProjectId, maybeDueDate } =
    col [ A.class "pa1" ]
        [ col [ A.class "pv1" ]
            [ ipt2 title (PatchTodoFormField << TitleChanged)
            ]
        , viewProjectSelect maybeProjectId (PatchTodoFormField << ProjectIdChanged)
        , viewDueDateInput maybeDueDate (PatchTodoFormField << DueDateChanged)
        , row [ A.class "pv1" ] [ btn2 "Save" Save, btn2 "Cancel" Cancel ]
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
    H.select [ E.onInput (ProjectId.fromString >> projectIdChanged) ]
        (H.option [] [ H.text "Inbox" ]
            :: List.map viewOpt Project.mockProjects
        )


main : Program Flags Model Msg
main =
    Browser.element
        { init = \f -> init f |> Return.effect_ cacheModel_
        , update = \msg model -> update msg model |> Return.effect_ cacheModel_
        , view = view
        , subscriptions = subscriptions
        }
