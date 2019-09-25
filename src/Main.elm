port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (type_)
import Html.Events as E
import Json.Decode as JD
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE exposing (Value, encode, object)



-- TODO_


type TodoId
    = TodoId String


type alias Todo =
    { id : TodoId
    , title : String
    , isDone : Bool
    }


createTodo : String -> String -> Todo
createTodo id title =
    Todo (TodoId id) title False


initialTodoList =
    [ createTodo "1" "Get Milk!!"
    , createTodo "2" "Submit assignment"
    , createTodo "3" "Check Facebook"
    , createTodo "4" "Go to movies"
    , createTodo "5" "Get Milk!!"
    ]


type TodoPatch
    = SetDone Bool


patchTodo : TodoPatch -> Todo -> Todo
patchTodo patch todo =
    case patch of
        SetDone isDone ->
            { todo | isDone = isDone }



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
    in
    JD.succeed Cache
        |> optional "todoList" (JD.list todoDecoder) initialTodoList


cacheModel : Model -> Cmd msg
cacheModel { todoList } =
    let
        todoEncoder { id, title, isDone } =
            let
                unwrapId (TodoId v) =
                    v
            in
            object
                [ ( "id", JE.string <| unwrapId id )
                , ( "title", JE.string title )
                , ( "isDone", JE.bool isDone )
                ]

        encoded =
            object
                [ ( "todoList", JE.list todoEncoder todoList )
                ]
    in
    setCache (encode 0 encoded)


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


type alias AddTodoForm =
    { fields : { title : String }, isOpen : Bool }


type alias Model =
    { todoList : List Todo
    , addTodo : AddTodoForm
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
            , addTodo = { fields = { title = "" }, isOpen = False }
            }
    in
    ( model
    , cacheModel model
    )


mapTodo : TodoId -> (Todo -> Todo) -> Model -> Model
mapTodo todoId fn model =
    { model
        | todoList =
            model.todoList
                |> List.map
                    (\todo ->
                        if todo.id == todoId then
                            fn todo

                        else
                            todo
                    )
    }



-- UPDATE


type Msg
    = NoOp
    | PatchTodo TodoId TodoPatch
    | AddTodoClicked


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
                    mapTodo todoId (patchTodo todoPatch) model
            in
            ( newModel, cacheModel newModel )

        AddTodoClicked ->
            let
                addTodo =
                    model.addTodo

                newModel =
                    { model | addTodo = { addTodo | isOpen = True } }
            in
            ( newModel, cacheModel newModel )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Html.Html Msg
view model =
    div []
        [ viewTodoList model.todoList
        , viewAddTodo model.addTodo
        ]


viewTodoList : List Todo -> Html Msg
viewTodoList list =
    div [] (List.map viewTodo list)


viewTodo : Todo -> Html Msg
viewTodo todo =
    div []
        [ input [ type_ "checkbox", E.onCheck (doneChecked todo.id) ] []
        , text todo.title
        ]


viewAddTodo : AddTodoForm -> Html Msg
viewAddTodo { fields, isOpen } =
    if isOpen then
        div []
            []

    else
        button [ E.onClick AddTodoClicked ] [ text "add todo" ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
