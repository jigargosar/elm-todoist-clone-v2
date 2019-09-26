port module Main exposing (main)

import Basics.More exposing (idEq, updateWhenIdEq)
import Browser
import El exposing (click, el, rootEl, txt)
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events as E
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE exposing (Value, encode, object)
import List.Extra as List



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
    | TodoPatches (List TodoPatch)


patchTodo : TodoPatch -> Todo -> Todo
patchTodo patch todo =
    case patch of
        SetDone isDone ->
            { todo | isDone = isDone }

        TodoPatches patches ->
            List.foldl patchTodo todo patches



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

        addTodoDecoder : Decoder (Toggle AddTodoForm)
        addTodoDecoder =
            JD.oneOf
                [ JD.null Off
                , todoFormDecoder |> JD.map On
                ]

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
        |> optional "addTodo" addTodoDecoder Off


cacheModel : Model -> Cmd msg
cacheModel model =
    let
        todoEncoder : Todo -> Value
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
            , addTodo = Off
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


addTodoFormClicked : Msg
addTodoFormClicked =
    AddTodoForm { title = "" } |> setAddTodoForm


patchAddTodoTitle : AddTodoForm -> String -> Msg
patchAddTodoTitle { fields } title =
    AddTodoForm { fields | title = title } |> setAddTodoForm


viewAddTodo : Toggle AddTodoForm -> Html Msg
viewAddTodo addTodo =
    case addTodo of
        On ({ fields } as form) ->
            div []
                [ input
                    [ value fields.title
                    , E.onInput (patchAddTodoTitle form)
                    ]
                    []
                , div []
                    [ button [ E.onClick Save ] [ text "Save" ]
                    , button [ E.onClick closeForm ] [ text "Cancel" ]
                    ]
                ]

        Off ->
            rootEl
                [ el
                    [ click addTodoFormClicked
                    , txt "add todo"

                    --, attr (attribute "data-onclick" "alert('addTodo')")
                    ]
                ]


type alias Attrs msg =
    List (Attribute msg)


type alias Children msg =
    List (Html msg)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
