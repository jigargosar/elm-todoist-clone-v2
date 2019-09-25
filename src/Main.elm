port module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (type_)
import Json.Decode as JD
import Json.Decode.Pipeline as JD exposing (optional, required, resolve)
import Json.Encode as JE exposing (Value)


type alias Flags =
    { cache : Value
    , now : Int
    }


type alias Todo =
    { id : String
    , title : String
    , isDone : Bool
    }


type alias Model =
    { todoList : List Todo
    }


createTodo : String -> String -> Todo
createTodo id title =
    Todo id title False


initialTodoList =
    [ createTodo "1" "Get Milk!!"
    , createTodo "2" "Submit assignment"
    , createTodo "3" "Check Facebook"
    , createTodo "4" "Go to movies"
    , createTodo "5" "Get Milk!!"
    ]


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
                |> required "id" JD.string
                |> required "title" JD.string
                |> required "isDone" JD.bool
    in
    JD.succeed Cache
        |> optional "todoList" (JD.list todoDecoder) initialTodoList


cacheEncoder : Cache -> Value
cacheEncoder cache =
    let
        todoEncoder todo =
            JE.object
                [ ( "id", JE.string todo.id )
                , ( "title", JE.string todo.title )
                , ( "isDone", JE.bool todo.isDone )
                ]
    in
    JE.object
        [ ( "todoList", JE.list todoEncoder cache.todoList )
        ]


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


init : Flags -> ( Model, Cmd msg )
init flags =
    let
        cache =
            flags.cache
                |> JD.decodeValue (stringOrValueDecoder cacheDecoder)
                |> Result.withDefault defaultCacheValue
    in
    ( { todoList = cache.todoList
      }
    , setCache (JE.encode 0 (cacheEncoder cache))
    )


port setCache : String -> Cmd msg


type Msg
    = NoOp


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions _ =
    Sub.none


view : Model -> Html.Html Msg
view model =
    div []
        [ viewTodoList model.todoList
        ]


viewTodoList : List Todo -> Html Msg
viewTodoList list =
    div [] (List.map viewTodo list)


viewTodo : Todo -> Html Msg
viewTodo todo =
    div []
        [ input [ type_ "checkbox" ] []
        , text todo.title
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
