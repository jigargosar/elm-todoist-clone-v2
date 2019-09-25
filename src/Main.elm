module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (type_)


type alias Flags =
    {}


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


init : Flags -> ( Model, Cmd msg )
init _ =
    ( { todoList =
            [ createTodo "1" "Get Milk!!"
            , createTodo "2" "Submit assignment"
            , createTodo "3" "Check Facebook"
            , createTodo "4" "Go to movies"
            , createTodo "5" "Get Milk!!"
            ]
      }
    , Cmd.none
    )


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
