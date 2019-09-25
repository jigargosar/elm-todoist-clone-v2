module Main exposing (main)

import Browser
import Html exposing (div, text)


type alias Flags =
    {}


type alias Todo =
    { id : String
    , title : String
    }


type alias Model =
    { todoList : List Todo }


init : Flags -> ( Model, Cmd msg )
init _ =
    ( { todoList =
            [ Todo "1" "Get Milk!!"
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


view model =
    div []
        [ viewTodoList model.todoList
        ]


viewTodoList list =
    div [] (List.map viewTodo list)


viewTodo todo =
    div []
        [ text todo.title
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
