module Main exposing (main)

import Browser
import Html exposing (Html, div, text)


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
            , Todo "2" "Submit assignment"
            , Todo "3" "Check Facebook"
            , Todo "4" "Go to movies"
            , Todo "5" "Get Milk!!"
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
