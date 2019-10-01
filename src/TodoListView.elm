module TodoListView exposing (..)

import Todo exposing (Todo)
import TodoForm exposing (TodoForm)


type alias Config a =
    { viewTodo : Todo -> a
    , viewForm : TodoForm -> a
    }


view : Config a -> Maybe TodoForm -> List Todo -> List a
view =
    Debug.todo "implement"
