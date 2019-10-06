port module Firebase.Firestore exposing (fireDeleteTodoId, onFireTodoList, pushTodoList)

import Json.Encode as JE exposing (Value)
import Todo exposing (Todo)


port firePushTodoList : Value -> Cmd msg


port fireDeleteTodoId : Value -> Cmd msg


port onFireTodoList : (Value -> msg) -> Sub msg


pushTodoList : List Todo -> Cmd msg
pushTodoList =
    firePushTodoList << JE.list Todo.encoder
