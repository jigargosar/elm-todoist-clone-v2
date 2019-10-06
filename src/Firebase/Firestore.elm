port module Firebase.Firestore exposing (deleteTodo, onTodoListReceived, pushTodoList)

import Json.Encode as JE exposing (Value)
import Todo exposing (Todo)
import TodoId exposing (TodoId)


port firePushTodoList : Value -> Cmd msg


port fireDeleteTodoId : Value -> Cmd msg


port onFireTodoList : (Value -> msg) -> Sub msg


pushTodoList : List Todo -> Cmd msg
pushTodoList =
    firePushTodoList << JE.list Todo.encoder


deleteTodo : TodoId -> Cmd msg
deleteTodo =
    fireDeleteTodoId << TodoId.encoder


onTodoListReceived : (Value -> msg) -> Sub msg
onTodoListReceived =
    onFireTodoList
