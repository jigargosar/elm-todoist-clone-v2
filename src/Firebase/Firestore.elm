port module Firebase.Firestore exposing (fireDeleteTodoId, firePushTodoList, onFireTodoList)

import Json.Encode exposing (Value)


port firePushTodoList : Value -> Cmd msg


port fireDeleteTodoId : Value -> Cmd msg


port onFireTodoList : (Value -> msg) -> Sub msg
