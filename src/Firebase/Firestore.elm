port module Firebase.Firestore exposing (decodeTodoList, deleteTodo, onTodoListReceived, pushTodoList)

import Json.Decode as JD
import Json.Decode.More as JDM
import Json.Encode as JE exposing (Value)
import Result.Extra as RX
import Todo exposing (Todo)
import TodoId exposing (TodoId)


port firestoreSetAll : ( String, Value ) -> Cmd msg


port firestoreDelete : ( String, Value ) -> Cmd msg


port onFireTodoList : (Value -> msg) -> Sub msg


pushTodoList : List Todo -> Cmd msg
pushTodoList =
    firestoreSetAll << Tuple.pair "todos" << JE.list Todo.encoder


deleteTodo : TodoId -> Cmd msg
deleteTodo =
    firestoreDelete << Tuple.pair "todos" << TodoId.encoder


onTodoListReceived : (Value -> msg) -> Sub msg
onTodoListReceived =
    onFireTodoList


decodeTodoList : Value -> ( List Todo, List String )
decodeTodoList value =
    JD.decodeValue (JD.list (JDM.tuple JD.string JD.value)) value
        |> Result.map
            (List.foldr
                (\( todoIdStr, todoValue ) ( todoList, errorList ) ->
                    case JD.decodeValue Todo.decoder todoValue of
                        Ok todo ->
                            ( todo :: todoList, errorList )

                        Err error ->
                            ( todoList
                            , ("Error Decoding Todo: "
                                ++ todoIdStr
                                ++ "\n"
                                ++ JD.errorToString error
                              )
                                :: errorList
                            )
                )
                ( [], [] )
            )
        |> RX.extract (\error -> ( [], [ JD.errorToString error ] ))
