module Todo exposing (Todo, fromPartial, mockList)

import Date exposing (Date)
import ProjectId exposing (ProjectId)
import TodoId exposing (TodoId)


type alias Todo =
    { id : TodoId
    , title : String
    , isDone : Bool
    , isDeleted : Bool
    , maybeProjectId : Maybe ProjectId
    , maybeDueDate : Maybe Date
    }


type alias Partial a =
    { a
        | title : String
        , maybeProjectId : Maybe ProjectId
        , maybeDueDate : Maybe Date
    }


fromPartial : TodoId -> Partial a -> Todo
fromPartial id { title, maybeProjectId, maybeDueDate } =
    Todo id title False False maybeProjectId maybeDueDate


createMockTodo : String -> String -> Maybe Todo
createMockTodo id title =
    TodoId.fromString id
        |> Maybe.map (\todoId -> Todo todoId title False False Nothing Nothing)


mockList : List Todo
mockList =
    [ createMockTodo "1" "Get Milk!!"
    , createMockTodo "2" "Submit assignment"
    , createMockTodo "3" "Check Facebook"
    , createMockTodo "4" "Go to movies"
    , createMockTodo "5" "Get Milk!!"
    ]
        |> List.filterMap identity
