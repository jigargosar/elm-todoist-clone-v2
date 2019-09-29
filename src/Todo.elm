module Todo exposing (Todo, decoder, encoder, generatorFromPartial, mockList, patchWithPartial)

import Date exposing (Date)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE exposing (Value, object)
import Maybe.Extra as MX
import ProjectId exposing (ProjectId)
import Random
import TodoId exposing (TodoId)


type alias Todo =
    { id : TodoId
    , title : String
    , isDone : Bool
    , isDeleted : Bool
    , maybeProjectId : Maybe ProjectId
    , maybeDueDate : Maybe Date
    , projectSortIdx : Int
    }


decoder : JD.Decoder Todo
decoder =
    JD.succeed Todo
        |> required "id" TodoId.decoder
        |> required "title" JD.string
        |> required "isDone" JD.bool
        |> optional "isDeleted" JD.bool False
        |> optional "maybeProjectId"
            (ProjectId.decoder |> JD.map Just)
            Nothing
        |> optional "maybeDueDate" (JD.string |> JD.map (Date.fromIsoString >> Result.toMaybe)) Nothing
        |> optional "projectSortIdx" JD.int 0


maybeEncoder : (a -> Value) -> Maybe a -> Value
maybeEncoder =
    MX.unwrap JE.null


encoder : Todo -> Value
encoder { id, title, isDone, isDeleted, maybeProjectId, maybeDueDate, projectSortIdx } =
    object
        [ ( "id", TodoId.encoder id )
        , ( "title", JE.string title )
        , ( "isDone", JE.bool isDone )
        , ( "isDeleted", JE.bool isDeleted )
        , ( "maybeProjectId", maybeEncoder ProjectId.encoder maybeProjectId )
        , ( "maybeDueDate", maybeEncoder (Date.toIsoString >> JE.string) maybeDueDate )
        , ( "projectSortIdx", JE.int projectSortIdx )
        ]


type alias Partial a =
    { a
        | title : String
        , maybeProjectId : Maybe ProjectId
        , maybeDueDate : Maybe Date
    }


fromPartial : TodoId -> Partial a -> Todo
fromPartial id { title, maybeProjectId, maybeDueDate } =
    Todo id title False False maybeProjectId maybeDueDate 0


generatorFromPartial : Partial a -> Random.Generator Todo
generatorFromPartial partial =
    TodoId.generator
        |> Random.map (\id -> fromPartial id partial)


patchWithPartial : Partial a -> Todo -> Todo
patchWithPartial { title, maybeProjectId, maybeDueDate } todo =
    { todo | title = title, maybeProjectId = maybeProjectId, maybeDueDate = maybeDueDate }


createMockTodo : String -> String -> Maybe Todo
createMockTodo id title =
    TodoId.fromString id
        |> Maybe.map (\todoId -> Todo todoId title False False Nothing Nothing 0)


mockList : List Todo
mockList =
    [ createMockTodo "1" "Get Milk!!"
    , createMockTodo "2" "Submit assignment"
    , createMockTodo "3" "Check Facebook"
    , createMockTodo "4" "Go to movies"
    , createMockTodo "5" "Get Milk!!"
    ]
        |> List.filterMap identity
