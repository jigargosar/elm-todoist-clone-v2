module Todo exposing (Todo, applyPatches, decoder, encoder, generatorFromPartial, mockListGenerator, patchWithPartial)

import Date exposing (Date)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE exposing (Value, object)
import Maybe.Extra as MX
import ProjectId exposing (ProjectId)
import Random
import Time exposing (Posix)
import TodoId exposing (TodoId)


type alias Todo =
    { id : TodoId
    , title : String
    , isDone : Bool
    , isDeleted : Bool
    , maybeProjectId : Maybe ProjectId
    , maybeDueDate : Maybe Date
    , projectSortIdx : Int
    , createdAt : Posix
    , updatedAt : Posix
    }


type alias Internal =
    Todo


mockTitles : List String
mockTitles =
    [ "Get Milk!!"
    , "Submit assignment"
    , "Check Facebook"
    , "Go to movies"
    , "Get Coffee"
    ]


defaultPosix : Posix
defaultPosix =
    Time.millisToPosix 0


mockListGenerator : Random.Generator (List Todo)
mockListGenerator =
    let
        gen : Random.Generator Todo
        gen =
            TodoId.generator
                |> Random.map (\id -> Todo id "" False False Nothing Nothing 0 defaultPosix defaultPosix)
    in
    Random.list (List.length mockTitles) gen
        |> Random.map (List.map2 setTitle mockTitles >> setSortIndices)


map : (Internal -> Internal) -> Todo -> Todo
map =
    identity


setTitle : String -> Todo -> Todo
setTitle title =
    map (\m -> { m | title = title })


setProjectSortIdx : Int -> Todo -> Todo
setProjectSortIdx projectSortIdx =
    map (\m -> { m | projectSortIdx = projectSortIdx })


setSortIndices : List Todo -> List Todo
setSortIndices =
    List.indexedMap setProjectSortIdx


posixDecoder : JD.Decoder Posix
posixDecoder =
    JD.int |> JD.map Time.millisToPosix


decoder : JD.Decoder Todo
decoder =
    let
        optionalPosix field =
            optional field posixDecoder defaultPosix
    in
    JD.succeed Todo
        |> required "id" TodoId.decoder
        |> required "title" JD.string
        |> required "isDone" JD.bool
        |> optional "isDeleted" JD.bool False
        |> optional "maybeProjectId" (ProjectId.decoder |> JD.map Just) Nothing
        |> optional "maybeDueDate" (JD.string |> JD.map (Date.fromIsoString >> Result.toMaybe)) Nothing
        |> optional "projectSortIdx" JD.int 0
        |> optionalPosix "createdAt"
        |> optionalPosix "updatedAt"


maybeEncoder : (a -> Value) -> Maybe a -> Value
maybeEncoder =
    MX.unwrap JE.null


posixEncoder : Posix -> Value
posixEncoder posix =
    JE.int (Time.posixToMillis posix)


encoder : Todo -> Value
encoder ({ id, title, isDone, isDeleted, maybeProjectId, maybeDueDate, projectSortIdx } as m) =
    object
        [ ( "id", TodoId.encoder id )
        , ( "title", JE.string title )
        , ( "isDone", JE.bool isDone )
        , ( "isDeleted", JE.bool isDeleted )
        , ( "maybeProjectId", maybeEncoder ProjectId.encoder maybeProjectId )
        , ( "maybeDueDate", maybeEncoder (Date.toIsoString >> JE.string) maybeDueDate )
        , ( "projectSortIdx", JE.int projectSortIdx )
        , ( "createdAt", posixEncoder m.createdAt )
        , ( "updatedAt", posixEncoder m.updatedAt )
        ]


type alias Partial a =
    { a
        | title : String
        , maybeProjectId : Maybe ProjectId
        , maybeDueDate : Maybe Date
        , projectSortIdx : Int
    }


fromPartial : TodoId -> Posix -> Partial a -> Todo
fromPartial id now { title, maybeProjectId, maybeDueDate, projectSortIdx } =
    Todo id title False False maybeProjectId maybeDueDate projectSortIdx now now


generatorFromPartial : Posix -> Partial a -> Random.Generator Todo
generatorFromPartial now partial =
    TodoId.generator
        |> Random.map (\id -> fromPartial id now partial)


patchWithPartial : Posix -> Partial a -> Todo -> Todo
patchWithPartial now p todo =
    { todo
        | title = p.title
        , maybeProjectId = p.maybeProjectId
        , maybeDueDate = p.maybeDueDate
        , projectSortIdx = p.projectSortIdx
        , updatedAt = now
    }


type Patch
    = Title String
    | Project (Maybe ProjectId)
    | ProjectSortIdx Int
    | DueDate (Maybe Date)
    | Completed Bool


applyPatches : Posix -> List Patch -> Todo -> Todo
applyPatches now patches todo =
    List.foldl applyPatch todo patches
        |> setUpdatedAtIfNotEq now todo


setUpdatedAtIfNotEq : Posix -> Todo -> Todo -> Todo
setUpdatedAtIfNotEq now oldTodo newTodo =
    if oldTodo /= newTodo then
        { newTodo | updatedAt = now }

    else
        newTodo


applyPatch : Patch -> Todo -> Todo
applyPatch patch todo =
    case patch of
        Title v ->
            { todo | title = v }

        Project v ->
            { todo | maybeProjectId = v }

        ProjectSortIdx v ->
            { todo | projectSortIdx = v }

        DueDate v ->
            { todo | maybeDueDate = v }

        Completed v ->
            { todo | isDone = v }
