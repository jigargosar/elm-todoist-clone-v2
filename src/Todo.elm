module Todo exposing
    ( Patch(..)
    , Todo
    , decoder
    , encoder
    , generator
    , isNewerThan
    , mockListGenerator
    , update
    )

import Date exposing (Date)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE exposing (Value, object)
import Maybe.Extra as MX
import ProjectId exposing (ProjectId)
import Random exposing (Generator)
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


generator : Posix -> List Patch -> Generator Todo
generator now patches =
    TodoId.generator
        |> Random.map
            ((\id -> Todo id "" False False Nothing Nothing 0 now now)
                >> applyPatches patches
            )


type Patch
    = Title String
    | Project (Maybe ProjectId)
    | ProjectSortIdx Int
    | DueDate (Maybe Date)
    | Completed Bool


isNewerThan : Todo -> Todo -> Bool
isNewerThan t2 t1 =
    let
        updatedAtMillis =
            Time.posixToMillis << .updatedAt
    in
    updatedAtMillis t1 > updatedAtMillis t2


update : Posix -> List Patch -> Todo -> Maybe ( Todo, Todo )
update now patches todo =
    applyPatches patches todo
        |> setUpdatedAtIfChanged now todo


setUpdatedAtIfChanged : Posix -> Todo -> Todo -> Maybe ( Todo, Todo )
setUpdatedAtIfChanged now oldTodo newTodo =
    if oldTodo /= newTodo then
        Just ( oldTodo, { newTodo | updatedAt = now } )

    else
        Nothing


applyPatches : List Patch -> Todo -> Todo
applyPatches patches todo =
    List.foldl applyPatchesHelp todo patches


applyPatchesHelp : Patch -> Todo -> Todo
applyPatchesHelp patch todo =
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
