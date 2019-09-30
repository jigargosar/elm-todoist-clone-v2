module Project exposing (Project, decoder, encoder, mockListGenerator, viewSelectOne)

import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE exposing (object)
import ProjectId exposing (ProjectId)
import Random


type alias Project =
    { id : ProjectId
    , title : String
    , isDeleted : Bool
    }


encoder : Project -> JE.Value
encoder p =
    object
        [ ( "id", ProjectId.encoder p.id )
        , ( "title", JE.string p.title )
        , ( "isDeleted", JE.bool p.isDeleted )
        ]


decoder : JD.Decoder Project
decoder =
    JD.succeed Project
        |> required "id" ProjectId.decoder
        |> required "title" JD.string
        |> required "isDeleted" JD.bool


type alias Internal =
    Project


mockTitles : List String
mockTitles =
    [ "Build Utils"
    , "Publish Post"
    , "Complete Story"
    , "Exam Prep"
    ]


mockListGenerator : Random.Generator (List Project)
mockListGenerator =
    let
        gen : Random.Generator Project
        gen =
            ProjectId.generator
                |> Random.map (\id -> Project id "" False)
    in
    Random.list (List.length mockTitles) gen
        |> Random.map (List.map2 setTitle mockTitles)


setTitle : String -> Project -> Project
setTitle title =
    map (\m -> { m | title = title })


map : (Internal -> Internal) -> Project -> Project
map =
    identity


viewSelectOne : Maybe ProjectId -> (Maybe ProjectId -> msg) -> List Project -> H.Html msg
viewSelectOne maybeProjectId onChange projectList =
    let
        viewOpt { id, title } =
            H.option
                [ A.selected (maybeProjectId == Just id)
                , A.value <| ProjectId.toString id
                ]
                [ H.text title ]
    in
    H.select [ E.onInput (ProjectId.fromString >> onChange) ]
        (H.option [ A.value " " ] [ H.text "Inbox" ]
            :: List.map viewOpt projectList
        )
