module Project exposing (Project, mockProjects, viewSelectOne)

import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import ProjectId exposing (ProjectId)


type alias Project =
    { id : ProjectId
    , title : String
    , isDeleted : Bool
    }


createMockProject : String -> String -> Maybe Project
createMockProject id title =
    ProjectId.fromString id
        |> Maybe.map (\projectId -> Project projectId title False)


mockProjects : List Project
mockProjects =
    [ createMockProject "1" "Build Utils"
    , createMockProject "2" "Publish Post"
    , createMockProject "3" "Complete Story"
    , createMockProject "4" "Exam Prep"
    ]
        |> List.filterMap identity


viewSelectOne : Maybe ProjectId -> (Maybe ProjectId -> msg) -> H.Html msg
viewSelectOne maybeProjectId onChange =
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
            :: List.map viewOpt mockProjects
        )
