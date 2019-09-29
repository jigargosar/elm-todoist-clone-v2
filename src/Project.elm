module Project exposing (Project, mockProjects)

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
