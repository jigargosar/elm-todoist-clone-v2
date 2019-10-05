module ProjectId exposing (ProjectId, decoder, encoder, fromString, generator, toString)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random
import Tagged exposing (Tagged)


type ProjectIdTag
    = ProjectIdTag


type alias ProjectId =
    Tagged ProjectIdTag String


encoder : ProjectId -> Value
encoder =
    Tagged.untag >> JE.string


decoder : Decoder ProjectId
decoder =
    JD.map Tagged.tag JD.string


toString : ProjectId -> String
toString =
    Tagged.untag


fromString : String -> Maybe ProjectId
fromString =
    String.trim
        >> (\s ->
                if String.isEmpty s then
                    Nothing

                else
                    Just <| Tagged.tag s
           )


generator : Random.Generator ProjectId
generator =
    Random.int (10 ^ 3) (10 ^ 5)
        |> Random.map (String.fromInt >> (++) "ProjectId-" >> Tagged.tag)
