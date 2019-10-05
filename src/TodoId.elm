module TodoId exposing (TodoId, TodoIdDict, decoder, encoder, fromString, generator, toString)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random
import Tagged exposing (Tagged, tag, untag)
import Tagged.Dict exposing (TaggedDict)


type TodoIdTag
    = TodoIdTag


type alias TodoId =
    Tagged TodoIdTag String


type alias TodoIdDict q =
    TaggedDict TodoIdTag String q


encoder : TodoId -> Value
encoder =
    untag >> JE.string


decoder : Decoder TodoId
decoder =
    JD.map tag JD.string


toString : TodoId -> String
toString =
    untag


fromString : String -> Maybe TodoId
fromString =
    String.trim
        >> (\s ->
                if String.isEmpty s then
                    Nothing

                else
                    Just (tag s)
           )


generator : Random.Generator TodoId
generator =
    Random.int (10 ^ 3) (10 ^ 5)
        |> Random.map (String.fromInt >> (++) "TodoId-" >> tag)
