module ProjectId exposing (ProjectId, encoder, decoder, toString ,fromString, generator)

import Json.Encode as JE exposing(Value)
import Json.Decode as JD exposing(Decoder)
import Random

type ProjectId =
    ProjectId String
    

encoder: ProjectId -> Value
encoder (ProjectId v) = 
    JE.string v

decoder: Decoder ProjectId
decoder = 
  JD.map ProjectId JD.string
  
toString: ProjectId -> String
toString (ProjectId s) = 
  s
  
fromString: String -> Maybe ProjectId
fromString =
  String.trim
  >> \s -> if String.isEmpty s then Nothing else Just (ProjectId s)

generator: Random.Generator ProjectId
generator = 
  Random.int (10 ^ 3) (10 ^ 5)
  |> Random.map (String.fromInt >> (++) "ProjectId-" >> ProjectId)

  
