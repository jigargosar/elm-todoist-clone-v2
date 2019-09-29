module TodoId exposing (TodoId, encoder, decoder, toString ,fromString, generator)

import Json.Encode as JE exposing(Value)
import Json.Decode as JD exposing(Decoder)
import Random

type TodoId =
    TodoId String
    

encoder: TodoId -> Value
encoder (TodoId v) = 
    JE.string v

decoder: Decoder TodoId
decoder = 
  JD.map TodoId JD.string
  
toString: TodoId -> String
toString (TodoId s) = 
  s
  
fromString: String -> Maybe TodoId
fromString =
  String.trim
  >> \s -> if String.isEmpty s then Nothing else Just (TodoId s)

generator: Random.Generator TodoId
generator = 
  Random.int (10 ^ 3) (10 ^ 5)
  |> Random.map (String.fromInt >> (++) "TodoId-" >> TodoId)

  
