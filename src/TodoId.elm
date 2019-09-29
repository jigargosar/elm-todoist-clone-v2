
module TodoId exposing (TodoId, encoder, decoder, toString ,fromString)

import Json.Encode as JE exposing(Value)
import Json.Decode as JD exposing(Decoder)

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
