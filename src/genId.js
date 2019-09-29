const fs = require('fs')

fs.writeFileSync(
  './src/ProjectId.elm',
  `
module ProjectId exposing (ProjectId, encoder, decoder, toString ,fromString)

import Json.Encode as JE exposing(Value)
import Json.Decode as JE exposing(Value)

type ProjectId =
    ProjectId String
    

encoder: ProjectId -> Value
encoder (ProjectId v) = 
    JE.string v

decoder: Decoder ProjectId
decoder = 
  JD.map ProjectID JD.string
  
toString: ProjectId -> String
toString (ProjectId s) = 
  s
  
fromString: String -> Maybe ProjectId
fromString =
  String.trim
  >> \s -> if String.isEmpty s then Nothing else ProjectId s  
`,
  { encoding: 'UTF-8' },
)
