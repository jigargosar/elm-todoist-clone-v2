const fs = require('fs')

function genId(name) {
  fs.writeFileSync(
    `./src/${name}.elm`,
    `
module ${name} exposing (${name}, encoder, decoder, toString ,fromString)

import Json.Encode as JE exposing(Value)
import Json.Decode as JD exposing(Decoder)

type ${name} =
    ${name} String
    

encoder: ${name} -> Value
encoder (${name} v) = 
    JE.string v

decoder: Decoder ${name}
decoder = 
  JD.map ${name} JD.string
  
toString: ${name} -> String
toString (${name} s) = 
  s
  
fromString: String -> Maybe ${name}
fromString =
  String.trim
  >> \\s -> if String.isEmpty s then Nothing else Just (${name} s)  
`,
    { encoding: 'UTF-8', flag: 'w' },
  )
}
console.log('process.argv', process.argv)
genId('ProjectId')
