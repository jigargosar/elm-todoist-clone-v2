#!/usr/bin/env node
'use strict'
const meow = require('meow')
const fs = require('fs')

const cli = meow(
  `
	Usage
	  $ genId <name> [... <more names>]
  
  Options:
    <name> - Name of the Id type, e.g. ProjectId

  Examples
	  $ genId ProjectId
	  $ genId ProjectId TodoId
	  
`,
  {
    flags: {},
  },
)

if (cli.input.length === 0) return cli.showHelp()

cli.input.forEach(genId)

function genId(name) {
  const filePath = `./src/${name}.elm`
  fs.writeFileSync(filePath, idFileCode(name), {
    encoding: 'UTF-8',
    flag: 'w',
  })
  console.log('Generated: ', filePath)
}

function idFileCode(name) {
  return `module ${name} exposing (${name}, encoder, decoder, toString ,fromString, generator)

import Json.Encode as JE exposing(Value)
import Json.Decode as JD exposing(Decoder)
import Random

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

generator: Random.Generator ${name}
generator = 
  Random.int (10 ^ 3) (10 ^ 5)
  |> Random.map (String.fromInt >> (++) "${name}-" >> ${name})

  
`
}
