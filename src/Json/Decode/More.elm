module Json.Decode.More exposing (..)

import Json.Decode exposing (..)


tuple : Decoder a -> Decoder b -> Decoder ( a, b )
tuple firstDecoder secondDecoder =
    map2 Tuple.pair (index 0 firstDecoder) (index 1 secondDecoder)
