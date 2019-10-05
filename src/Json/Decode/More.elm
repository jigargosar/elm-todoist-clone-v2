module Json.Decode.More exposing (..)

import Json.Decode exposing (..)


tupleDecoder : Decoder a -> Decoder b -> Decoder ( a, b )
tupleDecoder firstDecoder secondDecoder =
    map2 Tuple.pair (index 0 firstDecoder) (index 1 secondDecoder)
