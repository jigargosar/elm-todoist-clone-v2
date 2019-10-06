module Tagged.Dict.More exposing (..)

import Tagged exposing (Tagged)
import Tagged.Dict as TaggedDict exposing (TaggedDict)


fromListBy : (v -> Tagged tag comparable) -> List v -> TaggedDict tag comparable v
fromListBy func list =
    List.map (\a -> ( func a, a )) list
        |> TaggedDict.fromList
