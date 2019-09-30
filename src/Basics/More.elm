module Basics.More exposing (HasId, allPass, anyPass, callWith, clampListIndex, clampListLength, idEq, insertAt, propEq, uncurry, updateWhenIdEq, upsertById, when)

import List.Extra


when : (a -> Bool) -> (a -> a) -> a -> a
when pred func val =
    if pred val then
        func val

    else
        val


type alias HasId a id =
    { a | id : id }


idEq : id -> HasId a id -> Bool
idEq id_ { id } =
    id == id_


propEq : (big -> small) -> small -> big -> Bool
propEq get val obj =
    get obj == val


updateWhenIdEq : id -> (HasId a id -> HasId a id) -> List (HasId a id) -> List (HasId a id)
updateWhenIdEq id =
    List.Extra.updateIf (idEq id)


callWith : a -> (a -> b) -> b
callWith =
    (|>)


upsertById : { a | id : id } -> List (HasId a id) -> List (HasId a id)
upsertById item itemList =
    if List.any (idEq item.id) itemList then
        updateWhenIdEq item.id (always item) itemList

    else
        item :: itemList


anyPass : List (b -> Bool) -> b -> Bool
anyPass predFunctions val =
    List.any (\fn -> fn val) predFunctions


allPass : List (b -> Bool) -> b -> Bool
allPass predFunctions val =
    List.all (\fn -> fn val) predFunctions


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b


clampListIndex : List a -> Int -> Int
clampListIndex list =
    clamp 0 (List.length list - 1)


clampListLength : List a -> Int -> Int
clampListLength list =
    clamp 0 (List.length list)


insertAt : Int -> a -> List a -> List a
insertAt i c =
    List.Extra.splitAt i
        >> (\( l, r ) -> l ++ [ c ] ++ r)
