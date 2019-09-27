module Basics.More exposing (HasId, callWith, idEq, propEq, updateWhenIdEq, upsertById, when)

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
