module Basics.More exposing (idEq, updateWhenIdEq, when)

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


updateWhenIdEq : id -> (HasId a id -> HasId a id) -> List (HasId a id) -> List (HasId a id)
updateWhenIdEq id =
    List.Extra.updateIf (idEq id)
