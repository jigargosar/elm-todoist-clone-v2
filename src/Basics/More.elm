module Basics.More exposing (idEq, updateIfIdEq, when)

import List.Extra


when : (a -> Bool) -> (a -> a) -> a -> a
when pred func val =
    if pred val then
        func val

    else
        val


idEq : a -> { b | id : a } -> Bool
idEq id_ { id } =
    id == id_


type alias HasId a id =
    { a | id : id }


updateIfIdEq : id -> (HasId a id -> HasId a id) -> List (HasId a id) -> List (HasId a id)
updateIfIdEq id =
    List.Extra.updateIf (idEq id)
