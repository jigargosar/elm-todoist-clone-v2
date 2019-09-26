module Basics.More exposing (idEq, when)


when : (a -> Bool) -> (a -> a) -> a -> a
when pred func val =
    if pred val then
        func val

    else
        val


idEq : a -> { b | id : a } -> Bool
idEq id_ { id } =
    id == id_
