module Basics.Extra exposing (when)


when : (a -> Bool) -> (a -> a) -> a -> a
when pred func val =
    if pred val then
        func val

    else
        val
