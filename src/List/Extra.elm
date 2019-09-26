module List.Extra exposing (mapWhen)

import Basics.Extra exposing (when)


mapWhen : (a -> Bool) -> (a -> a) -> List a -> List a
mapWhen pred func =
    List.map (when pred func)
