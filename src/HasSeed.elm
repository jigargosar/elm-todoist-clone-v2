module HasSeed exposing (HasSeed, step)

import Random


type alias HasSeed a =
    { a | seed : Random.Seed }


setSeed seed model =
    { model | seed = seed }


step : Random.Generator a -> HasSeed b -> ( a, HasSeed b )
step generator model =
    let
        ( a, newSeed ) =
            Random.step generator model.seed
    in
    ( a, setSeed newSeed model )
