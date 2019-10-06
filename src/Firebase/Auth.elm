port module Firebase.Auth exposing
    ( AuthState(..)
    , AuthUser
    , decoder
    , onAuthStateChanged
    , signIn
    , signOut
    )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode exposing (Value)


port onAuthStateChanged : (Value -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


type alias AuthUser =
    { uid : String
    , displayName : String
    }


type AuthState
    = Unknown
    | SignedIn AuthUser
    | SignedOut


authUserDecoder : Decoder AuthUser
authUserDecoder =
    JD.succeed AuthUser
        |> required "uid" JD.string
        |> required "displayName" JD.string


decoder : Decoder AuthState
decoder =
    JD.oneOf [ JD.null SignedOut, authUserDecoder |> JD.map SignedIn ]
