module Key exposing (ctrlEnter, enter, onKeyDown)

import Html.Styled as H
import Html.Styled.Events as E
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JDX
import Json.Decode.Pipeline exposing (required)


noModifiersDown : msg -> Decoder msg
noModifiersDown msg =
    JDX.when modifiersDecoder
        (\{ ctrlKey, shiftKey, altKey, metaKey } ->
            not (ctrlKey || shiftKey || altKey || metaKey)
        )
        (JD.succeed msg)


onlyCtrlDown : msg -> Decoder msg
onlyCtrlDown msg =
    JDX.when modifiersDecoder
        (\{ ctrlKey, shiftKey, altKey, metaKey } ->
            ctrlKey && not (shiftKey || altKey || metaKey)
        )
        (JD.succeed msg)


type alias Modifiers =
    { ctrlKey : Bool
    , shiftKey : Bool
    , altKey : Bool
    , metaKey : Bool
    }


modifiersDecoder : Decoder Modifiers
modifiersDecoder =
    let
        bool name =
            required name JD.bool
    in
    JD.succeed Modifiers
        |> bool "ctrlKey"
        |> bool "shiftKey"
        |> bool "altKey"
        |> bool "metaKey"


keyName : Decoder String
keyName =
    JD.field "key" JD.string


is : a -> a -> Bool
is =
    (==)


enter : msg -> Decoder msg
enter msg =
    JDX.when keyName (is "Enter") (noModifiersDown msg)


ctrlEnter : msg -> Decoder msg
ctrlEnter msg =
    JDX.when keyName (is "Enter") (onlyCtrlDown msg)


onKeyDown : List (Decoder a) -> H.Attribute a
onKeyDown =
    E.on "keydown" << JD.oneOf
