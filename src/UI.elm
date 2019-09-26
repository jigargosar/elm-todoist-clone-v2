module UI exposing (..)

import Basics.More exposing (updateWhenIdEq)
import Browser
import Html
import Html.Styled as H exposing (div)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE exposing (Value, encode, object)


col : List (H.Attribute msg) -> List (H.Html msg) -> H.Html msg
col attrs =
    H.div (A.class "flex flex-column" :: attrs)


row : List (H.Attribute msg) -> List (H.Html msg) -> H.Html msg
row attrs =
    H.div (A.class "flex flex-row" :: attrs)


checkbox3 : Bool -> (Bool -> msg) -> List (H.Attribute msg) -> H.Html msg
checkbox3 bool onCheck attrs =
    H.input
        ([ A.type_ "checkbox"
         , A.checked bool
         , E.onCheck onCheck
         ]
            ++ attrs
        )
        []


btn2 : String -> msg -> H.Html msg
btn2 title msg =
    H.button [ E.onClick msg, A.class "ph2 pv1 nice-blue" ] [ H.text title ]
