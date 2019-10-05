module UI exposing (..)

import Basics.More exposing (updateWhenIdEq)
import Browser
import Html
import Html.Styled as H exposing (div)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Html.Styled.Keyed as HK
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE exposing (Value, encode, object)


col : List (H.Attribute msg) -> List (H.Html msg) -> H.Html msg
col attrs =
    H.div (A.class "flex flex-column" :: attrs)


colKeyed : List (H.Attribute msg) -> List ( String, H.Html msg ) -> H.Html msg
colKeyed attrs =
    HK.node "div" (A.class "flex flex-column" :: attrs)


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


btn3 : String -> msg -> List (H.Attribute msg) -> H.Html msg
btn3 title msg attrs =
    H.button ([ E.onClick msg, A.class "ph2 pv1 color-primary" ] ++ attrs) [ H.text title ]


btnDisabled : String -> H.Html msg
btnDisabled title =
    H.button [ A.class "ph2 pv1 gray", A.disabled True ] [ H.text title ]


btn2 : String -> msg -> H.Html msg
btn2 title msg =
    btn3 title msg []


submit : String -> List (H.Attribute msg) -> H.Html msg
submit title attrs =
    H.button (A.class "ph2 pv1 color-primary" :: attrs) [ H.text title ]


btn1 : String -> H.Html msg
btn1 title =
    H.button [ A.class "ph2 pv1 color-primary" ] [ H.text title ]


ipt3 : String -> (String -> msg) -> List (H.Attribute msg) -> H.Html msg
ipt3 val msg attrs =
    H.input ([ A.value val, E.onInput msg ] ++ attrs) []


ipt2 : String -> (String -> msg) -> H.Html msg
ipt2 val msg =
    H.input [ A.value val, E.onInput msg ] []


ipt1 : String -> H.Html msg
ipt1 val =
    H.input [ A.value val ] []
