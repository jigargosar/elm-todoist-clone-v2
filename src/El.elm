module El exposing (Prop, attr, btn, click, el, ipt, ipt3, iptCheck3, rootEl, tag, txt)

import Html exposing (Attribute, Html, node, text)
import Html.Attributes exposing (checked, type_, value)
import Html.Events as E


type alias Attrs msg =
    List (Attribute msg)


type alias Children msg =
    List (Html msg)


type Prop msg
    = Attr (Attribute msg)
    | Child (El msg)
    | Children (List (El msg))
    | Tag String


type El msg
    = El (List (Prop msg))
    | TxtEl String


attr : Attribute msg -> Prop msg
attr =
    Attr


el : List (Prop msg) -> Prop msg
el =
    Child << El


taggedEl : String -> List (Prop msg) -> Prop msg
taggedEl tagName props =
    el (Tag tagName :: props)


btn : List (Prop msg) -> Prop msg
btn =
    taggedEl "button"


ipt : List (Prop msg) -> Prop msg
ipt =
    taggedEl "input"


ipt3 : String -> (String -> msg) -> List (Prop msg) -> Prop msg
ipt3 val onInput props =
    ipt (List.map attr [ value val, E.onInput onInput ] ++ props)


iptCheck3 : Bool -> (Bool -> msg) -> List (Prop msg) -> Prop msg
iptCheck3 val onCheck props =
    ipt (List.map attr [ type_ "checkbox", checked val, E.onCheck onCheck ] ++ props)


tag : String -> Prop msg
tag =
    Tag


rootEl : List (Prop msg) -> Html msg
rootEl props =
    El props |> toHtml


click : msg -> Prop msg
click =
    attr << E.onClick


txt : String -> Prop msg
txt =
    Child << TxtEl


toHtml : El msg -> Html msg
toHtml el_ =
    case el_ of
        El props ->
            List.foldr
                (\prop ( t, a, c ) ->
                    case prop of
                        Attr v ->
                            ( t, v :: a, c )

                        Child v ->
                            ( t, a, toHtml v :: c )

                        Tag v ->
                            ( v, a, c )

                        Children list ->
                            ( t, a, List.map toHtml list ++ c )
                )
                ( "div", [], [] )
                props
                |> (\( tag_, attrs, children ) -> node tag_ attrs children)

        TxtEl v ->
            text v
