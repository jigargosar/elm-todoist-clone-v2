module El exposing (Prop, attr, boolIpt, btn, btn3, el, ipt, rootEl, strIpt, tag, txt)

import Html exposing (Attribute, Html, node, text)
import Html.Attributes exposing (checked, type_, value)
import Html.Events as E


type alias Attrs msg =
    List (Attribute msg)


type Prop msg
    = Attr (Attribute msg)
    | Child (El msg)
    | Tag String


type El msg
    = El (List (Prop msg))
    | TxtEl String


attr : Attribute msg -> Prop msg
attr =
    Attr


fromHtmlAttrs : List (Attribute msg) -> List (Prop msg)
fromHtmlAttrs =
    List.map attr


el : List (Prop msg) -> Prop msg
el =
    Child << El


taggedEl : String -> List (Prop msg) -> Prop msg
taggedEl tagName props =
    el (Tag tagName :: props)


btn : List (Prop msg) -> Prop msg
btn =
    taggedEl "button"


btn3 : String -> msg -> List (Prop msg) -> Prop msg
btn3 title action props =
    btn ((attr <| E.onClick action) :: txt title :: props)


ipt : List (Prop msg) -> Prop msg
ipt =
    taggedEl "input"


strIpt : String -> (String -> msg) -> List (Prop msg) -> Prop msg
strIpt val onInput props =
    ipt
        (fromHtmlAttrs [ value val, E.onInput onInput ]
            ++ props
        )


boolIpt : Bool -> (Bool -> msg) -> List (Prop msg) -> Prop msg
boolIpt val onCheck props =
    ipt
        (fromHtmlAttrs [ type_ "checkbox", checked val, E.onCheck onCheck ]
            ++ props
        )


tag : String -> Prop msg
tag =
    Tag


rootEl : List (Prop msg) -> Html msg
rootEl props =
    El props |> toHtml


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
                )
                ( "div", [], [] )
                props
                |> (\( tag_, attrs, children ) -> node tag_ attrs children)

        TxtEl v ->
            text v
