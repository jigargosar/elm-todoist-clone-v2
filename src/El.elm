module El exposing (Prop, attr, boolIpt, btn, btn3, col, el, ipt, rootEl, row, strIpt, tag, txt)

import Html
import Html.Styled as H exposing (Attribute, Html, node, text)
import Html.Styled.Attributes exposing (checked, class, type_, value)
import Html.Styled.Events as E


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


row : List (Prop msg) -> Prop msg
row props =
    el (fromHtmlAttrs [ class "flex flex-row" ] ++ props)


col : List (Prop msg) -> Prop msg
col props =
    el (fromHtmlAttrs [ class "flex flex-column" ] ++ props)


taggedEl : String -> List (Prop msg) -> Prop msg
taggedEl tagName props =
    el (Tag tagName :: props)


btn : List (Prop msg) -> Prop msg
btn =
    taggedEl "button"


btn3 : String -> msg -> List (Prop msg) -> Prop msg
btn3 title action props =
    btn (fromHtmlAttrs [ E.onClick action, class "ph2 pv1 blue" ] ++ (txt title :: props))


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


rootEl : List (Prop msg) -> Html.Html msg
rootEl props =
    El props
        |> toHtml
        |> H.toUnstyled


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
