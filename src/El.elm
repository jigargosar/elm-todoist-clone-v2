module El exposing (Prop, attr, button, click, el, rootEl, txt)

import Html exposing (Attribute, Html, node, text)
import Html.Events as E


type alias Attrs msg =
    List (Attribute msg)


type alias Children msg =
    List (Html msg)


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


el : List (Prop msg) -> Prop msg
el =
    Child << El


button : List (Prop msg) -> Prop msg
button props =
    el (Tag "button" :: props)


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
                )
                ( "div", [], [] )
                props
                |> (\( tag, attrs, children ) -> node tag attrs children)

        TxtEl v ->
            text v
