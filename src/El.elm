module El exposing (El, Prop, click, el, rootEl, toHtml, txt)

import Html exposing (Attribute, Html, node, text)
import Html.Events as E


type Prop msg
    = Attr (Attribute msg)
    | Child (El msg)
    | Tag String


el : List (Prop msg) -> Prop msg
el =
    Child << El


rootEl : List (Prop msg) -> Html msg
rootEl props =
    El props |> toHtml


click : msg -> Prop msg
click =
    Attr << E.onClick


txt : String -> Prop msg
txt =
    Child << TxtEl


type El msg
    = El (List (Prop msg))
    | TxtEl String


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
