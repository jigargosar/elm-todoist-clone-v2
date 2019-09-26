module Tachyons exposing (..)

import Array exposing (Array)
import Basics.More exposing (callWith)
import Css exposing (Rem, Style, hex, inherit, pct, rem)


pointer : Style
pointer =
    Css.cursor Css.pointer


bold =
    Css.fontWeight Css.bold


noStyle : Style
noStyle =
    Css.batch []


styleIf : Bool -> Style -> Style
styleIf bool style =
    if bool then
        style

    else
        noStyle


spacingArray : Array Rem
spacingArray =
    [ 0, 0.25, 0.5, 1, 1.5, 2, 4, 8 ]
        |> List.map rem
        |> Array.fromList


sp : Int -> Rem
sp idx =
    spacingArray |> Array.get idx |> Maybe.withDefault (rem (toFloat idx))


pv : Int -> Style
pv v =
    [ Css.paddingTop, Css.paddingBottom ]
        |> List.map (callWith (sp v))
        |> Css.batch


ph : Int -> Style
ph v =
    [ Css.paddingLeft, Css.paddingRight ]
        |> List.map (callWith (sp v))
        |> Css.batch


pa : Int -> Style
pa a =
    Css.padding <| sp a


ma : Int -> Style
ma a =
    Css.margin <| sp a


db =
    Css.display Css.block


w100 =
    Css.width <| pct 100


lhInherit =
    Css.lineHeight inherit


tal =
    Css.textAlign Css.left


hexLightBlue =
    hex "#96ccff"


bgLightBlue : Style
bgLightBlue =
    Css.backgroundColor hexLightBlue
