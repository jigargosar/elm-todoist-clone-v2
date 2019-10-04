module TodoMoreMenu exposing (..)

import Html.Styled exposing (text)
import TodoId exposing (TodoId)
import UI exposing (col)


type alias Model =
    Maybe TodoId


view model =
    Maybe.map viewHelp model


viewHelp _ =
    col []
        [ viewMenuItem "Edit"
        , viewMenuItem "Insert Above"
        , viewMenuItem "Insert Below"
        , viewPopupMenuItem "Schedule"
        ]


viewMenuItem title =
    col [] [ text title ]


viewPopupMenuItem title =
    col [] [ text title ]
