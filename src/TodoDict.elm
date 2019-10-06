module TodoDict exposing (..)

import Tagged.Dict.More as TDM
import Todo exposing (Todo)
import TodoId exposing (TodoIdDict)


type alias TodoDict =
    TodoIdDict Todo


fromList : List Todo -> TodoDict
fromList =
    TDM.fromListBy .id
