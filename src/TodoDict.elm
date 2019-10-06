module TodoDict exposing (TodoDict, fromList, upsertNewer)

import Tagged.Dict as TaggedDict
import Tagged.Dict.More as TDM
import Todo exposing (Todo)
import TodoId exposing (TodoIdDict)


type alias TodoDict =
    TodoIdDict Todo


fromList : List Todo -> TodoDict
fromList =
    TDM.fromListBy .id


upsertIfNewerHelp : Todo -> TodoDict -> TodoDict
upsertIfNewerHelp new dict =
    case TaggedDict.get new.id dict of
        Nothing ->
            TaggedDict.insert new.id new dict

        Just old ->
            if Todo.isNewerThan old new then
                TaggedDict.insert new.id new dict

            else
                dict


upsertNewer : List Todo -> TodoDict -> TodoDict
upsertNewer todoList todoDict =
    List.foldl upsertIfNewerHelp todoDict todoList
