module Recipe exposing (..)

import Utils exposing (targetMap, replace)
import Html exposing (section)

type alias Group =
    { title: String
    , list: List String
    }

type alias Section = List Group

type alias Preparation = Section

type alias Ingredients = Section

type alias Recipe =
    { title: String
    , ingredients: Ingredients
    , preparation: Preparation
    }

type alias Selector =
    { groupIndex : Int
    , listIndex : Int
    }

addGroup : Section -> List String -> Section
addGroup section placeholder =
    List.append section (List.singleton { title = "None", list = placeholder })

removeGroup : Section -> Section
removeGroup section =
    let
        length = List.length section
    in
    if length > 1 then
        List.take (length - 1) section
    else
        section

updateSection : Section -> String -> Selector -> Section
updateSection section input { groupIndex, listIndex } =
    targetMap
        section
        groupIndex
        (\ group -> updateGroup group listIndex input)

updateGroup : Group -> Int -> String -> Group
updateGroup group targetIndex value =
    { group | list = replace group.list targetIndex value }

addInput : Section -> Selector -> String -> Section
addInput section { groupIndex, listIndex } placeholder =
    targetMap
        section
        groupIndex
        (\ group -> addGroupInput group placeholder)

addGroupInput : Group -> String -> Group
addGroupInput group placeholder =
    { group | list = List.append group.list (List.singleton placeholder) }


removeInput : Section -> Selector -> Section
removeInput section { groupIndex, listIndex } =
    targetMap
        section
        groupIndex
        (\group -> removeGroupInput group)

removeGroupInput : Group -> Group
removeGroupInput group =
    let
        length = List.length group.list
    in
    { group | list =
        case length > 1 of
            True ->
                List.take (length - 1) group.list
            False -> 
                group.list
    }