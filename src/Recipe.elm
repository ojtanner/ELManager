module Recipe exposing (..)

import Utils exposing (targetMap, replace)
import Html exposing (section)

type alias Group =
    { title: String
    , list: List String
    }

type alias Section = List Group

type SectionType
    = Ingredients
    | Preparation

type alias Recipe =
    {-
        Metadata Fields:
            - Reference aka from where did i steal the recipe: Maybe URL
            - Vegetarian | Vegan | Meat
            - Tag-list e.g. "sauce", "soup", "main dish"
            - Maybe ID (None if you create a new recipe, Some ID if you update an existing ID)
    -}
    { title: String
    , ingredients: Section
    , preparation: Section
    }

type alias Selector =
    { groupIndex : Int
    , listIndex : Int
    }

addGroup : Section -> Section
addGroup section =
    List.append section (List.singleton { title = "None", list = [""] })

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

updateGroupTitle : Section -> Int -> String -> Section
updateGroupTitle section groupIndex value =
    targetMap
        section
        groupIndex
        (\ group -> updateTitle group value) 

updateTitle : Group -> String -> Group
updateTitle group value =
    { group | title = value }

updateGroup : Group -> Int -> String -> Group
updateGroup group targetIndex value =
    { group | list = replace group.list targetIndex value }

addInput : Section -> Selector-> Section
addInput section { groupIndex } =
    targetMap
        section
        groupIndex
        (\ group -> addGroupInput group "")

addGroupInput : Group -> String -> Group
addGroupInput group placeholder =
    { group | list = List.append group.list (List.singleton placeholder) }


removeInput : Section -> Selector -> Section
removeInput section { groupIndex } =
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
        if length > 1 then
            List.take (length - 1) group.list
        else
            group.list
    }