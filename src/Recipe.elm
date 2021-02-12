module Recipe exposing (..)

import Utils exposing (targetMap, replace)
import Html exposing (section)

-- Types
type alias Group =
    { title: String
    , list: List String
    }

type alias Section = List Group

type SectionType
    = Ingredients
    | Preparation

type DietType
    = Vegan
    | Vegetarian
    | Meatarian

dietTypeToString : DietType -> String
dietTypeToString dietType =
    case dietType of
        Vegan -> "Vegan"

        Vegetarian -> "Vegetarian" 

        Meatarian -> "Meatarian"

type Difficulty
    = Easy
    | Advanced
    | Complicated

difficultyToString : Difficulty -> String
difficultyToString difficulty =
    case difficulty of
        Easy -> "Easy"

        Advanced -> "Advanced"

        Complicated -> "Complicated"

type Reference
    = NoReference
    | Online
    | Print

referenceToString : Reference -> String
referenceToString reference =
    case reference of
        NoReference -> "No Reference"

        Online -> "Online"

        Print -> "Print"

type CookingTime
    = Minutes
    | Hours

cookingTimeToString : CookingTime -> String
cookingTimeToString cookingTime =
    case cookingTime of
        Minutes -> "Minutes"

        Hours -> "Hours"

type alias Recipe =
    { title: String
    , dietType: DietType
    , cookingTime: Int
    , cookingTimeUnit: CookingTime
    , difficulty: Difficulty
    , referenceType: Reference
    , referenceInput: String
    , ingredients: Section
    , preparation: Section
    , id: Maybe Int
    }

type alias Selector =
    { groupIndex : Int
    , listIndex : Int
    }

-- Functions
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