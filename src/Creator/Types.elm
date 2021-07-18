module Creator.Types exposing (..)

import Recipe exposing (..)

type alias Model = Recipe

type Msg
    = GotPreparationInput Selector String
    | GotIngredientsInput Selector String
    | AddField SectionType Int
    | RemoveField SectionType Int
    | AddGroup SectionType
    | RemoveGroup SectionType
    | GotTitleInput SectionType Int String
    | SelectedDietType DietType
    | SelectedDifficulty Difficulty
    | SelectedReference Reference
    | GotCookingTimeInput Int
    | GotRerefenceInput String
    | GotRecipeTitleInput String
