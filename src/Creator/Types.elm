module Creator.Types exposing (..)

import Recipe exposing (..)

type alias Model = Recipe

type Msg
    = GotInput SectionType Selector String
    | AddField SectionType Int
    | RemoveField SectionType Int
    | AddGroup SectionType
    | RemoveGroup SectionType
    | GotTitleInput SectionType Int String
    | SelectedDietType DietType
    | SelectedDifficulty Difficulty
    | SelectedReference Reference
    | SelectedCookingTimeUnit CookingTime
    | GotCookingTimeInput Int
    | GotRerefenceInput String
    | GotRecipeTitleInput String
