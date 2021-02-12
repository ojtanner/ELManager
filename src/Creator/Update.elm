module Creator.Update exposing (..)

import Creator.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Recipe exposing (..)
import Creator.Types exposing (Msg(..))

updateSectionByType : SectionType -> Model -> String -> Selector -> (Section -> String -> Selector -> Section) -> Model
updateSectionByType sType model input selector fun =
    case sType of
       Preparation ->
            { model | preparation = fun model.preparation input selector }

       Ingredients ->
            { model | ingredients = fun model.ingredients input selector }

changeFieldAmountByType : SectionType -> (Section -> Selector -> Section) -> Model -> Selector -> Model
changeFieldAmountByType sType fun model selector =
    case sType of
       Preparation ->
            { model | preparation = fun model.preparation selector }

       Ingredients ->
            { model | ingredients = fun model.ingredients selector }

changeGroupAmountByType : SectionType -> (Section -> Section) -> Model -> Model
changeGroupAmountByType  sType fun model =
    case sType of
        Preparation ->
            { model | preparation = fun model.preparation }

        Ingredients ->
            { model | ingredients = fun model.ingredients }

updateGroupTitleByType : SectionType -> Model -> String -> Int -> (Section -> Int -> String -> Section) -> Model
updateGroupTitleByType sectionType model input groupIndex fun =
    case sectionType of
        Preparation ->
            { model | preparation = fun model.preparation groupIndex input }

        Ingredients ->
            { model | ingredients = fun model.ingredients groupIndex input }

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case Debug.log "msg: " msg of
        GotInput sType selector input ->
            ( updateSectionByType sType model input selector updateSection , Cmd.none )

        AddField sType index ->
            ( changeFieldAmountByType sType addInput model { groupIndex = index, listIndex = -1 }, Cmd.none )

        RemoveField sType index ->
            ( changeFieldAmountByType sType removeInput model { groupIndex = index, listIndex = -1 }, Cmd.none )
        
        AddGroup sType ->
            ( changeGroupAmountByType sType addGroup model, Cmd.none )

        RemoveGroup sType ->
            ( changeGroupAmountByType sType removeGroup model, Cmd.none )

        GotTitleInput sectionType groupIndex input ->
            ( updateGroupTitleByType sectionType model input groupIndex updateGroupTitle, Cmd.none )

        SelectedDietType dietType ->
            ( { model | dietType = dietType }, Cmd.none )

        SelectedDifficulty difficulty ->
            ( { model | difficulty = difficulty }, Cmd.none )

        SelectedReference reference ->
            ( { model | referenceType = reference }, Cmd.none )

        SelectedCookingTimeUnit unit ->
            ( { model | cookingTimeUnit = unit }, Cmd.none )

        GotCookingTimeInput time ->
            ( { model | cookingTime = time }, Cmd.none )

        GotRerefenceInput input ->
            ( { model | referenceInput = input }, Cmd.none )