module Creator.View exposing (..)

import Creator.Types exposing (..)
import Recipe exposing (..)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (placeholder, type_, value, selected)
import Html.Events exposing (onClick, onInput)
import Dict exposing (diff)

view : Model -> Document Msg
view model =
    { title = "Recipe Creator"
    , body =
        [ createSelectionButtons
            { typeList = [Meatarian, Vegetarian, Vegan]
            , selected = model.dietType
            , toString = dietTypeToString
            }
            SelectedDietType
        , createSelectionButtons
            { typeList = [(Hours 0), (Minutes 0)]
            , selected = model.cookingTime
            , toString = cookingTimeToString
            }
            SelectedCookingTime
        , createSelectionButtons
            { typeList = [Easy, Advanced, Complicated]
            , selected = model.difficulty
            , toString = difficultyToString
            }
            SelectedDifficulty
        , createSelectionButtons
            { typeList = [None, Online, Print]
            , selected = model.reference
            , toString = referenceToString
            }
            SelectedReference
        , createInputFields Preparation model.preparation
        , createInputFields Ingredients model.ingredients 
        ]
    }

type alias SelectADT a =
    { typeList : List a
    , selected : a
    , toString : (a -> String)
    }

createSelectionButtons : SelectADT a -> (a -> Msg) -> Html Msg
createSelectionButtons { typeList, selected, toString } message =
    div
        []
        <| List.map
            (\el ->
                msgButton el selected (toString el) message)
            typeList


msgButton : a -> a -> String -> (a -> Msg) -> Html Msg
msgButton a currentA strOfA message =
    button
        [ onClick (message a)
        , selected (a == currentA)
        ]
        [ text strOfA ]

createInputFields : SectionType -> Section -> Html Msg
createInputFields sType section =
    let
        buttons =
                [ button [ onClick (AddGroup sType) ] [ text "Add a Group" ]
                , button [ onClick (RemoveGroup sType) ] [ text "Remove a Group" ]
                ]
        groups =
            List.indexedMap
                (\groupIndex group ->
                    createGroupInputs sType group groupIndex)
                section

        content =
            (legend [] [ text "Section Placeholder" ]) :: groups
    in
    fieldset []
        [ div [] content
        , div [] buttons
        ]

-- its not the buttons
createGroupInputs : SectionType -> Group -> Int -> Html Msg
createGroupInputs sType group groupIndex =
    let
        buttons =
                [ button [ onClick (AddField sType groupIndex) ] [ text "Add Input" ]
                , button [ onClick (RemoveField sType groupIndex) ] [ text "Remove Input" ]
                ]
        inputFields =
            List.indexedMap (\listIndex el ->
                inputField sType { groupIndex = groupIndex , listIndex = listIndex } el)
                group.list

        titleInput =
            input 
                [ type_ "text"
                , placeholder "Group Title"
                , value group.title
                , onInput (GotTitleInput sType groupIndex)
                ]
                []

        content = titleInput :: inputFields
    in
    fieldset [] 
        [ div [] content
        , div [] buttons
        ]

inputField : SectionType -> Selector -> String -> Html Msg
inputField sType selector currValue =
    input [ type_ "text", placeholder "Placeholder Text", value currValue, onInput (GotInput sType selector) ] []
