module Creator.View exposing (..)

import Creator.Types exposing (..)
import Recipe exposing (..)
import Browser exposing (Document)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)

{-|
Convert Body from Html to Html.Styled: https://www.reddit.com/r/elm/comments/9e4qcu/is_elmcss_compatible_with_browserdocument/ 
-}
view : Model -> Document Msg
view model =
    let
       body = 
        Html.Styled.div
            []
            [ wrapper
                (createSelectionButtons
                    { typeList = [Meatarian, Vegetarian, Vegan]
                    , selected = model.dietType
                    , toString = dietTypeToString
                    }
                    SelectedDietType)
                "Select the Diet Type:"

            , wrapper 
                (createSelectionButtons
                    { typeList = [Easy, Advanced, Complicated]
                    , selected = model.difficulty
                    , toString = difficultyToString
                    }
                    SelectedDifficulty)
                "Select the Difficulty:"
            , wrapper
                (rerefenceInput model.referenceInput)
                "Add the Reference:"
            , wrapper
                (createSelectionButtons
                    { typeList = [None, Online, Print]
                    , selected = model.reference
                    , toString = referenceToString
                    }
                    SelectedReference)
                "Select  the Reference Type:"
            , createInputFields Preparation model.preparation
            , createInputFields Ingredients model.ingredients 
            ]
    in
    
    { title = "Recipe Creator"
    , body = [ Html.Styled.toUnstyled body]
    }

type alias SelectADT a =
    { typeList : List a
    , selected : a
    , toString : (a -> String)
    }

wrapper : Html msg -> String -> Html msg
wrapper wrappee title =
    div
        [ css 
            [ backgroundColor (rgb 255 255 0)
            ]
        ]
        [ h2 [] [ text title ]
        , wrappee
        ]

cookingTimeInput : Int -> Html Msg
cookingTimeInput currentTime =
    div
        []
        [ input
            [ type_ "number"
            , onInput (GotCookingTimeInput << Maybe.withDefault 1 << String.toInt)
            , value (String.fromInt currentTime)
            ]
            []
        ]

rerefenceInput : String -> Html Msg
rerefenceInput currentValue =
    div
        []
        [ input
            [ type_ "text"
            , onInput GotRerefenceInput
            , value currentValue
            ]
            []
        ]

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

        sectionTitle =
            case sType of
                Preparation -> "Preparation"

                Ingredients -> "Ingredients"

        content =
            (legend [] [ text sectionTitle ]) :: groups
    in
    fieldset []
        [ div [] content
        , div [] buttons
        ]

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
