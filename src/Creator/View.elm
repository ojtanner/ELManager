module Creator.View exposing (..)

import Creator.Types exposing (..)
import Recipe exposing (..)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

{-|
Convert Body from Html to Html.Styled: https://www.reddit.com/r/elm/comments/9e4qcu/is_elmcss_compatible_with_browserdocument/ 
-}
view : Model -> Document Msg
view model =
    let
       body = 
            [ wrapper
                (titleInput model.title)
                "Add the Recipe Title:"
            , wrapper
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
                (cookingTimeInput model.cookingTime)
                "Add the Cooking Time in Minutes:"
            , wrapper
                (createSelectionButtons
                    { typeList = [NoReference, Online, Print]
                    , selected = model.referenceType
                    , toString = referenceToString
                    }
                    SelectedReference)
                "Select the Reference Type:"
            , toggleWrapper
                (rerefenceInput model.referenceType model.referenceInput)
                "Add the Reference:"
                (refTypeToBool model.referenceType)
            , createInputFields Preparation model.preparation
            , createInputFields Ingredients model.ingredients 
            ]
    in
    
    { title = "Recipe Creator"
    , body = body
    }

type alias SelectADT a =
    { typeList : List a
    , selected : a
    , toString : (a -> String)
    }

wrapper : Html msg -> String -> Html msg
wrapper wrappee title =
    div
        []
        [ h2 [] [ text title ]
        , wrappee
        ]

toggleWrapper : Html msg -> String -> Bool -> Html msg
toggleWrapper wrappee title isVisible =
    if isVisible then
        wrapper wrappee title
    else
        text ""

titleInput : String -> Html Msg
titleInput currentValue =
    div
        []
        [ input
            [ type_ "text"
            , onInput GotRecipeTitleInput
            , value currentValue
            ]
            []
        ]

refTypeToBool : Reference -> Bool
refTypeToBool refType =
    case refType of
        NoReference -> False

        Online -> True

        Print -> True

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

rerefenceInput : Reference -> String -> Html Msg
rerefenceInput refType currentValue =
    let
        nonempty =
            [ input
                [ type_ "text"
                , onInput GotRerefenceInput
                , value currentValue
                ]
                []
            ]

        refInput =
            case refType of
                NoReference ->
                    []
                
                Online ->
                    nonempty

                Print ->
                    nonempty

    in

    div
        []
        refInput

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

        title =
            input 
                [ type_ "text"
                , placeholder "Group Title"
                , value group.title
                , onInput (GotTitleInput sType groupIndex)
                ]
                []

        content = title :: inputFields
    in
    fieldset [] 
        [ div [] content
        , div [] buttons
        ]

inputField : SectionType -> Selector -> String -> Html Msg
inputField sType selector currValue =
    let
        constructor =
            case sType of
                Preparation -> GotPreparationInput

                Ingredients -> GotIngredientsInput
    in
    input [ type_ "text"
          , placeholder "Placeholder Text"
          , value currValue
          , onInput (constructor selector) ] []