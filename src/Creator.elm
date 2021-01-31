module Creator exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Recipe exposing (..)
import List
import Browser

-- Types
type alias Model =
    { preparation : Preparation
    , title : String
    }


type Msg
    = GotPreparationInput Selector String
    | AddPreparationField Int
    | RemovePreparationField Int
    | AddGroup
    | RemoveGroup


-- JavaScript Interop
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- Update
update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotPreparationInput selector input ->
            ( { model | preparation =  updateSection model.preparation input selector }, Cmd.none )

        AddPreparationField index ->
            ( { model | preparation = addInput model.preparation { groupIndex = index, listIndex = -1 } "empty" }, Cmd.none )

        RemovePreparationField index ->
            ( { model | preparation = removeInput model.preparation { groupIndex = index, listIndex = -1 } }, Cmd.none )
        
        AddGroup ->
            ( { model | preparation = addGroup model.preparation ["New Group Placeholder"] }, Cmd.none )

        RemoveGroup ->
            ( { model | preparation = removeGroup model.preparation }, Cmd.none )


-- View

-- Throws INITIALISATION ERROR
-- Error is located here.
view : Model -> Document Msg
view model =
    { title = "Recipe Creator"
    , body =
        [ createInputFields model.preparation ]
    }

createInputFields : Section -> Html Msg
createInputFields section =
    let
        buttons =
                [ button [ onClick AddGroup ] [ text "Add a Group" ]
                , button [ onClick RemoveGroup ] [ text "Remove a Group" ]
                ]
        groups =
            List.indexedMap
                (\groupIndex group ->
                    createGroupInputs group groupIndex)
                section

        content =
            (legend [] [ text "Section Placeholder" ]) :: groups
    in
    fieldset []
        [ div [] content
        , div [] buttons
        ]

-- its not the buttons
createGroupInputs : Group -> Int -> Html Msg
createGroupInputs group groupIndex =
    let
        buttons =
                [ button [ onClick (AddPreparationField groupIndex) ] [ text "Add Input" ]
                , button [ onClick (RemovePreparationField groupIndex) ] [ text "Remove Input" ]
                ]
        inputFields =
            List.indexedMap (\listIndex el ->
                inputField { groupIndex = groupIndex , listIndex = listIndex } el)
                group.list
        content = (legend [] [ text group.title ]) :: inputFields
    in
    fieldset [] 
        [ div [] content
        , div [] buttons
        ]

inputField : Selector -> String -> Html Msg
inputField selector currValue =
    input [ type_ "text", placeholder "Placeholder Text", value currValue, onInput (GotPreparationInput selector) ] []


-- Main

init : () -> (Model, Cmd msg)
init _ =
    let
        prepGroup = 
            { title = "Group Placeholder"
            , list = ["First instruction goes here"]
            }
        prepSection =
            [prepGroup]
        model =
            { preparation = prepSection
            , title = "Placeholder Recipe Title"
            }
    in
    ( model
    , Cmd.none
    )

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }