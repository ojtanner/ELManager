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
    { preparation : Section
    , ingredients : Section
    , title : String
    }


type Msg
    = GotInput SectionType Selector String
    | AddField SectionType Int
    | RemoveField SectionType Int
    | AddGroup SectionType
    | RemoveGroup SectionType
    | GotTitleInput SectionType Int String


-- JavaScript Interop
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- Update
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


-- View
view : Model -> Document Msg
view model =
    { title = "Recipe Creator"
    , body =
        [ createInputFields Preparation model.preparation
        , createInputFields Ingredients model.ingredients 
        ]
    }

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


-- Main

init : () -> (Model, Cmd msg)
init _ =
    let
        prepGroup = 
            { title = "Group Placeholder"
            , list = ["First instruction goes here"]
            }

        ingredientGroup =
            { title = "Ingredients Placeholder"
            , list = ["1 tbsp garlic powder"]
            }

        ingredientSection =
            [ingredientGroup]

        prepSection =
            [prepGroup]
        model =
            { preparation = prepSection
            , ingredients = ingredientSection
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