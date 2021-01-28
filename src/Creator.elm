module Creator exposing (main)

import Browser exposing (Document)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Recipe exposing (Preparation)
import Html exposing (fieldset)
import Html exposing (legend)

-- TODO: Convert "input" to "preparation" and add another set of input fields for "ingredients"
-- Types


type alias Model =
    { preparation : Preparation
    , title : String
    }


type Msg
    = GotPreparationInput Int String
    | AddPreparationField
    | RemovePreparationField



-- JavaScript Interop


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Update


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotPreparationInput index input ->
            ( { model | preparation = updateInputs index input model.preparation }, Cmd.none )

        AddPreparationField ->
            ( { model | preparation = addInput model.preparation }, Cmd.none )

        RemovePreparationField ->
            ( { model | preparation = removeInput model.preparation }, Cmd.none )


addInput : Preparation -> Preparation
addInput preparation =
    { preparation | list = List.append preparation.list (List.singleton "") }
    

removeInput : Preparation -> Preparation
removeInput preparation =
    { preparation | list = 
        if List.length preparation.list > 1 then
            List.take (List.length preparation.list - 1) preparation.list
        else
            preparation.list
    }


updateInputs : Int -> String -> Preparation -> Preparation
updateInputs index input prep =
    let
        prepInput = 
            List.indexedMap
                (\i el ->
                    if i == index then
                        input
                    else
                        el
                )
                prep.list
    in
    { title = prep.title
    , list = prepInput
    }



-- View


view : Model -> Document Msg
view model =
    { title = "Recipe Creator"
    , body =
        [ div []
            [ button [ onClick AddPreparationField ] [ text "Add Input" ]
            , button [ onClick RemovePreparationField ] [ text "Remove Input" ]
            ]
        , div [] [ createInputFields model.preparation.list ]
        , div [] <| createOutputFields model.preparation.list
        ]
    }


createInputFields : List String -> Html Msg
createInputFields currentInput =
    let
        inputFields = List.indexedMap (\i el -> inputField i el) currentInput
        content = (legend [] [ text "Placeholder" ]) :: inputFields
    in
    fieldset [] content

inputField : Int -> String -> Html Msg
inputField position currValue =
    input [ type_ "text", placeholder "Placeholder Text", value currValue, onInput (GotPreparationInput position) ] []


createOutputFields : List String -> List (Html msg)
createOutputFields currentInputs =
    List.map (\el -> outputField el) currentInputs


outputField : String -> Html msg
outputField input =
    p [] [ text input ]



-- Main

init : () -> (Model, Cmd msg)
init _ =
    (
        { title = "Placeholder"
        , preparation = 
            { title = "Placeholder"
            , list = []
            } 
        }
    ,   Cmd.none
    )

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
