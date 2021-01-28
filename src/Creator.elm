module Creator exposing (main)

import Browser exposing (Document)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)

-- TODO: Convert "input" to "preparation" and add another set of input fields for "ingredients"
-- Types


type alias Model =
    { input : List String }


type Msg
    = GotInput Int String
    | AddInput
    | RemoveInput



-- JavaScript Interop


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Update


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotInput index input ->
            ( { model | input = updateInputs index input model }, Cmd.none )

        AddInput ->
            ( { model | input = addInput model.input }, Cmd.none )

        RemoveInput ->
            ( { model | input = removeInput model.input }, Cmd.none )


addInput : List String -> List String
addInput input =
    List.append input (List.singleton "")


removeInput : List String -> List String
removeInput input =
    if List.length input > 1 then
        List.take (List.length input - 1) input

    else
        input


updateInputs : Int -> String -> Model -> List String
updateInputs index input model =
    List.indexedMap
        (\i el ->
            if i == index then
                input

            else
                el
        )
        model.input



-- View


view : Model -> Document Msg
view model =
    { title = "Recipe Creator"
    , body =
        [ div []
            [ button [ onClick AddInput ] [ text "Add Input" ]
            , button [ onClick RemoveInput ] [ text "Remove Input" ]
            ]
        , div [] <| createInputFields model.input
        , div [] <| createOutputFields model.input
        ]
    }


createInputFields : List String -> List (Html Msg)
createInputFields currentInput =
    List.indexedMap (\i el -> inputField i el) currentInput


inputField : Int -> String -> Html Msg
inputField position currValue =
    input [ type_ "text", placeholder "Placeholder Text", value currValue, onInput (GotInput position) ] []


createOutputFields : List String -> List (Html msg)
createOutputFields currentInputs =
    List.map (\el -> outputField el) currentInputs


outputField : String -> Html msg
outputField input =
    p [] [ text input ]



-- Main


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { input = [ "" ] }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
