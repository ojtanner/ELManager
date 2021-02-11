module Creator.Main exposing (main)

import Html exposing (..)
import Recipe exposing (..)
import Browser

import Creator.View exposing (..)
import Creator.Types exposing (..)
import Creator.Update exposing (..)

-- JavaScript Interop
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

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
            , dietType = Meatarian
            , difficulty = Easy
            , cookingTime = 0
            , reference = None
            , referenceInput = ""
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