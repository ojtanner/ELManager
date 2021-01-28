module Recipe exposing (Recipe, Preparation, Ingredient, IngredientGroup, IngredientList)

type alias Recipe =
    { ingredients: IngredientList
    , preparation: Preparation
    }

type alias Preparation =
    List String

type alias Ingredient = 
    { amount: String
    , name: String
    }

type alias IngredientGroup =
    { ingredients: List Ingredient
    , title: String
    }

type alias IngredientList =
    List IngredientGroup