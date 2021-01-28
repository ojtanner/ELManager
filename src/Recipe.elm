module Recipe exposing (Recipe, Ingredients, Preparation)

type alias ListGroup = 
    { title: String
    , list: List String
    }

type alias Preparation = ListGroup

type Ingredients = List ListGroup

type alias Recipe =
    { title: String
    , ingredients: Ingredients
    , preparation: Preparation
    }