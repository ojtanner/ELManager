# Recipe Manager

Elm Frontend and TypeScript (not locked in) Backend

## Data Models

### Recipes

High level overview:

```
Recipe: {
    Ingredient-List
    Preparation
}
```
Ingredients should be either one list of all ingredients or multiple sublists of ingredients for separate parts of the recipe.

```
Ingredient-List: {
    List[Ingredient] | List[List[Ingredient]]
}
```

A single ingredient is an amount followed by a name. Specifying a specific subset of measurements seems overkill. 
Example: {"3 Tbsp", "Salt"}

```
Ingredient: {
    Amount
    Name
}
```

Preparation is an ordered list of steps.

```
Preparation: {
    List[String]
}
```# recipe_namager
