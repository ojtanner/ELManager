# Recipe Manager

Should allow you to:

- Add a recipe
- Modify a recipe
- View all recipes
- View a specific recipe

## Data Structues

Not final.

### Recipe Creator

You have two sections:

- Ingredients
- Preparation

Each section is a list of groups.

A group represents a grouping of related steps.

Example: Ingredients of group "Sauce" are grouped together.

For now Ingredients are not separated by quantity and name.

```
type Section = List Group

type Group =
    { title : String
    , list : List String
    }
```

### Recipe

A recipe should have various meta-information fields.

Some of those fields are:

```
- DietType = Vegan | Vegetarian | Meatarian
- CookingTime = Hours Int | Minutes Int
- Difficulty = Easy | Advanced | Difficult
- Reference = None | Online (some Data Structure) | Print (some Data Structure)
- Tags = List String
```

Maybe add some pictures as well.
