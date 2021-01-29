module Utils exposing (replace, targetMap)

replace : List a -> Int -> a -> List a
replace list targetIndex value =
    List.indexedMap
        (\index el ->
            if index == targetIndex then
                value
            else
                el)
        list

targetMap : List a -> Int -> (a -> a) -> List a
targetMap list targetIndex fun =
    List.indexedMap
        (\index el ->
            if index == targetIndex then
                fun el
            else
                el)
        list