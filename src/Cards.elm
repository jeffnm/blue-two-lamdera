module Cards exposing (..)

import Types exposing (..)


words : List String
words =
    Debug.todo "Get a list of words"


pickTeams : Int -> List Team
pickTeams gridSize =
    case gridSize of
        _ ->
            Debug.todo "List of Teams"


generateCards : List String -> List Team -> List Card
generateCards wordList teamsList =
    Debug.todo "Generate Cards"


shuffleList : List a -> List a
shuffleList list_a =
    Debug.todo "figure out how to shuffle stuff"
