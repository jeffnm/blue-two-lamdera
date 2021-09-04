module Cards exposing (..)

import List.Extra exposing (unique)
import Types exposing (..)
import Words exposing (words)


generateWords : Int -> List String -> List String
generateWords gridSize wordList =
    case gridSize of
        5 ->
            shuffleList wordList
                |> List.Extra.unique
                |> List.take 25

        _ ->
            Debug.todo "Get a list of words"


pickTeams : Int -> List Team
pickTeams gridSize =
    case gridSize of
        5 ->
            -- 25 cards
            List.repeat 6 Red
                |> List.append (List.repeat 6 Blue)
                |> List.append (List.repeat 12 Gray)
                |> List.append (List.singleton Assassin)
                |> shuffleList

        _ ->
            Debug.todo "List of Teams"


generateCards : List String -> List Team -> List Card
generateCards wordList teamsList =
    List.map2 (\w t -> Card w t False) wordList teamsList


shuffleList : List a -> List a
shuffleList list_a =
    Debug.todo "figure out how to shuffle stuff"
