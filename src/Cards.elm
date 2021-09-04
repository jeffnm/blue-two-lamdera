module Cards exposing (..)

import List.Extra exposing (unique)
import Types exposing (..)
import Words exposing (words)


generateWords : GridSize -> List String -> List String
generateWords gridSize wordList =
    case gridSize of
        SmallGrid ->
            shuffleList wordList
                |> List.Extra.unique
                |> List.take 16

        MediumGrid ->
            shuffleList wordList
                |> List.Extra.unique
                |> List.take 25

        LargeGrid ->
            shuffleList wordList
                |> List.Extra.unique
                |> List.take 36


pickTeams : GridSize -> List Team
pickTeams gridSize =
    case gridSize of
        SmallGrid ->
            -- 16 cards
            List.repeat 4 Red
                |> List.append (List.repeat 4 Blue)
                |> List.append (List.repeat 7 Gray)
                |> List.append (List.singleton Assassin)
                |> shuffleList

        MediumGrid ->
            -- 25 cards
            List.repeat 6 Red
                |> List.append (List.repeat 6 Blue)
                |> List.append (List.repeat 12 Gray)
                |> List.append (List.singleton Assassin)
                |> shuffleList

        MediumGrid ->
            -- 36 cards
            List.repeat 8 Red
                |> List.append (List.repeat 8 Blue)
                |> List.append (List.repeat 15 Gray)
                |> List.append (List.singleton Assassin)
                |> shuffleList


generateCards : List String -> List Team -> List Card
generateCards wordList teamsList =
    List.map2 (\w t -> Card w t False) wordList teamsList


shuffleList : List a -> List a
shuffleList list_a =
    Debug.todo "figure out how to shuffle stuff"
