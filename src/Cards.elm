module Cards exposing (..)

import List.Extra
import Random
import Random.List
import Types exposing (..)


generateWords : Int -> List String -> List String
generateWords numberOfWords words =
    words
        |> List.Extra.unique
        |> List.take numberOfWords


pickTeams : GridSize -> Team -> List CardAlignment
pickTeams gridSize startingTeam =
    case gridSize of
        SmallGrid ->
            -- 16 cards
            case startingTeam of
                Blue ->
                    List.repeat 4 RedCard
                        |> List.append (List.repeat 5 BlueCard)
                        |> List.append (List.repeat 6 Gray)
                        |> List.append (List.singleton Assassin)

                Red ->
                    List.repeat 5 RedCard
                        |> List.append (List.repeat 4 BlueCard)
                        |> List.append (List.repeat 6 Gray)
                        |> List.append (List.singleton Assassin)

        MediumGrid ->
            -- 25 cards
            case startingTeam of
                Blue ->
                    List.repeat 6 RedCard
                        |> List.append (List.repeat 7 BlueCard)
                        |> List.append (List.repeat 11 Gray)
                        |> List.append (List.singleton Assassin)

                Red ->
                    List.repeat 7 RedCard
                        |> List.append (List.repeat 6 BlueCard)
                        |> List.append (List.repeat 11 Gray)
                        |> List.append (List.singleton Assassin)

        LargeGrid ->
            -- 36 cards
            case startingTeam of
                Blue ->
                    List.repeat 8 RedCard
                        |> List.append (List.repeat 9 BlueCard)
                        |> List.append (List.repeat 14 Gray)
                        |> List.append (List.singleton Assassin)

                Red ->
                    List.repeat 9 RedCard
                        |> List.append (List.repeat 8 BlueCard)
                        |> List.append (List.repeat 14 Gray)
                        |> List.append (List.singleton Assassin)


cardCardAlignmentToString : CardAlignment -> String
cardCardAlignmentToString team =
    case team of
        BlueCard ->
            "blue"

        RedCard ->
            "red"

        Gray ->
            "lightgray"

        Assassin ->
            "black"


updateCard : Card -> List Card -> List Card
updateCard card cardList =
    List.map
        (\c ->
            if c.word == card.word then
                card

            else
                c
        )
        cardList


revealAllCards : List Card -> List Card
revealAllCards cardList =
    List.map (\c -> { c | revealed = True }) cardList


hideAllCards : List Card -> List Card
hideAllCards cardList =
    List.map (\c -> { c | revealed = False }) cardList


generateCards : List String -> List CardAlignment -> List Card
generateCards words teams =
    List.map2 (\w t -> Card w t False) words teams


shuffleWords : List String -> Cmd BackendMsg
shuffleWords words =
    -- Debug.todo "figure out how to shuffle stuff"
    Random.generate ShuffledWords (Random.List.shuffle words)


shuffleCardAlignments : List CardAlignment -> Cmd BackendMsg
shuffleCardAlignments teams =
    -- Debug.todo "figure out how to shuffle stuff"
    Random.generate ShuffledCardTeams (Random.List.shuffle teams)
