module Cards exposing (..)

import List.Extra
import Types exposing (..)
import Words exposing (words)


generateWords : GridSize -> List String
generateWords gridSize =
    case gridSize of
        SmallGrid ->
            shuffleList words
                |> List.Extra.unique
                |> List.take 16

        MediumGrid ->
            shuffleList words
                |> List.Extra.unique
                |> List.take 25

        LargeGrid ->
            shuffleList words
                |> List.Extra.unique
                |> List.take 36


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
                        |> shuffleList

                Red ->
                    List.repeat 5 RedCard
                        |> List.append (List.repeat 4 BlueCard)
                        |> List.append (List.repeat 6 Gray)
                        |> List.append (List.singleton Assassin)
                        |> shuffleList

        MediumGrid ->
            -- 25 cards
            case startingTeam of
                Blue ->
                    List.repeat 6 RedCard
                        |> List.append (List.repeat 7 BlueCard)
                        |> List.append (List.repeat 11 Gray)
                        |> List.append (List.singleton Assassin)
                        |> shuffleList

                Red ->
                    List.repeat 7 RedCard
                        |> List.append (List.repeat 6 BlueCard)
                        |> List.append (List.repeat 11 Gray)
                        |> List.append (List.singleton Assassin)
                        |> shuffleList

        LargeGrid ->
            -- 36 cards
            case startingTeam of
                Blue ->
                    List.repeat 8 RedCard
                        |> List.append (List.repeat 9 BlueCard)
                        |> List.append (List.repeat 14 Gray)
                        |> List.append (List.singleton Assassin)
                        |> shuffleList

                Red ->
                    List.repeat 9 RedCard
                        |> List.append (List.repeat 8 BlueCard)
                        |> List.append (List.repeat 14 Gray)
                        |> List.append (List.singleton Assassin)
                        |> shuffleList


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


generateCards : GridSize -> Team -> List Card
generateCards gridSize startingTeam =
    List.map2 (\w t -> Card w t False) (generateWords gridSize) (pickTeams gridSize startingTeam)


shuffleList : List a -> List a
shuffleList list_a =
    -- Debug.todo "figure out how to shuffle stuff"
    list_a
