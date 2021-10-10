module Backend exposing (..)

import Cards exposing (generateCards, generateWords, pickTeams, shuffleCardAlignments, shuffleWords, updateCard)
import Lamdera exposing (ClientId, SessionId, clientConnected_, sendToBackend, sendToFrontend)
import Types exposing (..)
import Words exposing (words)



-- MODEL


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { games = []
      , words = words
      }
    , shuffleWords words
    )



-- UPDATE


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        ShuffledWords words ->
            ( { model | words = words }, Cmd.none )

        ShuffledCardTeams teams ->
            let
                newGame =
                    findGame (List.length model.games) model.games
            in
            case newGame of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    let
                        updatedGame =
                            updateGame model.games (replaceGameCards (generateCards (generateWords (List.length teams) model.words) teams) game)
                    in
                    ( { model | games = updatedGame }, Cmd.batch [ shuffleWords model.words, sendUpdatedGameToPlayers game.id updatedGame ] )

        SendGameToPlayers id ->
            ( model, sendUpdatedGameToPlayers id model.games )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        CreateNewGame newGameSettings ->
            let
                game =
                    generateGame model newGameSettings clientId

                teams =
                    pickTeams newGameSettings.gridSize newGameSettings.startingTeam
            in
            ( { model | games = model.games ++ [ game ] }, shuffleCardAlignments teams )

        GetPublicGames ->
            ( model, sendToFrontend clientId (PublicGames (List.filter (\g -> g.public) model.games)) )

        JoinGame id ->
            -- Join the user to the game
            case joinGame id clientId model.games of
                Nothing ->
                    -- Debug.todo "Send error in joining game to the front"
                    ( model, Cmd.none )

                Just games ->
                    -- Find the game you just joined
                    case findGame id games of
                        Nothing ->
                            -- Debug.todo "somethimg with this branch - can we avoid it?"
                            ( model, Cmd.none )

                        Just game ->
                            let
                                activeGame =
                                    game
                            in
                            --update the games in the model and let the joiner load the game
                            ( { model | games = games }, Cmd.batch [ sendUpdatedGameToPlayers activeGame.id games, sendToFrontend clientId (ActiveGame activeGame) ] )

        ChangeCardRevealedState card game ->
            let
                newGame =
                    updateGameCard card game
                        |> updateGame model.games
            in
            ( { model
                | games = newGame
              }
            , sendUpdatedGameToPlayers game.id newGame
            )

        EndTurn game newturn ->
            let
                newGame =
                    updateGameStatus newturn game
                        |> updateGame model.games
            in
            ( { model | games = newGame }, sendUpdatedGameToPlayers game.id newGame )

        EndGame team game ->
            let
                newGame =
                    updateGameStatus (getGameStatusFromWinningTeam team) game
                        |> updateGame model.games
            in
            case team of
                Blue ->
                    ( { model | games = newGame }, sendUpdatedGameToPlayers game.id newGame )

                Red ->
                    ( { model | games = newGame }, sendUpdatedGameToPlayers game.id newGame )

        -- Debug.todo "There needs to be a way to let all game.users know the game has changed"
        _ ->
            -- Debug.todo "Implement the other branches"
            ( model, Cmd.none )



-- UTILITIES


generateGame : Model -> NewGameSettings -> ClientId -> Game
generateGame model settings clientId =
    let
        newId =
            List.length model.games + 1

        status =
            getGameStatusFromStartingTeam settings.startingTeam
    in
    Game newId [ clientId ] settings.gridSize settings.public [] status [] []



-- (generateCards settings.gridSize settings.startingTeam)


joinGame : Int -> String -> List Game -> Maybe (List Game)
joinGame id clientId games =
    case findGame id games of
        Nothing ->
            Nothing

        Just game ->
            addUserToGame clientId game
                |> updateGame games
                |> Just


findGame : Int -> List Game -> Maybe Game
findGame id games =
    List.filter (\g -> g.id == id) games
        |> List.head


addUserToGame : String -> Game -> Game
addUserToGame user game =
    { game | users = game.users ++ [ user ] }


updateGame : List Game -> Game -> List Game
updateGame games newgame =
    List.map
        (\g ->
            if g.id == newgame.id then
                newgame

            else
                g
        )
        games


updateGameCard : Card -> Game -> Game
updateGameCard card game =
    { game | cards = updateCard card game.cards }


replaceGameCards : List Card -> Game -> Game
replaceGameCards cards game =
    { game | cards = cards }


updateGameStatus : GameStatus -> Game -> Game
updateGameStatus status game =
    { game | gameStatus = status }


sendUpdatedGameToPlayers : Int -> List Game -> Cmd msg
sendUpdatedGameToPlayers gameId games =
    case findGame gameId games of
        Nothing ->
            Cmd.none

        Just game ->
            Cmd.batch <| List.map (\u -> sendToFrontend u (ActiveGame game)) game.users


getGameStatusFromStartingTeam : Team -> GameStatus
getGameStatusFromStartingTeam startingTeam =
    case startingTeam of
        Blue ->
            BlueTurn

        _ ->
            RedTurn


getGameStatusFromWinningTeam : Team -> GameStatus
getGameStatusFromWinningTeam winningTeam =
    case winningTeam of
        Blue ->
            BlueWon

        Red ->
            RedWon
