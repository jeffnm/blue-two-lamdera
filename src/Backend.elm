module Backend exposing (..)

import Cards exposing (generateCards, generateWords, pickTeams, shuffleCardAlignments, shuffleWords, updateCard)
import Hashids exposing (..)
import Lamdera exposing (ClientId, SessionId, clientConnected_, onConnect, sendToBackend, sendToFrontend)
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
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { games = []
      , words = words
      , sessions = []
      }
    , shuffleWords words
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]



-- UPDATE


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        ClientConnected sessionId clientId ->
            -- Debug.todo "Check if the session is already active, and if so, send the user information to the client"
            ( model, sendToFrontend clientId (ClientInfo sessionId clientId (getSessionInfo sessionId model.sessions)) )

        ClientDisconnected sessionId clientId ->
            cleanupSessionsAndGames model sessionId clientId

        ShuffledWords words ->
            ( { model | words = words }, Cmd.none )

        ShuffledCardTeams game teams ->
            let
                updatedGame =
                    updateGame model.games (replaceGameCards (generateCards (generateWords (List.length teams) model.words) teams) game)
            in
            ( { model | games = updatedGame }
            , Cmd.batch
                [ shuffleWords model.words
                , sendUpdatedGameToPlayers game.id updatedGame
                ]
            )

        SendGameToPlayers id ->
            ( model, sendUpdatedGameToPlayers id model.games )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        RegisterUserSession user ->
            let
                newSession =
                    ( sessionId, user )
            in
            ( { model | sessions = model.sessions ++ [ newSession ] }, Cmd.none )

        CreateNewGame newGameSettings user ->
            let
                game =
                    generateGame model newGameSettings user

                teams =
                    pickTeams newGameSettings.gridSize newGameSettings.startingTeam
            in
            ( { model | games = model.games ++ [ game ] }, shuffleCardAlignments teams game )

        JoinGame id user ->
            -- Join the user to the game
            case joinGame id user model.games of
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
                            ( { model | games = games }
                            , Cmd.batch
                                [ sendUpdatedGameToPlayers activeGame.id games
                                , sendToFrontend clientId (ActiveGame activeGame)
                                ]
                            )

        ChangeUserTeam game user ->
            let
                newusers =
                    List.map
                        (\u ->
                            if u.name == user.name then
                                user

                            else
                                u
                        )
                        game.users

                newGame =
                    { game | users = newusers }

                newGames =
                    updateGame model.games newGame
            in
            ( { model | games = newGames }, Cmd.batch [ sendUpdatedGameToPlayers newGame.id newGames ] )

        ChangeCardRevealedState card game ->
            let
                newGame =
                    updateGameCard card game
                        |> isGameOver
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


hash =
    createHashidsContext "a salt for a clue" 5 "0123456789abcdefghijk"


generateId : List Int -> String
generateId ints =
    encodeList hash ints


decodeId : String -> List Int
decodeId hashids =
    decode hash hashids


generateGame : Model -> NewGameSettings -> User -> Game
generateGame model settings user =
    let
        newId =
            generateId ((List.length model.games + 1) :: [ String.length user.name ])

        status =
            getGameStatusFromStartingTeam settings.startingTeam
    in
    Game newId [ user ] settings.gridSize [] status [] []



-- (generateCards settings.gridSize settings.startingTeam)


joinGame : String -> User -> List Game -> Maybe (List Game)
joinGame id user games =
    case findGame id games of
        Nothing ->
            Nothing

        Just game ->
            addUserToGame user game
                |> updateGame games
                |> Just


findGame : String -> List Game -> Maybe Game
findGame id games =
    List.filter (\g -> g.id == id) games
        |> List.head


isGameOver : Game -> Game
isGameOver game =
    if didTeamWin game RedCard then
        { game | gameStatus = RedWon }

    else if didTeamWin game BlueCard then
        { game | gameStatus = BlueWon }

    else
        game


didTeamWin : Game -> CardAlignment -> Bool
didTeamWin game team =
    let
        teamscards =
            List.filter (\c -> c.team == team) game.cards

        revealedcards =
            List.filter (\c -> c.revealed) teamscards
                |> List.length
    in
    if revealedcards == List.length teamscards then
        True

    else
        False


addUserToGame : User -> Game -> Game
addUserToGame user game =
    if List.member user game.users then
        game

    else
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


sendUpdatedGameToPlayers : String -> List Game -> Cmd msg
sendUpdatedGameToPlayers gameId games =
    case findGame gameId games of
        Nothing ->
            Cmd.none

        Just game ->
            Cmd.batch <| List.map (\u -> sendToFrontend u (ActiveGame game)) (getClientIdsFromUsers game.users)


sessionExists : SessionId -> List ( SessionId, User ) -> Bool
sessionExists sessionId sessions =
    if (List.length <| List.filter (\( id, _ ) -> id == sessionId) sessions) > 0 then
        True

    else
        False


getSessionInfo : SessionId -> List ( SessionId, User ) -> Maybe User
getSessionInfo sessionId sessions =
    -- List.length sessions
    List.filter (\( id, _ ) -> id == sessionId) sessions
        |> List.map (\( _, user ) -> user)
        |> List.head


removeSessionAndClientIdsFromUserInAllGames : Model -> SessionId -> ClientId -> List Game
removeSessionAndClientIdsFromUserInAllGames model sessionId clientId =
    -- It might be a bad id to have maybe sessionids and maybe clientids - perhaps we should simply remove users when they disconnect
    List.map
        (\g ->
            removeSessionIdAndClientIdFromUserInOneGame g sessionId clientId
        )
        model.games


cleanupSessionsAndGames : Model -> SessionId -> ClientId -> ( Model, Cmd msg )
cleanupSessionsAndGames model sessionId clientId =
    let
        newGames =
            removeSessionAndClientIdsFromUserInAllGames model sessionId clientId

        newSessions =
            List.filter (\( id, _ ) -> id /= sessionId) model.sessions
    in
    ( { model | games = newGames, sessions = newSessions }, Cmd.batch <| List.map (\g -> sendUpdatedGameToPlayers g.id newGames) newGames )


removeSessionIdAndClientIdFromUserInOneGame : Game -> SessionId -> ClientId -> Game
removeSessionIdAndClientIdFromUserInOneGame game sessionId clientId =
    let
        newUsers =
            List.map
                (\u ->
                    case u.clientId of
                        Nothing ->
                            u

                        Just c ->
                            if c == clientId then
                                { u | clientId = Nothing, sessionId = Nothing }

                            else
                                u
                )
                game.users
    in
    { game | users = newUsers }


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


getClientIdsFromUsers : List User -> List ClientId
getClientIdsFromUsers users =
    List.filter
        (\u ->
            case u.clientId of
                Nothing ->
                    False

                Just _ ->
                    True
        )
        users
        |> List.map
            (\u ->
                case u.clientId of
                    Nothing ->
                        ""

                    Just clientId ->
                        clientId
            )
        |> List.filter (\u -> u /= "")
