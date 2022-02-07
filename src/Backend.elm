module Backend exposing (..)

import Cards exposing (generateCards, generateWords, pickTeams, shuffleCardAlignments, shuffleWords, updateCard)
import Hashids
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Task
import Time
import Time.Extra as Time
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
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect CheckSession
        , Lamdera.onDisconnect ClientDisconnected
        ]



-- UPDATE


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        CheckSession sid cid ->
            model.sessions
                |> getSessionInfo sid
                |> Maybe.map (\user -> ( model, sendToFrontend cid (ClientInfo sid cid (Just { user | clientId = Just cid })) ))
                |> Maybe.withDefault ( model, sendToFrontend cid (ClientInfo sid cid Nothing) )

        RenewSession _ sid cid now ->
            let
                updatedSession =
                    model.sessions
                        |> List.filter (\( s, _ ) -> s == sid)
                        |> List.map (\( s, sd ) -> ( s, { user = sd.user, expires = now |> Time.add Time.Hour 1 Time.utc } ))

                newSessions =
                    model.sessions
                        |> List.filter (\( s, _ ) -> s /= sid)
                        |> List.append updatedSession
            in
            ( { model | sessions = newSessions }, Time.now |> Task.perform (always (CheckSession sid cid)) )

        RegisterUserSession user sessionId _ now ->
            let
                newSession =
                    ( sessionId, Session user (now |> Time.add Time.Hour 1 Time.utc) )
            in
            ( { model | sessions = model.sessions ++ [ newSession ] }, Cmd.batch [ Time.now |> Task.perform CleanUpSessions, Time.now |> Task.perform (always CleanUpGames) ] )

        CleanUpSessions now ->
            cleanupSessions model now

        CleanUpGames ->
            cleanupGames model

        ClientConnected sessionId clientId ->
            -- Check that if a session is active and send the user data to the new client
            ( model, sendToFrontend clientId (ClientInfo sessionId clientId (getSessionInfo sessionId model.sessions)) )

        ClientDisconnected _ _ ->
            ( model, Cmd.batch [ Time.now |> Task.perform CleanUpSessions, Time.now |> Task.perform (always CleanUpGames) ] )

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

        RegisterUser user ->
            ( model, Time.now |> Task.perform (RegisterUserSession user sessionId clientId) )

        CreateNewGame newGameSettings user ->
            let
                game =
                    generateGame model newGameSettings user

                teams =
                    pickTeams newGameSettings.gridSize newGameSettings.startingTeam
            in
            ( { model | games = model.games ++ [ game ] }, Cmd.batch [ shuffleCardAlignments teams game, renewSession user sessionId clientId ] )

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
                            -- update the games in the model and batch Cmd msg
                            ( { model | games = games }
                            , Cmd.batch
                                [ sendUpdatedGameToPlayers activeGame.id games
                                , sendToFrontend clientId (ActiveGame activeGame)
                                , renewSession user sessionId clientId
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

        LeaveGame game user ->
            let
                newGame =
                    updateGameRemoveUser user game

                gamesList =
                    if checkThatGameHasUsers newGame then
                        updateGame model.games newGame

                    else
                        removeGame model.games newGame
            in
            ( { model | games = gamesList }, sendUpdatedGameToPlayers game.id gamesList )

        LoadGame game ->
            ( model, sendUpdatedGameToPlayers game.id model.games )



-- UTILITIES


hash : Hashids.Context
hash =
    Hashids.createHashidsContext "a salt for a clue" 5 "0123456789abcdefghijk"


generateId : List Int -> String
generateId ints =
    Hashids.encodeList hash ints


decodeId : String -> List Int
decodeId hashids =
    Hashids.decode hash hashids


generateGame : Model -> NewGameSettings -> User -> Game
generateGame model settings user =
    let
        newId =
            generateId [ List.length model.games + 1, String.length user.name ]

        status =
            getGameStatusFromStartingTeam settings.startingTeam
    in
    Game newId [ user ] settings.gridSize [] status [] []



-- (generateCards settings.gridSize settings.startingTeam)


joinGame : String -> User -> List Game -> Maybe (List Game)
joinGame id user games =
    findGame id games
        |> Maybe.andThen
            (\g ->
                addUserToGame user g
                    |> updateGame games
                    |> Just
            )


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


removeGame : List Game -> Game -> List Game
removeGame games oldgame =
    List.filter (\g -> g.id /= oldgame.id) games


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


checkThatGameHasUsers : Game -> Bool
checkThatGameHasUsers game =
    if List.isEmpty game.users then
        False

    else
        True


replaceGameCards : List Card -> Game -> Game
replaceGameCards cards game =
    { game | cards = cards }


updateGameStatus : GameStatus -> Game -> Game
updateGameStatus status game =
    { game | gameStatus = status }


updateGameRemoveUser : User -> Game -> Game
updateGameRemoveUser user game =
    let
        newusers =
            List.filter (\u -> u.name /= user.name) game.users
    in
    { game | users = newusers }


sendUpdatedGameToPlayers : String -> List Game -> Cmd msg
sendUpdatedGameToPlayers gameId games =
    case findGame gameId games of
        Nothing ->
            Cmd.none

        Just game ->
            Cmd.batch <| List.map (\u -> sendToFrontend u (ActiveGame game)) (getClientIdsFromUsers game.users)


renewSession : User -> SessionId -> ClientId -> Cmd BackendMsg
renewSession user sid cid =
    Time.now |> Task.perform (RenewSession user sid cid)


sessionExists : SessionId -> List ( SessionId, User ) -> Bool
sessionExists sessionId sessions =
    if (List.length <| List.filter (\( id, _ ) -> id == sessionId) sessions) > 0 then
        True

    else
        False


getSessionInfo : SessionId -> List ( SessionId, Session ) -> Maybe User
getSessionInfo sessionId sessions =
    -- List.length sessions
    List.filter (\( id, _ ) -> id == sessionId) sessions
        |> List.map (\( _, s ) -> s.user)
        |> List.head


removeSessionAndClientIdsFromUserInAllGames : Model -> SessionId -> ClientId -> List Game
removeSessionAndClientIdsFromUserInAllGames model sessionId _ =
    -- It might be a bad id to have maybe sessionids and maybe clientids - perhaps we should simply remove users when they disconnect
    List.map
        (\g ->
            updateGameRemoveUserBySessionId g sessionId
        )
        model.games


updateGameRemoveUserBySessionId : Game -> SessionId -> Game
updateGameRemoveUserBySessionId game sessionId =
    { game
        | users =
            game.users
                |> List.filter
                    (\u ->
                        case u.sessionId of
                            Nothing ->
                                False

                            Just s ->
                                if s /= sessionId then
                                    True

                                else
                                    False
                    )
    }


cleanupSessions : Model -> Time.Posix -> ( Model, Cmd msg )
cleanupSessions model now =
    let
        newSessions =
            List.filter (\( _, session ) -> Time.posixToMillis session.expires > Time.posixToMillis now) model.sessions
    in
    -- ( { model | sessions = newSessions }, Cmd.batch <| List.map (\g -> sendUpdatedGameToPlayers g.id newGames) newGames )
    ( { model | sessions = newSessions }, Cmd.none )


cleanupGames : Model -> ( Model, Cmd msg )
cleanupGames model =
    let
        newGames =
            model.games
                |> List.concatMap
                    (\g ->
                        List.map
                            (\u ->
                                case u.sessionId of
                                    Nothing ->
                                        updateGameRemoveUser u g

                                    Just sid ->
                                        if List.member sid sessionIdsInGamesWithActiveSessions then
                                            g

                                        else
                                            updateGameRemoveUserBySessionId g sid
                            )
                            g.users
                    )
                |> List.filter (\g -> List.length g.users > 0)

        allUsersInGames =
            model.games
                |> List.concatMap (\g -> g.users)

        allUserSessionIds =
            allUsersInGames
                |> List.map
                    (\u ->
                        case u.sessionId of
                            Nothing ->
                                ""

                            Just sid ->
                                sid
                    )

        sessionIdsInGamesWithActiveSessions =
            model.sessions
                |> List.filter (\( sid, _ ) -> List.member sid allUserSessionIds)
                |> List.map (\( sid, _ ) -> sid)
    in
    ( { model | games = newGames }, Cmd.none )



-- removeSessionIdAndClientIdFromUserInOneGame : Game -> SessionId -> ClientId -> Game
-- removeSessionIdAndClientIdFromUserInOneGame game sessionId clientId =
--     let
--         newUsers =
--             List.map
--                 (\u ->
--                     case u.clientId of
--                         Nothing ->
--                             u
--                         Just c ->
--                             if c == clientId then
--                                 { u | clientId = Nothing, sessionId = Nothing }
--                             else
--                                 u
--                 )
--                 game.users
--     in
--     { game | users = newUsers }


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
