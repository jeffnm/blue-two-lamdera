module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Time
import Url exposing (Url)


type FrontendModel
    = LoadingPage RoutingState
    | LandingPage RoutingState NewUserSettings
    | LobbyPage RoutingState User NewGameSettings
    | LoadedGamePage RoutingState User Game


type alias RoutingState =
    { key : Key
    , url : Url
    }


type alias BackendModel =
    { games : List Game
    , words : List String
    , sessions : List ( SessionId, Session )
    }


type alias Session =
    -- debug.todo: we should not store the users in two places - replace this one with the sessionid and then get the user for the client from the games list
    { user : User
    , expires : Time.Posix
    }


type alias Game =
    { id : String
    , users : List User
    , gridSize : GridSize
    , cards : List Card
    , gameStatus : GameStatus
    , words : List String
    , cardAlignments : List CardAlignment
    }


type alias NewUserSettings =
    { username : String
    , team : Team
    , sessionId : Maybe SessionId
    , clientId : Maybe ClientId
    }


type alias NewGameSettings =
    { gridSize : GridSize
    , startingTeam : Team
    }


type alias Card =
    { word : String
    , team : CardAlignment
    , revealed : Bool
    }


type alias User =
    { name : String
    , team : Team
    , cluegiver : Bool
    , sessionId : Maybe SessionId
    , clientId : Maybe ClientId
    }


type GridSize
    = SmallGrid
    | MediumGrid
    | LargeGrid


type Team
    = Blue
    | Red


type CardAlignment
    = BlueCard
    | RedCard
    | Gray
    | Assassin


type GameStatus
    = RedWon
    | BlueWon
    | RedTurn
    | BlueTurn


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | NewUser NewUserSettings
    | ChangeNewUserSettingUsername String
    | ChangeNewUserSettingTeam String
    | CreatingNewGame User NewGameSettings
    | LeavingGame User Game
    | ChangeNewGameSettingGridSize String
    | ChangeNewGameSettingTeam String
    | RevealingCard Card Game User
    | EndingTurn Game
    | ToggleClueGiverStatus User Game
    | ToggleTeam User (Maybe Game)


type ToBackend
    = NoOpToBackend
    | RegisterUser User
    | CreateNewGame NewGameSettings User
    | JoinGame String User
    | LoadGame Game
    | LeaveGame Game User
    | ChangeCardRevealedState Card Game
    | EndTurn Game GameStatus
    | ChangeUserTeam Game User
    | EndGame Team Game


type BackendMsg
    = NoOpBackendMsg
    | ShuffledWords (List String)
    | ShuffledCardTeams Game (List CardAlignment)
    | SendGameToPlayers String
    | CheckSession SessionId ClientId
    | RenewSession User SessionId ClientId Time.Posix
    | RegisterUserSession User SessionId ClientId Time.Posix
    | CleanUpGames
    | CleanUpSessions Time.Posix
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | ActiveGame Game
    | ClientInfo SessionId ClientId (Maybe User)
