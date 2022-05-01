module Evergreen.V17.Types exposing (..)

import Browser
import Browser.Navigation
import Lamdera
import Time
import Url


type alias RoutingState =
    { key : Browser.Navigation.Key
    , url : Url.Url
    }


type Team
    = Blue
    | Red


type alias NewUserSettings =
    { username : String
    , team : Team
    , sessionId : Maybe Lamdera.SessionId
    , clientId : Maybe Lamdera.ClientId
    }


type alias User =
    { name : String
    , team : Team
    , cluegiver : Bool
    , sessionId : Maybe Lamdera.SessionId
    , clientId : Maybe Lamdera.ClientId
    }


type GridSize
    = SmallGrid
    | MediumGrid
    | LargeGrid


type alias NewGameSettings =
    { gridSize : GridSize
    , startingTeam : Team
    }


type CardAlignment
    = BlueCard
    | RedCard
    | Gray
    | Assassin


type alias Card =
    { word : String
    , team : CardAlignment
    , revealed : Bool
    }


type GameStatus
    = RedWon
    | BlueWon
    | RedTurn
    | BlueTurn


type alias Game =
    { id : String
    , users : List User
    , gridSize : GridSize
    , cards : List Card
    , gameStatus : GameStatus
    , words : List String
    , cardAlignments : List CardAlignment
    }


type FrontendModel
    = LoadingPage RoutingState
    | LandingPage RoutingState NewUserSettings
    | LobbyPage RoutingState User NewGameSettings
    | LoadedGamePage RoutingState User Game


type alias Session =
    { user : User
    , expires : Time.Posix
    }


type alias BackendModel =
    { games : List Game
    , words : List String
    , sessions : List ( Lamdera.SessionId, Session )
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
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
    | CheckSession Lamdera.SessionId Lamdera.ClientId
    | RenewSession User Lamdera.SessionId Lamdera.ClientId Time.Posix
    | RegisterUserSession User Lamdera.SessionId Lamdera.ClientId Time.Posix
    | CleanUpGames
    | CleanUpSessions Time.Posix
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | ActiveGame Game
    | ClientInfo Lamdera.SessionId Lamdera.ClientId (Maybe User)
