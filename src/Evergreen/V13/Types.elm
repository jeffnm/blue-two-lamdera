module Evergreen.V13.Types exposing (..)

import Browser
import Browser.Navigation
import Lamdera
import Time
import Url


type Team
    = Blue
    | Red


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


type alias NewGameSettings =
    { gridSize : GridSize
    , startingTeam : Team
    }


type alias NewUserSettings =
    { username : String
    , team : Team
    , sessionId : Maybe Lamdera.SessionId
    , clientId : Maybe Lamdera.ClientId
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , user : Maybe User
    , activeGame : Maybe Game
    , newGameSettings : NewGameSettings
    , newUserSettings : NewUserSettings
    }


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
    | NewUser
    | ChangeNewUserSettingUsername String
    | ChangeNewUserSettingTeam String
    | CreatingNewGame
    | LeavingGame
    | LoadingGame
    | JoiningGame String
    | ChangeNewGameSettingGridSize String
    | ChangeNewGameSettingTeam String
    | RevealingCard Card
    | EndingTurn
    | ToggleClueGiverStatus
    | ToggleTeam


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
    | UserGames
    | ActiveGame Game
    | ClientInfo Lamdera.SessionId Lamdera.ClientId (Maybe User)
