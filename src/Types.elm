module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , url : Url
    , user : Maybe User
    , activeGame : Maybe Game
    , newGameSettings : NewGameSettings
    , newUserSettings : NewUserSettings
    , publicGames : List Game
    }


type alias BackendModel =
    { games : List Game
    , words : List String
    , sessions : List (SessionId, User)
    }


type alias Game =
    { id : String
    , users : List User
    , gridSize : GridSize
    , public : Bool
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
    { public : Bool
    , gridSize : GridSize
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
    | NewUser
    | ChangeNewUserSettingUsername String
    | ChangeNewUserSettingTeam String
    | CreatingNewGame
    | LeavingGame
    | LoadingGame
    | JoiningGame String
    | ToggleNewGameSettingPublic Bool
    | ChangeNewGameSettingGridSize String
    | ChangeNewGameSettingTeam String
    | RevealingCard Card
    | EndingTurn
    | ToggleClueGiverStatus
    | ToggleTeam


type ToBackend
    = NoOpToBackend
    | RegisterUserSession User
    | CreateNewGame NewGameSettings User
    | JoinGame String User
    | LoadGame Game
    | LeaveGame Game User
    | GetPublicGames
    | ChangeCardRevealedState Card Game
    | EndTurn Game GameStatus
    | ChangeUserTeam Game User
    | EndGame Team Game


type BackendMsg
    = NoOpBackendMsg
    | ShuffledWords (List String)
    | ShuffledCardTeams Game (List CardAlignment)
    | SendGameToPlayers String
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | PublicGames (List Game)
    | UserGames
    | ActiveGame Game
    | ClientInfo SessionId ClientId (Maybe User)
