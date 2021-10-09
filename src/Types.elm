module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , user : Maybe User
    , activeGame : Maybe Game
    , newGameSettings : NewGameSettings
    , newUserSettings : NewUserSettings
    , publicGames : List Game
    }


type alias BackendModel =
    { games : List Game
    , words : List String
    }


type alias Game =
    { id : Int
    , users : List String
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
    , games : List Game
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
    | JoiningGame Int
    | ToggleNewGameSettingPublic Bool
    | ChangeNewGameSettingGridSize String
    | RevealingCard Card
    | EndingTurn
    | ToggleClueGiverStatus


type ToBackend
    = NoOpToBackend
    | CreateNewGame NewGameSettings
    | JoinGame Int
    | LoadGame Game
    | GetPublicGames
    | GetUserGames
    | ChangeCardRevealedState Card Game
    | EndTurn Game GameStatus
    | ChangeUserTeam
    | EndGame Team Game


type BackendMsg
    = NoOpBackendMsg
    | ShuffledWords (List String)
    | ShuffledCardTeams (List CardAlignment)
    | SendGameToPlayers Int


type ToFrontend
    = NoOpToFrontend
    | PublicGames (List Game)
    | UserGames
    | ActiveGame Game
