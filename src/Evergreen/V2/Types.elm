module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Url


type Team
    = Blue
    | Red


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
    { id : Int
    , users : List String
    , gridSize : GridSize
    , public : Bool
    , cards : List Card
    , gameStatus : GameStatus
    , words : List String
    , cardAlignments : List CardAlignment
    }


type alias User =
    { name : String
    , team : Team
    , cluegiver : Bool
    , games : List Game
    }


type alias NewGameSettings =
    { public : Bool
    , gridSize : GridSize
    , startingTeam : Team
    }


type alias NewUserSettings =
    { username : String
    , team : Team
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
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
    | JoiningGame Int
    | ToggleNewGameSettingPublic Bool
    | ChangeNewGameSettingGridSize String
    | RevealingCard Card
    | EndingTurn
    | ToggleClueGiverStatus
    | ToggleTeam


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
