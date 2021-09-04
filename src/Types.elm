module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , user : Maybe User
    , activeGame : Maybe Game
    , newGameSettings : List String
    }


type alias BackendModel =
    { games : List Game
    }


type alias Game =
    { id : Int
    , users : List String
    , public : Bool
    , cards : List Card
    }


type alias Card =
    { word : String
    , team : Team
    , revealed : Bool
    }


type alias User =
    { name : String
    , team : Team
    , games : List Int
    }


type Team
    = Blue
    | Red
    | Gray
    | Assassin


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend
    | CreateNewGame
    | JoinGame
    | LoadGame
    | GetPublicGames
    | GetUserGames
    | ChangeCardRevealedState
    | ChangeUserTeam


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | PublicGames
    | UserGames
    | ActiveGame
