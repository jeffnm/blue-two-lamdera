module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , user : String
    , activeGame : Maybe Game
    , newGameSettings : List String
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


type Team
    = Blue
    | Red
    | Gray
    | Assassin


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
