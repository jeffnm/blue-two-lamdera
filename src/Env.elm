module Env exposing (..)

import Url



-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.


dummyConfigItem =
    ""


protocol : Url.Protocol
protocol =
    case mode of
        Production ->
            Url.Https

        Development ->
            Url.Http


host : String
host =
    case mode of
        Production ->
            "blue-two-lamdera.lamdera.app"

        Development ->
            "localhost"


urlport : Maybe number
urlport =
    case mode of
        Production ->
            Nothing

        Development ->
            Just 8000
