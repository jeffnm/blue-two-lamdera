module Env exposing (..)
import Url

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.


dummyConfigItem =
  ""

protocol =
  case mode of
    Production ->
      Url.Https

    Development ->
      Url.Http

host = 
  case mode of
    Production ->
        "blue-two-lamdera.lamdera.app"

    Development ->
      "localhost"

urlport = 
  case mode of
    Production ->
      Nothing

    Development ->
      Just 8000