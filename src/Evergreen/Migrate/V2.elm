module Evergreen.Migrate.V2 exposing (..)

import Evergreen.V1.Types as Old exposing (CardAlignment(..))
import Evergreen.V2.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    case old of
        Old.UrlClicked url ->
            MsgMigrated ( New.UrlClicked url, Cmd.none )

        Old.UrlChanged url ->
            MsgMigrated ( New.UrlChanged url, Cmd.none )

        Old.NoOpFrontendMsg ->
            MsgMigrated ( New.NoOpFrontendMsg, Cmd.none )

        Old.NewUser ->
            MsgMigrated ( New.NewUser, Cmd.none )

        Old.ChangeNewUserSettingUsername name ->
            MsgMigrated ( New.ChangeNewUserSettingUsername name, Cmd.none )

        Old.ChangeNewUserSettingTeam team ->
            MsgMigrated ( New.ChangeNewUserSettingTeam team, Cmd.none )

        Old.CreatingNewGame ->
            MsgMigrated ( New.CreatingNewGame, Cmd.none )

        Old.LeavingGame ->
            MsgMigrated ( New.LeavingGame, Cmd.none )

        Old.LoadingGame ->
            MsgMigrated ( New.LoadingGame, Cmd.none )

        Old.JoiningGame id ->
            MsgMigrated ( New.JoiningGame id, Cmd.none )

        Old.ToggleNewGameSettingPublic public ->
            MsgMigrated ( New.ToggleNewGameSettingPublic public, Cmd.none )

        Old.ChangeNewGameSettingGridSize size ->
            MsgMigrated ( New.ChangeNewGameSettingGridSize size, Cmd.none )

        Old.RevealingCard card ->
            case card.team of
                Old.BlueCard ->
                    MsgMigrated ( New.RevealingCard (New.Card card.word New.BlueCard card.revealed), Cmd.none )

                Old.RedCard ->
                    MsgMigrated ( New.RevealingCard (New.Card card.word New.RedCard card.revealed), Cmd.none )

                Old.Assassin ->
                    MsgMigrated ( New.RevealingCard (New.Card card.word New.Assassin card.revealed), Cmd.none )

                Old.Gray ->
                    MsgMigrated ( New.RevealingCard (New.Card card.word New.Gray card.revealed), Cmd.none )

        Old.EndingTurn ->
            MsgMigrated ( New.EndingTurn, Cmd.none )


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged
