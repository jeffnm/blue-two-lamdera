module Evergreen.Migrate.V5 exposing (..)

import Evergreen.V2.Types as Old
import Evergreen.V5.Types as New
import Lamdera exposing (sendToBackend)
import Lamdera.Migrations exposing (..)
import Types


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    case old.user of
        Nothing ->
            case old.activeGame of
                Nothing ->
                    let
                        newmodel =
                            New.FrontendModel old.key Nothing Nothing (oldNewGameSettingsToNewNewGameSettings old.newGameSettings) (oldNewUserSettingsToNewNewUserSettings old.newUserSettings) []
                    in
                    ModelMigrated ( newmodel, sendToBackend Types.GetPublicGames )

                Just game ->
                    let
                        newmodel =
                            New.FrontendModel old.key Nothing (Just (oldGameToNewGame game)) (oldNewGameSettingsToNewNewGameSettings old.newGameSettings) (oldNewUserSettingsToNewNewUserSettings old.newUserSettings) []
                    in
                    ModelMigrated ( newmodel, sendToBackend Types.GetPublicGames )

        Just user ->
            case old.activeGame of
                Nothing ->
                    let
                        newmodel =
                            New.FrontendModel old.key (Just (New.User user.name (oldTeamToNewTeam user.team) user.cluegiver Nothing Nothing)) Nothing (oldNewGameSettingsToNewNewGameSettings old.newGameSettings) (oldNewUserSettingsToNewNewUserSettings old.newUserSettings) []
                    in
                    ModelMigrated ( newmodel, sendToBackend Types.GetPublicGames )

                Just game ->
                    let
                        newmodel =
                            New.FrontendModel old.key (Just (New.User user.name (oldTeamToNewTeam user.team) user.cluegiver Nothing Nothing)) (Just (oldGameToNewGame game)) (oldNewGameSettingsToNewNewGameSettings old.newGameSettings) (oldNewUserSettingsToNewNewUserSettings old.newUserSettings) []
                    in
                    ModelMigrated ( newmodel, sendToBackend Types.GetPublicGames )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    let
        newgames =
            List.map (\g -> oldGameToNewGame g) old.games
    in
    ModelMigrated ( New.BackendModel newgames old.words, Cmd.none )


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

        Old.ToggleClueGiverStatus ->
            MsgMigrated ( New.ToggleClueGiverStatus, Cmd.none )

        Old.ToggleTeam ->
            MsgMigrated ( New.ToggleTeam, Cmd.none )


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    case old of
        Old.NoOpToBackend ->
            MsgMigrated ( New.NoOpToBackend, Cmd.none )

        Old.CreateNewGame _ ->
            MsgMigrated ( New.NoOpToBackend, Cmd.none )

        Old.JoinGame _ ->
            MsgMigrated ( New.NoOpToBackend, Cmd.none )

        Old.LoadGame game ->
            MsgMigrated ( New.LoadGame (oldGameToNewGame game), Cmd.none )

        Old.GetPublicGames ->
            MsgMigrated ( New.GetPublicGames, Cmd.none )

        Old.GetUserGames ->
            MsgMigrated ( New.NoOpToBackend, Cmd.none )

        Old.ChangeCardRevealedState card game ->
            MsgMigrated ( New.ChangeCardRevealedState (oldCardToNewCard card) (oldGameToNewGame game), Cmd.none )

        Old.EndTurn game gamestatus ->
            MsgMigrated ( New.EndTurn (oldGameToNewGame game) (oldGameStatusToNewGameStatus gamestatus), Cmd.none )

        Old.ChangeUserTeam ->
            MsgMigrated ( New.NoOpToBackend, Cmd.none )

        Old.EndGame team game ->
            MsgMigrated ( New.EndGame (oldTeamToNewTeam team) (oldGameToNewGame game), Cmd.none )


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    case old of
        Old.NoOpBackendMsg ->
            MsgMigrated ( New.NoOpBackendMsg, Cmd.none )

        Old.ShuffledWords words ->
            MsgMigrated ( New.ShuffledWords words, Cmd.none )

        Old.ShuffledCardTeams cardalignments ->
            MsgMigrated ( New.ShuffledCardTeams (List.map (\ca -> oldCardAlignmentsToNewCardAlignments ca) cardalignments), Cmd.none )

        Old.SendGameToPlayers id ->
            MsgMigrated ( New.SendGameToPlayers id, Cmd.none )


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    case old of
        Old.NoOpToFrontend ->
            MsgMigrated ( New.NoOpToFrontend, Cmd.none )

        Old.PublicGames games ->
            MsgMigrated ( New.PublicGames (List.map (\g -> oldGameToNewGame g) games), Cmd.none )

        Old.UserGames ->
            MsgMigrated ( New.UserGames, Cmd.none )

        Old.ActiveGame game ->
            MsgMigrated ( New.ActiveGame (oldGameToNewGame game), Cmd.none )


oldGameToNewGame : Old.Game -> New.Game
oldGameToNewGame old =
    New.Game old.id [] (oldGridSizeToNewGridsize old.gridSize) old.public (oldCardsToNewCards old.cards) (oldGameStatusToNewGameStatus old.gameStatus) old.words (List.map (\c -> oldCardAlignmentsToNewCardAlignments c) old.cardAlignments)


oldGridSizeToNewGridsize : Old.GridSize -> New.GridSize
oldGridSizeToNewGridsize old =
    case old of
        Old.SmallGrid ->
            New.SmallGrid

        Old.MediumGrid ->
            New.MediumGrid

        Old.LargeGrid ->
            New.LargeGrid


oldCardsToNewCards : List Old.Card -> List New.Card
oldCardsToNewCards old =
    List.map (\c -> oldCardToNewCard c) old


oldCardToNewCard : Old.Card -> New.Card
oldCardToNewCard old =
    { word = old.word
    , team = oldCardAlignmentsToNewCardAlignments old.team
    , revealed = old.revealed
    }


oldCardAlignmentsToNewCardAlignments : Old.CardAlignment -> New.CardAlignment
oldCardAlignmentsToNewCardAlignments old =
    case old of
        Old.BlueCard ->
            New.BlueCard

        Old.RedCard ->
            New.RedCard

        Old.Gray ->
            New.Gray

        Old.Assassin ->
            New.Assassin


oldGameStatusToNewGameStatus : Old.GameStatus -> New.GameStatus
oldGameStatusToNewGameStatus old =
    case old of
        Old.BlueWon ->
            New.BlueWon

        Old.RedWon ->
            New.RedWon

        Old.RedTurn ->
            New.RedTurn

        Old.BlueTurn ->
            New.BlueTurn


oldTeamToNewTeam : Old.Team -> New.Team
oldTeamToNewTeam old =
    case old of
        Old.Blue ->
            New.Blue

        Old.Red ->
            New.Red


oldNewGameSettingsToNewNewGameSettings : Old.NewGameSettings -> New.NewGameSettings
oldNewGameSettingsToNewNewGameSettings old =
    New.NewGameSettings old.public (oldGridSizeToNewGridsize old.gridSize) (oldTeamToNewTeam old.startingTeam)


oldNewUserSettingsToNewNewUserSettings : Old.NewUserSettings -> New.NewUserSettings
oldNewUserSettingsToNewNewUserSettings old =
    New.NewUserSettings old.username (oldTeamToNewTeam old.team) Nothing Nothing
