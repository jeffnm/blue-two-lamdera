module Frontend exposing (..)

-- import Html.Events exposing (onCheck, onClick, onInput, onSubmit)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Cards exposing (cardCardAlignmentToRgb, cardCardAlignmentToString)
import Element as Element exposing (Element, centerX, column, el, paddingEach, rgb, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr exposing (name)
import Html.Events
import Json.Decode as Decode
import Lamdera exposing (ClientId, SessionId, sendToBackend)
import Types exposing (..)
import Url



-- MODEL


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    -- TODO: Generate a stub URL for each game to use as ID and access URL
    ( { key = key
      , user = Nothing
      , activeGame = Nothing
      , newGameSettings = NewGameSettings True MediumGrid Blue
      , newUserSettings = NewUserSettings "" Blue Nothing Nothing
      , publicGames = []
      }
    , sendToBackend GetPublicGames
    )



-- UPDATE


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        NewUser ->
            ( { model | user = Just (User model.newUserSettings.username model.newUserSettings.team False model.newUserSettings.sessionId model.newUserSettings.clientId) }, sendToBackend GetPublicGames )

        CreatingNewGame ->
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( model, sendToBackend (CreateNewGame model.newGameSettings user) )

        JoiningGame id ->
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( model, sendToBackend (JoinGame id user) )

        RevealingCard card ->
            case model.activeGame of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    let
                        newcard =
                            { card | revealed = True }
                    in
                    case model.user of
                        Nothing ->
                            ( model, Cmd.none )

                        Just user ->
                            if isItUsersTurn user.team game.gameStatus then
                                if card.team == Assassin then
                                    case user.team of
                                        -- If your team picked Assassin, the other team wins.
                                        Red ->
                                            ( model, sendToBackend (EndGame Blue game) )

                                        Blue ->
                                            ( model, sendToBackend (EndGame Red game) )

                                else
                                    ( model, sendToBackend (ChangeCardRevealedState newcard game) )

                            else
                                ( model, Cmd.none )

        EndingTurn ->
            case model.activeGame of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    case game.gameStatus of
                        RedTurn ->
                            ( model, sendToBackend (EndTurn game BlueTurn) )

                        BlueTurn ->
                            ( model, sendToBackend (EndTurn game RedTurn) )

                        _ ->
                            ( model, Cmd.none )

        ToggleClueGiverStatus ->
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( { model | user = Just (toggleClueGiver user (not user.cluegiver)) }, Cmd.none )

        ToggleTeam ->
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    case model.activeGame of
                        Nothing ->
                            ( { model | user = Just (toggleTeam user) }, Cmd.none )

                        Just game ->
                            ( { model | user = Just (toggleTeam user) }, sendToBackend (ChangeUserTeam game (toggleTeam user)) )

        ToggleNewGameSettingPublic value ->
            ( { model | newGameSettings = toggleNewGameSettingPublic model.newGameSettings value }, Cmd.none )

        ChangeNewGameSettingGridSize gridSize ->
            ( { model | newGameSettings = setNewGameSettingGridSize model.newGameSettings gridSize }, Cmd.none )

        ChangeNewGameSettingTeam team ->
            ( { model | newGameSettings = setNewGameSettingTeam model.newGameSettings (teamFromString team) }, Cmd.none )

        ChangeNewUserSettingTeam team ->
            ( { model | newUserSettings = setNewUserSettingTeam model.newUserSettings (teamFromString team) }, Cmd.none )

        ChangeNewUserSettingUsername username ->
            ( { model | newUserSettings = setNewUserSettingUsername model.newUserSettings username }, Cmd.none )

        LeavingGame ->
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    case model.activeGame of
                        Nothing ->
                            ( model, Cmd.none )

                        Just game ->
                            ( { model | activeGame = Nothing }, Cmd.batch [ sendToBackend (LeaveGame game user), sendToBackend GetPublicGames ] )

        _ ->
            -- Debug.todo "Finish FrontendMsg updates"
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        ClientInfo sessionId clientId ->
            ( { model | newUserSettings = setNewUserSettingSessionIdAndClientId model.newUserSettings sessionId clientId }, Cmd.none )

        ActiveGame game ->
            ( { model | activeGame = Just game }, Cmd.none )

        PublicGames publicGames ->
            ( { model | publicGames = publicGames }, Cmd.none )

        _ ->
            -- Debug.todo "Implement other branches"
            ( model, Cmd.none )



-- UTILITIES


teamFromString : String -> Team
teamFromString team =
    case String.toUpper team of
        "BLUE" ->
            Blue

        "RED" ->
            Red

        _ ->
            Blue


teamToString : Team -> String
teamToString team =
    case team of
        Blue ->
            "Blue"

        Red ->
            "Red"


isItUsersTurn : Team -> GameStatus -> Bool
isItUsersTurn team turn =
    case turn of
        BlueTurn ->
            if team == Blue then
                True

            else
                False

        RedTurn ->
            if team == Red then
                True

            else
                False

        _ ->
            False


toggleNewGameSettingPublic : NewGameSettings -> Bool -> NewGameSettings
toggleNewGameSettingPublic oldSettings public =
    { oldSettings | public = public }


toggleClueGiver : User -> Bool -> User
toggleClueGiver olduser cluegiver =
    { olduser | cluegiver = cluegiver }


toggleTeam : User -> User
toggleTeam user =
    { user
        | team =
            if user.team == Red then
                Blue

            else
                Red
    }


setNewGameSettingGridSize : NewGameSettings -> String -> NewGameSettings
setNewGameSettingGridSize oldSettings gridSize =
    { oldSettings | gridSize = gridSizeFromString gridSize }


setNewGameSettingTeam : NewGameSettings -> Team -> NewGameSettings
setNewGameSettingTeam oldSettings team =
    { oldSettings | startingTeam = team }


setNewUserSettingUsername : NewUserSettings -> String -> NewUserSettings
setNewUserSettingUsername oldSettings username =
    { oldSettings | username = username }


setNewUserSettingTeam : NewUserSettings -> Team -> NewUserSettings
setNewUserSettingTeam oldSettings team =
    { oldSettings | team = team }


setNewUserSettingSessionIdAndClientId : NewUserSettings -> SessionId -> ClientId -> NewUserSettings
setNewUserSettingSessionIdAndClientId oldSettings sessionId clientId =
    { oldSettings | sessionId = Just sessionId, clientId = Just clientId }


getUsername : Maybe User -> String
getUsername user =
    case user of
        Just u ->
            u.name

        Nothing ->
            ""


gridSizeFromString : String -> GridSize
gridSizeFromString gridSize =
    case gridSize of
        "Small" ->
            SmallGrid

        "Medium" ->
            MediumGrid

        "Large" ->
            LargeGrid

        _ ->
            MediumGrid


gridSizeToString : GridSize -> String
gridSizeToString gridsize =
    case gridsize of
        SmallGrid ->
            "Small"

        MediumGrid ->
            "Medium"

        LargeGrid ->
            "Large"


getScore : Game -> Team -> String
getScore game team =
    case team of
        Red ->
            let
                red =
                    List.filter (\c -> c.team == RedCard) game.cards
                        |> List.filter (\c -> c.revealed)
                        |> List.length
                        |> String.fromInt

                redTotal =
                    List.filter (\c -> c.team == RedCard) game.cards
                        |> List.length
                        |> String.fromInt
            in
            "Red: " ++ red ++ " / " ++ redTotal

        Blue ->
            let
                blue =
                    List.filter (\c -> c.team == BlueCard) game.cards
                        |> List.filter (\c -> c.revealed)
                        |> List.length
                        |> String.fromInt

                blueTotal =
                    List.filter (\c -> c.team == BlueCard) game.cards
                        |> List.length
                        |> String.fromInt
            in
            "Blue: " ++ blue ++ " / " ++ blueTotal


getPlayersNamesByTeam : Game -> Team -> List String
getPlayersNamesByTeam game team =
    List.filter (\p -> p.team == team) game.users
        |> List.filter (\p -> p.clientId /= Nothing)
        |> List.map (\p -> p.name)


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )



-- VIEW


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout [] (viewSwitch model) ]
    }


viewSwitch : Model -> Element FrontendMsg
viewSwitch model =
    case model.user of
        Nothing ->
            viewLandingPage model

        Just user ->
            case model.activeGame of
                Nothing ->
                    viewLobby model

                Just game ->
                    viewGame game user


viewLandingPage : Model -> Element FrontendMsg
viewLandingPage model =
    column [ Element.width Element.fill, Element.height Element.fill ] [ viewCreateUserForm model ]


viewLobby : Model -> Element FrontendMsg
viewLobby model =
    column [ Element.width Element.fill, Element.height Element.fill ]
        [ row [ Element.width Element.fill, Element.height Element.fill ]
            [ column [ Element.width (Element.fillPortion 1) ] []
            , column [ Element.width (Element.fillPortion 3) ]
                [ row
                    [ Element.width Element.fill
                    , Element.paddingEach
                        { bottom = 20
                        , left = 0
                        , right = 0
                        , top = 0
                        }
                    ]
                    [ el [] (text ("Welcome " ++ getUsername model.user)) ]
                , Element.wrappedRow [ Element.alignLeft, Element.width Element.fill ]
                    [ viewCreateGameForm model
                    , viewPublicGames model
                    ]
                ]
            , column [ Element.width (Element.fillPortion 1) ] []
            ]
        ]


viewGame : Game -> User -> Element FrontendMsg
viewGame game user =
    let
        endScore =
            [ el [] (text (getScore game Blue))
            , el [] (text (getScore game Red))
            ]

        gameOver =
            viewGameBoardWrapper game.gridSize (viewCardsGameOver game.cards)
    in
    case game.gameStatus of
        RedWon ->
            column [ Element.width Element.fill ]
                [ row [ Element.centerX, Element.spacing 5, Element.padding 10 ] [ el [] (text "Red won!") ]
                , row [ Element.centerX, Element.spacing 5, Element.padding 10 ] endScore
                , row [ Element.centerX, Element.spacing 5, Element.padding 10 ] [ viewLeaveGameButton ]
                , gameOver
                ]

        BlueWon ->
            column [ Element.width Element.fill ]
                [ row [ Element.centerX, Element.spacing 5, Element.padding 10 ] [ el [] (text "Blue won!") ]
                , row [ Element.centerX, Element.spacing 5, Element.padding 10 ] endScore
                , row [ Element.centerX, Element.spacing 5, Element.padding 10 ] [ viewLeaveGameButton ]
                , gameOver
                ]

        RedTurn ->
            viewGamePlaying game user

        BlueTurn ->
            viewGamePlaying game user


viewPublicGames : Model -> Element FrontendMsg
viewPublicGames model =
    let
        publicGames =
            List.filter (\g -> g.gameStatus /= RedWon && g.gameStatus /= BlueWon) model.publicGames
    in
        column [ Element.width (Element.fillPortion 1)]
            (row [  ] [ el [ Element.paddingXY 0 20 ] (text "Public Games") ]
                :: List.map
                    (\g -> row [ Element.width (Element.fill |> Element.minimum 200), Element.spacing 10, Element.padding 5] [ el [ ] (text ("Game " ++ String.fromInt g.id)), Input.button (viewButtonAttributes ++ [ Element.alignLeft ]) { onPress = Just (JoiningGame g.id), label = text "Join" } ])
                    publicGames
            )
        


viewGameHeader : Game -> User -> Element FrontendMsg
viewGameHeader game user =
    row [ Element.width Element.fill, Element.padding 10 ]
        [ viewBlueTeam game
        , row [ Element.width (Element.fillPortion 3) ] [ viewTurnAndToggles game user ]
        , viewRedTeam game
        ]


viewGameBoardWrapper : GridSize -> List (Element msg) -> Element msg
viewGameBoardWrapper gridsize gameboard =
    case gridsize of
        SmallGrid ->
            Element.row [ Element.width Element.fill, Element.padding 10 ]
                [ Element.column [centerX]
                [ Element.wrappedRow [ Element.width (Element.fill |> Element.maximum 1100 |> Element.minimum 1100), Element.padding 10, Element.spacing 10 ] gameboard ]
                ]

        MediumGrid ->
            Element.row [ Element.width Element.fill, Element.padding 10 ]
                [ Element.column [centerX]
                [ Element.wrappedRow [ Element.width (Element.fill |> Element.maximum 1350 |> Element.minimum 1350), Element.padding 10, Element.spacing 10, centerX ] gameboard ]
                ]
        LargeGrid ->
            Element.row [ Element.width Element.fill, Element.padding 10 ]
                [ Element.column [centerX]
                [ Element.wrappedRow [ Element.width (Element.fill |> Element.maximum 1600 |> Element.minimum 1600), Element.padding 10, Element.spacing 10] gameboard ]
                ]


viewGamePlaying : Game -> User -> Element FrontendMsg
viewGamePlaying game user =
    if user.cluegiver then
        column [ Element.width Element.fill ]
            [ viewGameHeader game user
            , viewGameBoardWrapper game.gridSize (viewCardsClueGiver game.cards)
            ]

    else
        column [ Element.width Element.fill ]
            [ viewGameHeader game user
            , viewGameBoardWrapper game.gridSize (viewCardsPlaying game.cards (isItUsersTurn user.team game.gameStatus))
            ]


viewBlueTeam : Game -> Element FrontendMsg
viewBlueTeam game =
    let
        score =
            el [] (text (getScore game Blue))

        players =
            column [] (List.map (\p -> el [] (text p)) (getPlayersNamesByTeam game Blue))
    in
    column [ Element.width (Element.fillPortion 1), Element.padding 20, Element.spacing 5 ] [ score, players ]


viewRedTeam : Game -> Element FrontendMsg
viewRedTeam game =
    let
        score =
            el [] (text (getScore game Red))

        players =
            column [] (List.map (\p -> el [] (text p)) (getPlayersNamesByTeam game Red))
    in
    column [ Element.width (Element.fillPortion 1), Element.padding 20, Element.spacing 5 ] [ score, players ]


viewTurnAndToggles : Game -> User -> Element FrontendMsg
viewTurnAndToggles game user =
    let
        endTurnButton =
            if isItUsersTurn user.team game.gameStatus then
                Input.button viewButtonAttributes { onPress = Just EndingTurn, label = text "End Turn" }

            else
                el [] (text "")

        centered =
            [ Element.centerX, Element.centerY ]

        controlAttrs =
            [ Element.padding 5
            , Element.spacing 5
            ]
                ++ centered
    in
    case game.gameStatus of
        RedWon ->
            row controlAttrs
                [ el centered (text "Red Won!")
                ]

        BlueWon ->
            row controlAttrs
                [ el centered (text "Blue Won!")
                ]

        RedTurn ->
            row controlAttrs
                [ el centered (text "It's Red's turn.")
                , el centered endTurnButton
                , el centered (viewClueGiverToggleButton user)
                , el centered (viewTeamToggleButton user)
                , el centered viewLeaveGameButton
                ]

        BlueTurn ->
            row controlAttrs
                [ el centered (text "It's Blue's turn.")
                , el centered endTurnButton
                , el centered (viewClueGiverToggleButton user)
                , el centered (viewTeamToggleButton user)
                , el centered viewLeaveGameButton
                ]



-- VIEW CARDS


viewCardsPlaying : List Card -> Bool -> List (Element FrontendMsg)
viewCardsPlaying cardList clickable =
    List.map
        (\c ->
            Element.paragraph (viewCardAttributes c clickable ++ viewCardColorIfRevealed c)
                [ text c.word ]
        )
        cardList


viewCardsGameOver : List Card -> List (Element FrontendMsg)
viewCardsGameOver cardList =
    List.map
        (\c ->
            el (viewCardAttributes c False ++ viewCardColorAttributes c)
                (text c.word)
        )
        cardList


viewCardsClueGiver : List Card -> List (Element FrontendMsg)
viewCardsClueGiver cardList =
    List.map
        (\c ->
            el (viewCardAttributes c False ++ viewCardColorAttributes c)
                (text c.word)
        )
        cardList



-- VIEW BUTTONS


viewClueGiverToggleButton : User -> Element FrontendMsg
viewClueGiverToggleButton user =
    if user.cluegiver then
        Input.button viewButtonAttributes { onPress = Just ToggleClueGiverStatus, label = text "Stop being clue giver" }

    else
        Input.button viewButtonAttributes { onPress = Just ToggleClueGiverStatus, label = text "Become clue giver" }


viewTeamToggleButton : User -> Element FrontendMsg
viewTeamToggleButton user =
    if user.team == Red then
        Input.button viewButtonAttributes { onPress = Just ToggleTeam, label = text "Switch to Blue team" }

    else
        Input.button viewButtonAttributes { onPress = Just ToggleTeam, label = text "Switch to Red team" }


viewLeaveGameButton : Element FrontendMsg
viewLeaveGameButton =
    Input.button viewButtonAttributes
        { onPress = Just LeavingGame
        , label = text "Return to lobby"
        }



-- VIEW FORMS


viewCreateUserForm : Model -> Element FrontendMsg
viewCreateUserForm model =
    row [ Element.width Element.fill, Element.height Element.fill, Element.padding 50, Element.spacing 5, Element.centerY ]
        [   column [Element.width (Element.fillPortion 1)][]
            ,column [Element.width (Element.fillPortion 3), Element.spacing 10, Element.centerX ]
            [ row [ Element.width Element.fill ]
                [ Input.text [ Element.spacing 5, onEnter NewUser , Element.width (Element.fill |> Element.maximum 800 |> Element.minimum 200)]
                    { label = Input.labelLeft [] (text "Username")
                    , onChange = ChangeNewUserSettingUsername
                    , placeholder = Nothing
                    , text = model.newUserSettings.username
                    }
                ]
            , row [ Element.width Element.fill ]
                [ Input.radioRow
                    [ Element.spacing 10 ]
                    { label = Input.labelLeft [ Element.paddingEach { bottom = 0, left = 0, right = 50, top = 0 } ] (text "Team")
                    , onChange = ChangeNewUserSettingTeam
                    , selected = Just (teamToString model.newUserSettings.team)
                    , options = [ Input.option "Blue" (text "Blue"), Input.option "Red" (text "Red") ]
                    }
                ]
            , row [ Element.width Element.fill ]
                [ Input.button
                    (viewButtonAttributes ++ [ Element.centerX, Element.width (Element.px 150) ])
                    { onPress = Just NewUser
                    , label = text "Start"
                    }
                ]
            ]
            , column [Element.width (Element.fillPortion 1)][]
        ]


viewCreateGameForm : Model -> Element FrontendMsg
viewCreateGameForm model =
        column [ Element.width (Element.fillPortion 3)]
            [ el [ Element.paddingXY 0 20 ] (text "Create a new game")
            , column [ Element.width Element.fill, Element.spacingXY 5 10 ]
                [ row [ Element.width Element.fill ]
                    [ Input.checkbox []
                        { onChange = ToggleNewGameSettingPublic
                        , icon = Input.defaultCheckbox
                        , checked = model.newGameSettings.public
                        , label =
                            Input.labelRight
                                [ Element.paddingEach
                                    { left = 10
                                    , bottom = 0
                                    , right = 0
                                    , top = 0
                                    }
                                ]
                                (text "Make game public")
                        }
                    ]
                , row [ Element.width Element.fill ]
                    [ Input.radioRow [ Element.spacing 10 ]
                        { onChange = ChangeNewGameSettingGridSize
                        , selected = Just (gridSizeToString model.newGameSettings.gridSize)
                        , label =
                            Input.labelLeft
                                [ Element.paddingEach
                                    { bottom = 0
                                    , left = 0
                                    , right = 55
                                    , top = 0
                                    }
                                , Element.centerY
                                ]
                                (text "Gridsize")
                        , options =
                            [ Input.option "Small" (text "Small")
                            , Input.option "Medium" (text "Medium")
                            , Input.option "Large" (text "Large")
                            ]
                        }
                    ]
                , row [ Element.width Element.fill ]
                    [ Input.radioRow [ Element.spacing 10 ]
                        { onChange = ChangeNewGameSettingTeam
                        , selected = Just (teamToString model.newGameSettings.startingTeam)
                        , label =
                            Input.labelLeft
                                [ Element.paddingEach
                                    { bottom = 0
                                    , left = 0
                                    , right = 5
                                    , top = 0
                                    }
                                ]
                                (text "Starting Team")
                        , options =
                            [ Input.option "Red" (text "Red")
                            , Input.option "Blue" (text "Blue")
                            ]
                        }
                    ]
                , row [ Element.width Element.fill ]
                    [ Input.button (viewButtonAttributes ++ [])
                        { onPress = Just CreatingNewGame
                        , label = text "Create Game"
                        }
                    ]
                ]
            ]
        



-- VIEW STYLE ATTRIBUTES


viewButtonAttributes : List (Element.Attribute msg)
viewButtonAttributes =
    [ Background.color (rgb 1 1 1)
    , Border.solid
    , Border.width 1
    , Element.paddingEach { bottom = 10, left = 10, right = 10, top = 15 }
    , Font.center
    , Border.rounded 5
    , Border.color (rgb 0 0 0)
    , Element.mouseOver [ Background.color (rgb 0.8 0.8 0.8) ]
    ]


viewCardAttributes : Card -> Bool -> List (Element.Attribute FrontendMsg)
viewCardAttributes card clickable =
    let
        base =
            [ Element.width (Element.fill |> Element.maximum 250 |> Element.minimum 250)
            , Element.spacing 50
            , Element.padding 40
            , Font.center
            , Element.alignLeft
            , Border.rounded 5
            , Border.width 1
            , Border.solid
            ]
    in
    if clickable then
        base
            ++ [ Element.pointer
               , Element.mouseOver [ Border.glow (rgb 0.5 0.5 0) 0.5 ]
               , onClick (RevealingCard card)
               ]

    else
        base


viewCardColorAttributes : Card -> List (Element.Attribute FrontendMsg)
viewCardColorAttributes card =
    let
        cardColorAttr =
            Background.color (cardCardAlignmentToRgb card.team)
    in
    case card.team of
        Assassin ->
            [ cardColorAttr, Font.color (rgb 1 1 1) ]

        _ ->
            [ cardColorAttr ]


viewCardColorIfRevealed : Card -> List (Element.Attribute FrontendMsg)
viewCardColorIfRevealed card =
    if card.revealed then
        viewCardColorAttributes card

    else
        [ Background.color (rgb 1 1 1) ]
