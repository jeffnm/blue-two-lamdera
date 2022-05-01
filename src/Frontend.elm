module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Cards exposing (cardCardAlignmentToRgb)
import Element exposing (Element, centerX, column, el, rgb, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Env
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
        , subscriptions = \_ -> Sub.none
        , view = view
        }


lobbyURL : Url.Url
lobbyURL =
    Url.Url Env.protocol Env.host Env.urlport "/lobby" Nothing Nothing


landingURLtoGame : String -> Url.Url
landingURLtoGame gameid =
    Url.Url Env.protocol Env.host Env.urlport "/landing" (Just gameid) Nothing


landingURL : Url.Url
landingURL =
    Url.Url Env.protocol Env.host Env.urlport "/landing" Nothing Nothing


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( LoadingPage { url = url, key = key }, Cmd.batch [ Nav.pushUrl key (Url.toString url) ] )


defaultNewGameSettings =
    NewGameSettings MediumGrid Blue


defaultNewUserSettings =
    NewUserSettings "" Blue Nothing Nothing



-- UPDATE


getRoutingStateFromModel : Model -> RoutingState
getRoutingStateFromModel model =
    case model of
        LoadingPage state ->
            state

        LandingPage state _ ->
            state

        LobbyPage state _ _ ->
            state

        LoadedGamePage state _ _ ->
            state


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    let
        state =
            getRoutingStateFromModel model
    in
    case msg of
        UrlClicked (Internal url) ->
            ( model
            , Nav.pushUrl state.key (Url.toString url)
            )

        UrlClicked (External url) ->
            ( model
            , Nav.load url
            )

        UrlChanged url ->
            case model of
                LoadingPage _ ->
                    -- Should be the case anytime the user manually changes the url
                    case url.path of
                        "/lobby" ->
                            ( LandingPage { state | url = landingURL } defaultNewUserSettings
                            , Nav.pushUrl state.key (Url.toString landingURL)
                            )

                        "/game" ->
                            -- Record the game id url parameter, then send them to the login
                            ( LandingPage { state | url = url } defaultNewUserSettings
                            , Nav.pushUrl state.key (Url.toString landingURL)
                            )

                        _ ->
                            ( model, Cmd.none )

                LandingPage _ _ ->
                    -- If there is no user yet, then we don't go anywhere else
                    ( model, Cmd.none )

                LobbyPage _ user settings ->
                    case url.path of
                        "/game" ->
                            -- If the user is logged in and tries to load a game param,
                            -- try to have them join the game, waiting in the lobby for a response
                            case url.query of
                                Just param ->
                                    if String.startsWith "id=" param then
                                        ( LobbyPage state user settings
                                        , Cmd.batch
                                            [ sendToBackend <|
                                                JoinGame (String.dropLeft 3 param) user
                                            ]
                                        )

                                    else
                                        ( LobbyPage { state | url = lobbyURL } user settings
                                        , Nav.pushUrl state.key (Url.toString lobbyURL)
                                        )

                                Nothing ->
                                    ( LobbyPage { state | url = lobbyURL } user settings
                                    , Nav.pushUrl state.key (Url.toString lobbyURL)
                                    )

                        _ ->
                            ( model, Cmd.none )

                LoadedGamePage _ _ _ ->
                    -- If the user has a loaded game, that's all we show
                    ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        NewUser settings ->
            let
                newUser =
                    User settings.username settings.team False settings.sessionId settings.clientId
            in
            case state.url.query of
                Nothing ->
                    -- If there is no url query saved, then we just go to the lobby and register the user
                    ( LobbyPage { state | url = lobbyURL } newUser defaultNewGameSettings
                    , Cmd.batch
                        [ Nav.pushUrl state.key (Url.toString lobbyURL)
                        , sendToBackend (RegisterUser newUser)
                        ]
                    )

                Just param ->
                    -- If there is a url query saved, then we send them to the game saved url to see if they can join it
                    ( LobbyPage state newUser defaultNewGameSettings
                    , Cmd.batch
                        [ Nav.pushUrl state.key ("/game?" ++ param)
                        , sendToBackend (RegisterUser newUser)
                        ]
                    )

        CreatingNewGame user settings ->
            let
                -- No one should be cluegiver immediately when creating a game
                u =
                    { user | cluegiver = False }
            in
            ( LobbyPage state u settings
            , sendToBackend (CreateNewGame settings u)
            )

        RevealingCard card game user ->
            if isItUsersTurn user.team game.gameStatus then
                if card.team == Assassin then
                    case user.team of
                        -- If your team picked Assassin, the other team wins.
                        Red ->
                            ( model, sendToBackend (EndGame Blue game) )

                        Blue ->
                            ( model, sendToBackend (EndGame Red game) )

                else
                    let
                        newcard =
                            { card | revealed = True }
                    in
                    ( model, sendToBackend (ChangeCardRevealedState newcard game) )

            else
                ( model, Cmd.none )

        EndingTurn game ->
            case game.gameStatus of
                RedTurn ->
                    ( model, sendToBackend (EndTurn game BlueTurn) )

                BlueTurn ->
                    ( model, sendToBackend (EndTurn game RedTurn) )

                _ ->
                    ( model, Cmd.none )

        ToggleClueGiverStatus user game ->
            ( LoadedGamePage state (toggleClueGiver user (not user.cluegiver)) game, Cmd.none )

        ToggleTeam user activeGame ->
            let
                newUser =
                    toggleTeam user
            in
            case activeGame of
                Nothing ->
                    case model of
                        LobbyPage _ _ settings ->
                            ( LobbyPage state newUser settings, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Just game ->
                    ( LoadedGamePage state newUser game, sendToBackend (ChangeUserTeam game newUser) )

        ChangeNewGameSettingGridSize gridSize ->
            case model of
                LobbyPage _ user settings ->
                    ( LobbyPage state user (setNewGameSettingGridSize settings gridSize), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeNewGameSettingTeam team ->
            case model of
                LobbyPage _ user settings ->
                    ( LobbyPage state user (setNewGameSettingTeam settings (teamFromString team)), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeNewUserSettingTeam team ->
            case model of
                LandingPage _ settings ->
                    ( LandingPage state (setNewUserSettingTeam settings (teamFromString team)), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeNewUserSettingUsername username ->
            case model of
                LandingPage _ settings ->
                    ( LandingPage state (setNewUserSettingUsername settings username), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LeavingGame user game ->
            ( LobbyPage state user defaultNewGameSettings
            , Cmd.batch
                [ sendToBackend (LeaveGame game user)
                , Nav.pushUrl state.key (Url.toString lobbyURL)
                ]
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    let
        state =
            getRoutingStateFromModel model
    in
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        ClientInfo _ _ (Just user) ->
            case model of
                LoadingPage _ ->
                    ( LobbyPage state user defaultNewGameSettings
                    , Cmd.batch [ Nav.pushUrl state.key (Url.toString lobbyURL) ]
                    )

                LobbyPage _ _ settings ->
                    if state.url.path == "/game" then
                        ( LobbyPage state user settings
                        , Cmd.batch [ Nav.pushUrl state.key (Url.toString state.url) ]
                        )

                    else
                        ( LobbyPage state user settings, Cmd.none )

                LandingPage _ _ ->
                    if state.url.path == "/game" then
                        ( LobbyPage state user defaultNewGameSettings
                        , Cmd.batch [ Nav.pushUrl state.key (Url.toString state.url) ]
                        )

                    else
                        ( LobbyPage state user defaultNewGameSettings
                        , Cmd.batch [ Nav.pushUrl state.key (Url.toString lobbyURL) ]
                        )

                LoadedGamePage _ _ game ->
                    ( LoadedGamePage state user game, Cmd.none )

        ClientInfo sessionId clientId Nothing ->
            ( LandingPage state <|
                setNewUserSettingSessionIdAndClientId defaultNewUserSettings sessionId clientId
            , Cmd.batch [ Nav.pushUrl state.key (Url.toString landingURL) ]
            )

        ActiveGame game ->
            case model of
                LobbyPage _ user _ ->
                    -- If the user is in the lobby, we just go to the game
                    ( LoadedGamePage state user game
                    , Cmd.batch
                        [ Nav.pushUrl state.key ("/game?id=" ++ game.id)
                        ]
                    )

                LoadedGamePage _ user _ ->
                    -- If the user is in game, justs update the game, no need to change anything else
                    ( LoadedGamePage state user game, Cmd.none )

                _ ->
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


toggleClueGiver : User -> Bool -> User
toggleClueGiver olduser cluegiver =
    { olduser | cluegiver = cluegiver }


toggleTeam : User -> User
toggleTeam user =
    let
        newTeam =
            if user.team == Red then
                Blue

            else
                Red
    in
    { user | team = newTeam }


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


getUserSessionId : User -> String
getUserSessionId user =
    -- Can we use pattern matching here?
    case user.sessionId of
        Just sid ->
            sid

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
        |> List.map .name


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
    { title = "BLUE TWO"
    , body =
        [ Element.layout [] (viewSwitch model) ]
    }


viewSwitch : Model -> Element FrontendMsg
viewSwitch model =
    case model of
        LoadingPage state ->
            viewLoading state

        LandingPage state settings ->
            viewLandingPage state settings

        LobbyPage state user settings ->
            viewLobby state user settings

        LoadedGamePage state user game ->
            viewGame state user game


viewLoading : RoutingState -> Element FrontendMsg
viewLoading state =
    column
        []
        [ Element.text "Loading...", viewDevelopmentFooter state.url "" ]


viewLandingPage : RoutingState -> NewUserSettings -> Element FrontendMsg
viewLandingPage state settings =
    column [ Element.width Element.fill, Element.height Element.fill ]
        [ viewCreateUserForm settings
        , viewDevelopmentFooter state.url ""
        ]


viewLobby : RoutingState -> User -> NewGameSettings -> Element FrontendMsg
viewLobby state user setting =
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
                    [ el [] (text ("Welcome " ++ user.name)) ]
                , Element.wrappedRow [ Element.alignLeft, Element.width Element.fill ]
                    [ viewCreateGameForm user setting
                    ]
                ]
            , column [ Element.width (Element.fillPortion 1) ] []
            ]
        , viewDevelopmentFooter state.url (getUserSessionId user)
        ]


viewDevelopmentFooter : Url.Url -> SessionId -> Element FrontendMsg
viewDevelopmentFooter url sessionId =
    -- Env is injected by Lamdera at runtime, so the elm tools error here can be ignored
    if Env.mode == Env.Development then
        row []
            [ el [] (text (Url.toString url))
            , el [] (text sessionId)
            ]

    else
        row []
            [ el [] (text "")
            ]


viewGame : RoutingState -> User -> Game -> Element FrontendMsg
viewGame _ user game =
    let
        endScore =
            [ el [] (text (getScore game Blue))
            , el [] (text (getScore game Red))
            ]

        gameOver =
            viewGameBoardWrapper game.gridSize (viewCardsAllRevealed game)

        winColumn winningText =
            column [ Element.width Element.fill ]
                [ row [ Element.centerX, Element.spacing 5, Element.padding 10 ] [ el [] (text winningText) ]
                , row [ Element.centerX, Element.spacing 5, Element.padding 10 ] endScore
                , row [ Element.centerX, Element.spacing 5, Element.padding 10 ] [ viewLeaveGameButton user game ]
                , gameOver
                ]
    in
    case game.gameStatus of
        RedWon ->
            if user.team == Red then
                winColumn ("Red won! Congratulations " ++ user.name ++ "!")

            else
                winColumn ("Red won! Better luck next time, " ++ user.name ++ "!")

        BlueWon ->
            if user.team == Blue then
                winColumn ("Blue won! Congratulations " ++ user.name ++ "!")

            else
                winColumn ("Blue won! Better luck next time, " ++ user.name ++ "!")

        RedTurn ->
            viewGamePlaying game user

        BlueTurn ->
            viewGamePlaying game user


viewGameHeader : Game -> User -> Element FrontendMsg
viewGameHeader game user =
    row [ Element.width Element.fill, Element.padding 10 ]
        [ viewBlueTeam game user
        , row [ Element.width (Element.fillPortion 3) ] [ viewTurnAndToggles game user ]
        , viewRedTeam game user
        ]


viewGameBoardWrapper : GridSize -> List (Element msg) -> Element msg
viewGameBoardWrapper gridsize gameboard =
    let
        row =
            Element.row [ Element.width Element.fill, Element.padding 10 ]

        column =
            Element.column [ centerX ]

        grid =
            case gridsize of
                SmallGrid ->
                    1100

                MediumGrid ->
                    1350

                LargeGrid ->
                    1600
    in
    row
        [ column
            [ Element.wrappedRow
                [ Element.width
                    (Element.fill
                        |> Element.maximum grid
                        |> Element.minimum grid
                    )
                , Element.spacing 10
                ]
                gameboard
            ]
        ]


viewGamePlaying : Game -> User -> Element FrontendMsg
viewGamePlaying game user =
    let
        wrapper =
            viewGameBoardWrapper game.gridSize <|
                if user.cluegiver then
                    viewCardsAllRevealed game

                else
                    -- viewCardsPlaying game.cards <| isItUsersTurn user.team game.gameStatus
                    viewCardsPlaying game user
    in
    column [ Element.width Element.fill ]
        [ viewGameHeader game user
        , wrapper
        ]


viewBlueTeam : Game -> User -> Element FrontendMsg
viewBlueTeam game user =
    let
        score =
            el [] (text (getScore game Blue))

        players =
            column [] <|
                List.map
                    (\p ->
                        el
                            [ if p == user.name then
                                Font.italic

                              else
                                Font.regular
                            ]
                            (text p)
                    )
                <|
                    getPlayersNamesByTeam game Blue
    in
    column [ Element.width (Element.fillPortion 1), Element.padding 20, Element.spacing 5 ] [ score, players ]


viewRedTeam : Game -> User -> Element FrontendMsg
viewRedTeam game user =
    let
        score =
            el [] (text (getScore game Red))

        players =
            column [] <|
                List.map
                    (\p ->
                        el
                            [ if p == user.name then
                                Font.italic

                              else
                                Font.regular
                            ]
                            (text p)
                    )
                <|
                    getPlayersNamesByTeam game Red
    in
    column [ Element.width (Element.fillPortion 1), Element.padding 20, Element.spacing 5 ] [ score, players ]


viewTurnAndToggles : Game -> User -> Element FrontendMsg
viewTurnAndToggles game user =
    let
        endTurnButton =
            if isItUsersTurn user.team game.gameStatus then
                Input.button viewButtonAttributes { onPress = Just (EndingTurn game), label = text "End Turn" }

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
                , el centered (viewClueGiverToggleButton user game)
                , el centered (viewTeamToggleButton user game)
                , el centered (viewLeaveGameButton user game)
                ]

        BlueTurn ->
            row controlAttrs
                [ el centered (text "It's Blue's turn.")
                , el centered endTurnButton
                , el centered (viewClueGiverToggleButton user game)
                , el centered (viewTeamToggleButton user game)
                , el centered (viewLeaveGameButton user game)
                ]



-- VIEW CARDS


viewCardsPlaying : Game -> User -> List (Element FrontendMsg)
viewCardsPlaying game user =
    if isItUsersTurn user.team game.gameStatus then
        List.map
            (\c ->
                Element.paragraph (viewCardBaseAttributes ++ viewCardAttributesClickable c game user ++ viewCardColorIfRevealed c)
                    [ text c.word ]
            )
            game.cards

    else
        List.map
            (\c ->
                Element.paragraph (viewCardBaseAttributes ++ viewCardColorIfRevealed c)
                    [ text c.word ]
            )
            game.cards


viewCardsAllRevealed : Game -> List (Element FrontendMsg)
viewCardsAllRevealed game =
    List.map
        (\c ->
            el (viewCardBaseAttributes ++ viewCardColorAttributes c)
                (text c.word)
        )
        game.cards



-- VIEW BUTTONS


viewClueGiverToggleButton : User -> Game -> Element FrontendMsg
viewClueGiverToggleButton user game =
    let
        labeltext =
            if user.cluegiver then
                text "Stop being clue giver"

            else
                text "Become clue giver"
    in
    Input.button viewButtonAttributes { onPress = Just (ToggleClueGiverStatus user game), label = labeltext }


viewTeamToggleButton : User -> Game -> Element FrontendMsg
viewTeamToggleButton user game =
    let
        labeltext =
            if user.team == Red then
                text "Switch to Blue team"

            else
                text "Switch to Red team"
    in
    Input.button viewButtonAttributes { onPress = Just (ToggleTeam user (Just game)), label = labeltext }


viewLeaveGameButton : User -> Game -> Element FrontendMsg
viewLeaveGameButton user game =
    Input.button viewButtonAttributes
        { onPress = Just (LeavingGame user game)
        , label = text "Quit"
        }



-- VIEW FORMS


viewCreateUserForm : NewUserSettings -> Element FrontendMsg
viewCreateUserForm setting =
    row [ Element.width Element.fill, Element.height Element.fill, Element.padding 50, Element.spacing 5, Element.centerY ]
        [ column [ Element.width (Element.fillPortion 1) ] []
        , column [ Element.width (Element.fillPortion 3) ]
            [ row [ Element.width Element.fill ]
                [ column [ centerX, Element.spacing 10, Element.width (Element.fillPortion 1) ] []
                , column [ centerX, Element.spacing 10, Element.width (Element.fillPortion 1) ]
                    [ row [ Element.width Element.fill ]
                        [ Input.text [ Element.spacing 5, onEnter (NewUser setting), Element.width (Element.fill |> Element.maximum 800 |> Element.minimum 200) ]
                            { label = Input.labelLeft [] (text "Username")
                            , onChange = ChangeNewUserSettingUsername
                            , placeholder = Nothing
                            , text = setting.username
                            }
                        ]
                    , row [ Element.width Element.fill ]
                        [ Input.radioRow
                            [ Element.spacing 10 ]
                            { label = Input.labelLeft [ Element.paddingEach { bottom = 0, left = 0, right = 50, top = 0 } ] (text "Team")
                            , onChange = ChangeNewUserSettingTeam
                            , selected = Just (teamToString setting.team)
                            , options = [ Input.option "Blue" (text "Blue"), Input.option "Red" (text "Red") ]
                            }
                        ]
                    , row [ Element.width Element.fill ]
                        [ Input.button
                            (viewButtonAttributes ++ [ Element.centerX, Element.width (Element.px 150) ])
                            { onPress = Just (NewUser setting)
                            , label = text "Start"
                            }
                        ]
                    ]
                , column [ centerX, Element.spacing 10, Element.width (Element.fillPortion 1) ] []
                ]
            ]
        , column [ Element.width (Element.fillPortion 1) ] []
        ]


viewCreateGameForm : User -> NewGameSettings -> Element FrontendMsg
viewCreateGameForm user setting =
    column [ Element.width (Element.fillPortion 3) ]
        [ el [ Element.paddingXY 0 20 ] (text "Create a new game, then send the URL to your friends so they can join you.")
        , column [ Element.width Element.fill, Element.spacingXY 5 10 ]
            [ row [ Element.width Element.fill ]
                [ Input.radioRow [ Element.spacing 10 ]
                    { onChange = ChangeNewGameSettingGridSize
                    , selected = Just (gridSizeToString setting.gridSize)
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
                    , selected = Just (teamToString setting.startingTeam)
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
                    { onPress = Just (CreatingNewGame user setting)
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


viewCardAttributesClickable : Card -> Game -> User -> List (Element.Attribute FrontendMsg)
viewCardAttributesClickable card game user =
    [ Element.pointer
    , Element.mouseOver [ Border.glow (rgb 0.5 0.5 0.5) 0.5 ]
    , onClick (RevealingCard card game user)
    ]


viewCardBaseAttributes : List (Element.Attribute msg)
viewCardBaseAttributes =
    [ Element.width (Element.fill |> Element.maximum 250 |> Element.minimum 250)
    , Element.spacing 50
    , Element.padding 40
    , Font.center
    , Element.alignLeft
    , Border.rounded 5
    , Border.width 1
    , Border.solid
    ]


viewCardColorAttributes : Card -> List (Element.Attribute FrontendMsg)
viewCardColorAttributes card =
    let
        cardColorAttr =
            Background.color (cardCardAlignmentToRgb card.team)
    in
    case card.team of
        Assassin ->
            [ cardColorAttr, Font.color (rgb 1 1 1) ]

        BlueCard ->
            [ cardColorAttr, Font.color (rgb 1 1 1) ]

        _ ->
            [ cardColorAttr ]


viewCardColorIfRevealed : Card -> List (Element.Attribute FrontendMsg)
viewCardColorIfRevealed card =
    if card.revealed then
        viewCardColorAttributes card

    else
        [ Background.color (rgb 1 1 1) ]
