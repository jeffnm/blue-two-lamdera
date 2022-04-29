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
    ( { key = key
      , url = landingURL
      , user = Nothing
      , activeGame = Nothing
      , newGameSettings = NewGameSettings MediumGrid Blue
      , newUserSettings = NewUserSettings "" Blue Nothing Nothing
      }
    , Cmd.batch [ Nav.pushUrl key (Url.toString url) ]
    )



-- UPDATE


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked (Internal url) ->
            ( model
            , Nav.pushUrl model.key (Url.toString url)
            )

        UrlClicked (External url) ->
            ( model
            , Nav.load url
            )

        UrlChanged url ->
            case url.path of
                "/landing" ->
                    ( { model | url = url }, Cmd.none )

                "/lobby" ->
                    if model.user == Nothing then
                        ( { model | url = landingURL }, Nav.pushUrl model.key (Url.toString landingURL) )

                    else
                        ( { model | url = url }, Cmd.none )

                "/game" ->
                    case ( url.query, model.user ) of
                        ( Nothing, Nothing ) ->
                            ( model, Cmd.none )

                        ( Just _, Nothing ) ->
                            ( { model | url = url }, Cmd.none )

                        ( Nothing, Just _ ) ->
                            ( { model | url = lobbyURL }, Nav.pushUrl model.key (Url.toString lobbyURL) )

                        ( Just param, Just user ) ->
                            if String.startsWith "id=" param then
                                case model.activeGame of
                                    Nothing ->
                                        ( { model | url = url }
                                        , Cmd.batch [ sendToBackend (JoinGame (String.dropLeft 3 param) user) ]
                                        )

                                    Just game ->
                                        if game.id == String.dropLeft 3 param then
                                            ( { model | url = url }, Cmd.none )

                                        else
                                            ( { model | url = url }
                                            , Cmd.batch [ sendToBackend (JoinGame (String.dropLeft 3 param) user) ]
                                            )

                            else
                                ( { model | url = lobbyURL }, Nav.pushUrl model.key (Url.toString lobbyURL) )

                _ ->
                    case model.user of
                        Just _ ->
                            if model.activeGame == Nothing then
                                ( model, Nav.pushUrl model.key (Url.toString lobbyURL) )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            ( { model | url = landingURL }
                            , Nav.pushUrl model.key (Url.toString landingURL)
                            )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        NewUser ->
            let
                newUser =
                    User model.newUserSettings.username model.newUserSettings.team False model.newUserSettings.sessionId model.newUserSettings.clientId
            in
            case model.url.query of
                Nothing ->
                    ( { model | user = Just newUser }
                    , Cmd.batch
                        [ Nav.pushUrl model.key (Url.toString lobbyURL)
                        , sendToBackend (RegisterUser newUser)
                        ]
                    )

                Just param ->
                    ( { model | user = Just newUser }
                    , Cmd.batch
                        [ Nav.pushUrl model.key ("/game?" ++ param)
                        , sendToBackend (RegisterUser newUser)
                        ]
                    )

        CreatingNewGame user ->
            let
                -- No one should be cluegiver immediately when creating a game
                u =
                    { user | cluegiver = False }
            in
            ( { model | user = Just u }
            , sendToBackend (CreateNewGame model.newGameSettings u)
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

        ToggleClueGiverStatus user ->
            ( { model | user = Just (toggleClueGiver user (not user.cluegiver)) }, Cmd.none )

        ToggleTeam user ->
            let
                newUser =
                    toggleTeam user
            in
            case model.activeGame of
                Nothing ->
                    ( { model | user = Just newUser }, Cmd.none )

                Just game ->
                    ( { model | user = Just newUser }, sendToBackend (ChangeUserTeam game newUser) )

        ChangeNewGameSettingGridSize gridSize ->
            ( { model | newGameSettings = setNewGameSettingGridSize model.newGameSettings gridSize }, Cmd.none )

        ChangeNewGameSettingTeam team ->
            ( { model | newGameSettings = setNewGameSettingTeam model.newGameSettings (teamFromString team) }, Cmd.none )

        ChangeNewUserSettingTeam team ->
            ( { model | newUserSettings = setNewUserSettingTeam model.newUserSettings (teamFromString team) }, Cmd.none )

        ChangeNewUserSettingUsername username ->
            ( { model | newUserSettings = setNewUserSettingUsername model.newUserSettings username }, Cmd.none )

        LeavingGame user game ->
            ( { model | activeGame = Nothing }
            , Cmd.batch
                [ sendToBackend (LeaveGame game user)
                , Nav.pushUrl model.key (Url.toString lobbyURL)
                ]
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        ClientInfo sessionId clientId (Just user) ->
            ( { model
                | newUserSettings = setNewUserSettingSessionIdAndClientId model.newUserSettings sessionId clientId
                , user = Just user
              }
            , Cmd.batch [ Nav.pushUrl model.key (Url.toString model.url) ]
            )

        ClientInfo sessionId clientId Nothing ->
            ( { model | newUserSettings = setNewUserSettingSessionIdAndClientId model.newUserSettings sessionId clientId }
            , Cmd.none
            )

        ActiveGame game ->
            ( { model | activeGame = Just game }
            , Cmd.batch
                [ Nav.pushUrl model.key ("/game?id=" ++ game.id)
                ]
            )



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
    case ( model.user, model.activeGame ) of
        ( Nothing, _ ) ->
            viewLandingPage model

        ( Just user, Nothing ) ->
            viewLobby model user

        ( Just user, Just game ) ->
            viewGame game user


viewLandingPage : Model -> Element FrontendMsg
viewLandingPage model =
    column [ Element.width Element.fill, Element.height Element.fill ] [ viewCreateUserForm model, viewDevelopmentFooter model.url "" ]


viewLobby : Model -> User -> Element FrontendMsg
viewLobby model user =
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
                    [ viewCreateGameForm model user
                    ]
                ]
            , column [ Element.width (Element.fillPortion 1) ] []
            ]
        , viewDevelopmentFooter model.url (getUserSessionId user)
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


viewGame : Game -> User -> Element FrontendMsg
viewGame game user =
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
                , el centered (viewClueGiverToggleButton user)
                , el centered (viewTeamToggleButton user)
                , el centered (viewLeaveGameButton user game)
                ]

        BlueTurn ->
            row controlAttrs
                [ el centered (text "It's Blue's turn.")
                , el centered endTurnButton
                , el centered (viewClueGiverToggleButton user)
                , el centered (viewTeamToggleButton user)
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


viewClueGiverToggleButton : User -> Element FrontendMsg
viewClueGiverToggleButton user =
    let
        labeltext =
            if user.cluegiver then
                text "Stop being clue giver"

            else
                text "Become clue giver"
    in
    Input.button viewButtonAttributes { onPress = Just (ToggleClueGiverStatus user), label = labeltext }


viewTeamToggleButton : User -> Element FrontendMsg
viewTeamToggleButton user =
    let
        labeltext =
            if user.team == Red then
                text "Switch to Blue team"

            else
                text "Switch to Red team"
    in
    Input.button viewButtonAttributes { onPress = Just (ToggleTeam user), label = labeltext }


viewLeaveGameButton : User -> Game -> Element FrontendMsg
viewLeaveGameButton user game =
    Input.button viewButtonAttributes
        { onPress = Just (LeavingGame user game)
        , label = text "Quit"
        }



-- VIEW FORMS


viewCreateUserForm : Model -> Element FrontendMsg
viewCreateUserForm model =
    row [ Element.width Element.fill, Element.height Element.fill, Element.padding 50, Element.spacing 5, Element.centerY ]
        [ column [ Element.width (Element.fillPortion 1) ] []
        , column [ Element.width (Element.fillPortion 3) ]
            [ row [ Element.width Element.fill ]
                [ column [ centerX, Element.spacing 10, Element.width (Element.fillPortion 1) ] []
                , column [ centerX, Element.spacing 10, Element.width (Element.fillPortion 1) ]
                    [ row [ Element.width Element.fill ]
                        [ Input.text [ Element.spacing 5, onEnter NewUser, Element.width (Element.fill |> Element.maximum 800 |> Element.minimum 200) ]
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
                , column [ centerX, Element.spacing 10, Element.width (Element.fillPortion 1) ] []
                ]
            ]
        , column [ Element.width (Element.fillPortion 1) ] []
        ]


viewCreateGameForm : Model -> User -> Element FrontendMsg
viewCreateGameForm model user =
    column [ Element.width (Element.fillPortion 3) ]
        [ el [ Element.paddingXY 0 20 ] (text "Create a new game, then send the URL to your friends so they can join you.")
        , column [ Element.width Element.fill, Element.spacingXY 5 10 ]
            [ row [ Element.width Element.fill ]
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
                    { onPress = Just (CreatingNewGame user)
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
