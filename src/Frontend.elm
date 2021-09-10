module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Cards exposing (cardCardAlignmentToString)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Lamdera exposing (sendToBackend)
import Types exposing (..)
import Url


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
    ( { key = key

      --   , user = Just testUser
      , user = Nothing
      , activeGame = Nothing
      , newGameSettings = NewGameSettings True MediumGrid Blue
      , newUserSettings = NewUserSettings "" Blue
      , publicGames = []
      }
    , sendToBackend GetPublicGames
    )


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
            ( { model | user = Just (User model.newUserSettings.username model.newUserSettings.team False []) }, Cmd.none )

        CreatingNewGame ->
            ( model, sendToBackend (CreateNewGame model.newGameSettings) )

        JoiningGame id ->
            ( model, sendToBackend (JoinGame id) )

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

        ToggleNewGameSettingPublic value ->
            ( { model | newGameSettings = toggleNewGameSettingPublic model.newGameSettings value }, Cmd.none )

        ChangeNewGameSettingGridSize gridSize ->
            ( { model | newGameSettings = setNewGameSettingGridSize model.newGameSettings gridSize }, Cmd.none )

        ChangeNewUserSettingTeam team ->
            ( { model | newUserSettings = setNewUserSettingTeam model.newUserSettings (teamFromString team) }, Cmd.none )

        ChangeNewUserSettingUsername username ->
            ( { model | newUserSettings = setNewUserSettingUsername model.newUserSettings username }, Cmd.none )

        _ ->
            -- Debug.todo "Finish FrontendMsg updates"
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        ActiveGame game ->
            ( { model | activeGame = Just game }, Cmd.none )

        PublicGames publicGames ->
            ( { model | publicGames = publicGames }, Cmd.none )

        _ ->
            -- Debug.todo "Implement other branches"
            ( model, Cmd.none )


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


setNewGameSettingGridSize : NewGameSettings -> String -> NewGameSettings
setNewGameSettingGridSize oldSettings gridSize =
    { oldSettings | gridSize = gridSizeFromString gridSize }


setNewUserSettingUsername : NewUserSettings -> String -> NewUserSettings
setNewUserSettingUsername oldSettings username =
    { oldSettings | username = username }


setNewUserSettingTeam : NewUserSettings -> Team -> NewUserSettings
setNewUserSettingTeam oldSettings team =
    { oldSettings | team = team }


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


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ viewSwitch model ]
            ]
        ]
    }


viewSwitch : Model -> Html.Html FrontendMsg
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


viewLandingPage : Model -> Html.Html FrontendMsg
viewLandingPage model =
    Html.div []
        [ Html.div [] [ Html.text "Welcome" ]
        , Html.div []
            [ Html.form [ onSubmit NewUser ]
                [ Html.div []
                    [ Html.label [ Attr.for "username" ] [ Html.text "Username" ]
                    , Html.input [ Attr.id "username", onInput ChangeNewUserSettingUsername, Attr.value model.newUserSettings.username ] []
                    ]
                , Html.div []
                    [ Html.label [ Attr.for "team" ] [ Html.text "Team" ]
                    , Html.select [ Attr.id "team", Attr.value (teamToString model.newUserSettings.team), onInput ChangeNewUserSettingTeam ]
                        [ Html.option [ Attr.value "Blue" ] [ Html.text "Blue" ]
                        , Html.option [ Attr.value "Red" ] [ Html.text "Red" ]
                        ]
                    ]
                , Html.button [ Attr.type_ "submit" ] [ Html.text "Start" ]
                ]
            ]
        ]


viewLobby : Model -> Html.Html FrontendMsg
viewLobby model =
    Html.div []
        [ Html.div []
            [ viewCreateGameForm model
            , viewPublicGames model
            ]
        ]


viewCreateGameForm : Model -> Html.Html FrontendMsg
viewCreateGameForm model =
    Html.div []
        [ Html.form [ onSubmit CreatingNewGame ]
            [ Html.label [ Attr.for "public" ] [ Html.text "Public?" ]
            , Html.input [ Attr.type_ "checkbox", Attr.id "public", onCheck ToggleNewGameSettingPublic, Attr.checked model.newGameSettings.public ] []
            , Html.label [ Attr.for "gridsize" ] [ Html.text "Size of grid:" ]
            , Html.select [ Attr.id "gridsize", onInput ChangeNewGameSettingGridSize, Attr.value (gridSizeToString model.newGameSettings.gridSize) ]
                [ Html.option [ Attr.value "Small" ] [ Html.text "Small" ]
                , Html.option [ Attr.value "Medium" ] [ Html.text "Medium" ]
                , Html.option [ Attr.value "Large" ] [ Html.text "Large" ]
                ]
            , Html.button [ Attr.type_ "submit" ] [ Html.text "Create Game" ]
            ]
        ]


viewPublicGames : Model -> Html.Html FrontendMsg
viewPublicGames model =
    Html.div []
        (List.map
            (\g -> Html.div [ onClick (JoiningGame g.id) ] [ Html.text ("Game " ++ String.fromInt g.id) ])
            model.publicGames
        )


viewGame : Game -> User -> Html.Html FrontendMsg
viewGame game user =
    let
        gameBoardWrapper =
            Html.div [ Attr.style "display" "flex", Attr.style "width" "80%", Attr.style "flex-wrap" "wrap", Attr.style "margin" "auto auto" ]

        gameOver =
            Html.div []
                [ viewScore game
                , gameBoardWrapper (viewCardsGameOver game.cards)
                ]

        usersTurn =
            isItUsersTurn user.team game.gameStatus

        endTurn =
            if usersTurn then
                Html.button [ onClick EndingTurn ] [ Html.text "End Turn" ]

            else
                Html.button [ Attr.disabled True ] [ Html.text "End Turn" ]

        gamePlaying =
            Html.div []
                [ viewScore game
                , endTurn
                , gameBoardWrapper (viewCardsPlaying game.cards usersTurn)
                ]
    in
    case game.gameStatus of
        RedWon ->
            gameOver

        BlueWon ->
            gameOver

        RedTurn ->
            gamePlaying

        BlueTurn ->
            gamePlaying


viewScore : Game -> Html.Html FrontendMsg
viewScore game =
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

        blue =
            List.filter (\c -> c.team == BlueCard) game.cards
                |> List.filter (\c -> c.revealed)
                |> List.length
                |> String.fromInt

        blueTotal =
            List.filter (\c -> c.team == BlueCard) game.cards
                |> List.length
                |> String.fromInt

        score =
            [ Html.div [] [ Html.text ("Blue: " ++ blue ++ " / " ++ blueTotal) ]
            , Html.div [] [ Html.text ("Red: " ++ red ++ " / " ++ redTotal) ]
            ]
    in
    case game.gameStatus of
        RedWon ->
            Html.div []
                [ Html.div [ Attr.style "color" "red" ] [ Html.text "Red Won!" ]
                , Html.div [] score
                ]

        BlueWon ->
            Html.div []
                [ Html.div [ Attr.style "color" "blue" ] [ Html.text "Blue Won!" ]
                , Html.div [] score
                ]

        RedTurn ->
            Html.div []
                [ Html.div [ Attr.style "color" "red" ] [ Html.text "It's Red's turn." ]
                , Html.div [] score
                ]

        BlueTurn ->
            Html.div []
                [ Html.div [ Attr.style "color" "blue" ] [ Html.text "It's Blue's turn." ]
                , Html.div [] score
                ]


viewCardsPlaying : List Card -> Bool -> List (Html.Html FrontendMsg)
viewCardsPlaying cardList clickable =
    List.map
        (\c ->
            Html.div
                (viewCardAttributes c clickable ++ viewCardColorIfRevealed c)
                [ Html.text c.word ]
        )
        cardList


viewCardAttributes : Card -> Bool -> List (Html.Attribute FrontendMsg)
viewCardAttributes card clickable =
    let
        base =
            [ Attr.style "width" "10%"
            , Attr.style "flex-grow" "5"
            , Attr.style "padding" "3.5%"
            , Attr.style "margin" "1%"
            , Attr.style "border" "solid black 1px"
            , Attr.style "border-radius" "5px"
            ]
    in
    if clickable then
        base ++ [ onClick (RevealingCard card) ]

    else
        base


viewCardsGameOver : List Card -> List (Html.Html FrontendMsg)
viewCardsGameOver cardList =
    let
        cardAttr =
            [ Attr.style "width" "10%"
            , Attr.style "flex-grow" "5"
            , Attr.style "padding" "3.5%"
            , Attr.style "margin" "1%"
            , Attr.style "border" "solid black 1px"
            , Attr.style "border-radius" "5px"
            ]
    in
    List.map
        (\c ->
            Html.div
                (cardAttr ++ viewCardColor c)
                [ Html.text c.word ]
        )
        cardList


viewCardColorIfRevealed : Card -> List (Html.Attribute FrontendMsg)
viewCardColorIfRevealed card =
    if card.revealed then
        viewCardColor card

    else
        [ Attr.style "background-color" "white" ]


viewCardColor : Card -> List (Html.Attribute FrontendMsg)
viewCardColor card =
    let
        cardColorAttr =
            Attr.style "background-color" (cardCardAlignmentToString card.team)
    in
    case card.team of
        Assassin ->
            [ cardColorAttr
            , Attr.style "color" "white"
            ]

        _ ->
            [ cardColorAttr ]
