module Frontend exposing (..)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Lamdera
import Types exposing (..)


app =
    Lamdera.frontend
        { init = init
        , view = view
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        }


init : Lamdera.Url -> Lamdera.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , page = RoleSelectionPage
      , roomCode = ""
      , roomCodeInput = ""
      , selectedRole = Nothing
      , playerRole = Nothing
      , gameState = Nothing
      , currentPuzzle = 1
      , puzzleStates = Nothing
      , error = Nothing
      , codeInput = ""
      , cipherInput = ""
      }
    , Cmd.none
    )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    let
        roleClass =
            case model.selectedRole of
                Just Player1 ->
                    "role-player1"

                Just Player2 ->
                    "role-player2"

                Nothing ->
                    ""
    in
    { title = "Escape Together"
    , body =
        [ div [ class ("app " ++ roleClass) ]
            [ case model.page of
                RoleSelectionPage ->
                    viewRoleSelection model

                HomePage ->
                    viewHomePage model

                WaitingRoom ->
                    viewWaitingRoom model

                GameRoom ->
                    viewGameRoom model

                VictoryPage ->
                    viewVictory model
            ]
        ]
    }


viewRoleSelection : FrontendModel -> Html FrontendMsg
viewRoleSelection _ =
    div [ class "role-selection-page" ]
        [ div [ class "role-header" ]
            [ h1 [] [ text "ESCAPE TOGETHER" ]
            , p [] [ text "Choisissez votre role" ]
            ]
        , div [ class "role-cards" ]
            [ div [ class "role-card role-card-1", onClick (SelectRole Player1) ]
                [ div [ class "role-icon" ] [ text "🔍" ]
                , h2 [] [ text "JOUEUR 1" ]
                , h3 [] [ text "L'Observateur" ]
                , ul [ class "role-abilities" ]
                    [ li [] [ text "Voit les symboles colores" ]
                    , li [] [ text "Controle le personnage" ]
                    , li [] [ text "Coupe les fils" ]
                    , li [] [ text "Voit le message crypte" ]
                    , li [] [ text "Actionne les interrupteurs" ]
                    ]
                , div [ class "role-motto" ] [ text "\"Je vois, tu guides\"" ]
                ]
            , div [ class "role-card role-card-2", onClick (SelectRole Player2) ]
                [ div [ class "role-icon" ] [ text "📖" ]
                , h2 [] [ text "JOUEUR 2" ]
                , h3 [] [ text "Le Stratege" ]
                , ul [ class "role-abilities" ]
                    [ li [] [ text "Connait la legende" ]
                    , li [] [ text "Voit les pieges" ]
                    , li [] [ text "Connait l'ordre de coupe" ]
                    , li [] [ text "Possede la cle de dechiffrement" ]
                    , li [] [ text "Voit l'objectif lumineux" ]
                    ]
                , div [ class "role-motto" ] [ text "\"Tu agis, je guide\"" ]
                ]
            ]
        ]


viewHomePage : FrontendModel -> Html FrontendMsg
viewHomePage model =
    let
        ( roleName, roleIcon ) =
            case model.selectedRole of
                Just Player1 ->
                    ( "L'Observateur", "🔍" )

                Just Player2 ->
                    ( "Le Stratege", "📖" )

                Nothing ->
                    ( "", "" )
    in
    div [ class "home-page" ]
        [ div [ class "selected-role-banner" ]
            [ span [ class "role-icon-small" ] [ text roleIcon ]
            , span [] [ text roleName ]
            , button [ class "btn-change-role", onClick BackToRoleSelection ] [ text "Changer" ]
            ]
        , div [ class "logo" ]
            [ h1 [] [ text "ESCAPE" ]
            , h2 [] [ text "TOGETHER" ]
            ]
        , p [ class "subtitle" ] [ text "Un escape game cooperatif pour 2 joueurs" ]
        , div [ class "home-buttons" ]
            [ button [ class "btn btn-primary", onClick CreateRoomClicked ]
                [ text "Creer une partie" ]
            , div [ class "join-section" ]
                [ input
                    [ type_ "text"
                    , placeholder "Code de la room"
                    , value model.roomCodeInput
                    , onInput RoomCodeInputChanged
                    , class "room-input"
                    ]
                    []
                , button [ class "btn btn-secondary", onClick JoinRoomClicked ]
                    [ text "Rejoindre" ]
                ]
            ]
        , case model.error of
            Just err ->
                div [ class "error" ] [ text err ]

            Nothing ->
                text ""
        , div [ class "instructions" ]
            [ h3 [] [ text "Votre mission" ]
            , case model.selectedRole of
                Just Player1 ->
                    ul []
                        [ li [] [ text "Vous VOYEZ les elements interactifs" ]
                        , li [] [ text "Vous AGISSEZ sur le jeu" ]
                        , li [] [ text "Decrivez ce que vous voyez a votre partenaire" ]
                        , li [] [ text "Suivez ses instructions pour reussir !" ]
                        ]

                Just Player2 ->
                    ul []
                        [ li [] [ text "Vous avez les INFORMATIONS cruciales" ]
                        , li [] [ text "Vous GUIDEZ votre partenaire" ]
                        , li [] [ text "Ecoutez ses descriptions attentivement" ]
                        , li [] [ text "Donnez-lui les bonnes instructions !" ]
                        ]

                Nothing ->
                    ul []
                        [ li [] [ text "Choisissez d'abord votre role" ] ]
            ]
        ]


viewWaitingRoom : FrontendModel -> Html FrontendMsg
viewWaitingRoom model =
    div [ class "waiting-room" ]
        [ h1 [] [ text "Salle d'attente" ]
        , div [ class "room-code-display" ]
            [ p [] [ text "Code de la room :" ]
            , div [ class "code" ] [ text model.roomCode ]
            ]
        , p [ class "waiting-text" ] [ text "En attente du second joueur..." ]
        , div [ class "loader" ] []
        , p [ class "role-info" ]
            [ text
                ("Vous etes le "
                    ++ (case model.playerRole of
                            Just Player1 ->
                                "Joueur 1"

                            Just Player2 ->
                                "Joueur 2"

                            Nothing ->
                                "..."
                       )
                )
            ]
        , button [ class "btn btn-danger", onClick LeaveRoom ] [ text "Quitter" ]
        ]


viewGameRoom : FrontendModel -> Html FrontendMsg
viewGameRoom model =
    case ( model.playerRole, model.puzzleStates ) of
        ( Just role, Just puzzles ) ->
            div [ class "game-room" ]
                [ viewHeader model role
                , viewPuzzle model.currentPuzzle role puzzles model
                ]

        _ ->
            div [ class "game-room" ] [ text "Chargement..." ]


viewHeader : FrontendModel -> PlayerRole -> Html FrontendMsg
viewHeader model role =
    div [ class "game-header" ]
        [ div [ class "puzzle-progress" ]
            [ text ("Enigme " ++ String.fromInt model.currentPuzzle ++ " / 5") ]
        , div [ class "player-badge" ]
            [ text
                (case role of
                    Player1 ->
                        "JOUEUR 1"

                    Player2 ->
                        "JOUEUR 2"
                )
            ]
        , div [ class "room-info" ] [ text ("Room: " ++ model.roomCode) ]
        ]


viewPuzzle : Int -> PlayerRole -> PuzzleStates -> FrontendModel -> Html FrontendMsg
viewPuzzle puzzleNum role puzzles model =
    case puzzleNum of
        1 ->
            viewColorCodePuzzle role puzzles.colorCode model

        2 ->
            viewMazePuzzle role puzzles.maze

        3 ->
            viewWiresPuzzle role puzzles.wires

        4 ->
            viewCipherPuzzle role puzzles.cipher model

        5 ->
            viewSwitchesPuzzle role puzzles.switches

        _ ->
            div [] [ text "Puzzle inconnu" ]



-- ===========================================
-- PUZZLE 1: COLOR CODE
-- ===========================================


viewColorCodePuzzle : PlayerRole -> ColorCodeState -> FrontendModel -> Html FrontendMsg
viewColorCodePuzzle role state model =
    div [ class "puzzle color-code-puzzle" ]
        [ h2 [] [ text "Enigme 1: Le Code Couleur" ]
        , if state.solved then
            viewPuzzleSolved

          else
            case role of
                Player1 ->
                    viewColorCodePlayer1 state

                Player2 ->
                    viewColorCodePlayer2 state model
        ]


viewColorCodePlayer1 : ColorCodeState -> Html FrontendMsg
viewColorCodePlayer1 state =
    div [ class "player-view" ]
        [ div [ class "view-label" ] [ text "Vous voyez des symboles colores" ]
        , p [ class "instruction" ] [ text "Decrivez ces symboles a votre partenaire !" ]
        , div [ class "symbols-display" ]
            (List.map viewColoredSymbol state.symbols)
        , p [ class "hint" ] [ text "Indice: L'ordre est important !" ]
        ]


viewColorCodePlayer2 : ColorCodeState -> FrontendModel -> Html FrontendMsg
viewColorCodePlayer2 state model =
    div [ class "player-view" ]
        [ div [ class "view-label" ] [ text "Vous voyez la legende" ]
        , p [ class "instruction" ] [ text "Demandez a votre partenaire de decrire les symboles !" ]
        , div [ class "legend-display" ]
            (List.map viewLegendItem state.legend)
        , div [ class "code-entry" ]
            [ input
                [ type_ "text"
                , placeholder "Entrez le code (4 chiffres)"
                , value model.codeInput
                , onInput CodeInputChanged
                , maxlength 4
                ]
                []
            , button [ class "btn btn-primary", onClick SubmitCode ] [ text "Valider" ]
            ]
        , div [ class "attempts" ]
            [ text ("Tentatives: " ++ String.fromInt state.attempts ++ "/3") ]
        ]


viewColoredSymbol : ColoredSymbol -> Html FrontendMsg
viewColoredSymbol cs =
    div
        [ class ("symbol symbol-" ++ colorToClass cs.color) ]
        [ text (symbolToEmoji cs.symbol) ]


viewLegendItem : ( Symbol, Int ) -> Html FrontendMsg
viewLegendItem ( sym, num ) =
    div [ class "legend-item" ]
        [ span [ class "legend-symbol" ] [ text (symbolToEmoji sym) ]
        , span [ class "legend-arrow" ] [ text " = " ]
        , span [ class "legend-number" ] [ text (String.fromInt num) ]
        ]


symbolToEmoji : Symbol -> String
symbolToEmoji sym =
    case sym of
        Star ->
            "ETOILE"

        Moon ->
            "LUNE"

        Sun ->
            "SOLEIL"

        Heart ->
            "COEUR"

        Diamond ->
            "DIAMANT"

        Circle ->
            "CERCLE"


colorToClass : Color -> String
colorToClass c =
    case c of
        Red ->
            "red"

        Blue ->
            "blue"

        Green ->
            "green"

        Yellow ->
            "yellow"

        Purple ->
            "purple"

        Orange ->
            "orange"



-- ===========================================
-- PUZZLE 2: MAZE
-- ===========================================


viewMazePuzzle : PlayerRole -> MazeState -> Html FrontendMsg
viewMazePuzzle role state =
    div [ class "puzzle maze-puzzle" ]
        [ h2 [] [ text "Enigme 2: Le Labyrinthe" ]
        , if state.solved then
            viewPuzzleSolved

          else if state.hitTrap then
            viewPuzzleFailed "Vous avez marche sur un piege !"

          else
            case role of
                Player1 ->
                    viewMazePlayer1 state

                Player2 ->
                    viewMazePlayer2 state
        ]


viewMazePlayer1 : MazeState -> Html FrontendMsg
viewMazePlayer1 state =
    div [ class "player-view" ]
        [ div [ class "view-label" ] [ text "Vous controlez le personnage" ]
        , p [ class "instruction" ] [ text "Votre partenaire voit les pieges. Ecoutez ses instructions !" ]
        , div [ class "maze-grid" ]
            (List.range 0 6
                |> List.map
                    (\y ->
                        div [ class "maze-row" ]
                            (List.range 0 6
                                |> List.map
                                    (\x ->
                                        let
                                            isPlayer =
                                                state.playerPos == ( x, y )

                                            isWall =
                                                List.member ( x, y ) state.walls

                                            isExit =
                                                ( x, y ) == state.exitPos

                                            cellClass =
                                                if isPlayer then
                                                    "maze-cell player"

                                                else if isWall then
                                                    "maze-cell wall"

                                                else if isExit then
                                                    "maze-cell exit"

                                                else
                                                    "maze-cell empty"
                                        in
                                        div [ class cellClass ]
                                            [ if isPlayer then
                                                text "P"

                                              else if isExit then
                                                text "X"

                                              else
                                                text ""
                                            ]
                                    )
                            )
                    )
            )
        , div [ class "controls" ]
            [ button [ class "btn-move", onClick (MovePlayer Up) ] [ text "^" ]
            , div [ class "horizontal-controls" ]
                [ button [ class "btn-move", onClick (MovePlayer Left) ] [ text "<" ]
                , button [ class "btn-move", onClick (MovePlayer Down) ] [ text "v" ]
                , button [ class "btn-move", onClick (MovePlayer Right) ] [ text ">" ]
                ]
            ]
        ]


viewMazePlayer2 : MazeState -> Html FrontendMsg
viewMazePlayer2 state =
    div [ class "player-view" ]
        [ div [ class "view-label" ] [ text "Vous voyez les pieges" ]
        , p [ class "instruction" ] [ text "Guidez votre partenaire pour eviter les pieges !" ]
        , div [ class "maze-grid trap-view" ]
            (List.range 0 6
                |> List.map
                    (\y ->
                        div [ class "maze-row" ]
                            (List.range 0 6
                                |> List.map
                                    (\x ->
                                        let
                                            isTrap =
                                                List.member ( x, y ) state.traps

                                            isWall =
                                                List.member ( x, y ) state.walls

                                            isExit =
                                                ( x, y ) == state.exitPos

                                            cellClass =
                                                if isTrap then
                                                    "maze-cell trap"

                                                else if isWall then
                                                    "maze-cell wall"

                                                else if isExit then
                                                    "maze-cell exit"

                                                else
                                                    "maze-cell empty"
                                        in
                                        div [ class cellClass ]
                                            [ if isTrap then
                                                text "!"

                                              else if isExit then
                                                text "X"

                                              else
                                                text ""
                                            ]
                                    )
                            )
                    )
            )
        , p [ class "hint" ] [ text ("Position actuelle: (" ++ String.fromInt (Tuple.first state.playerPos) ++ ", " ++ String.fromInt (Tuple.second state.playerPos) ++ ")") ]
        ]



-- ===========================================
-- PUZZLE 3: WIRES
-- ===========================================


viewWiresPuzzle : PlayerRole -> WiresState -> Html FrontendMsg
viewWiresPuzzle role state =
    div [ class "puzzle wires-puzzle" ]
        [ h2 [] [ text "Enigme 3: Les Fils" ]
        , if state.solved then
            viewPuzzleSolved

          else if state.exploded then
            viewPuzzleFailed "BOOM ! Mauvais fil !"

          else
            case role of
                Player1 ->
                    viewWiresPlayer1 state

                Player2 ->
                    viewWiresPlayer2 state
        ]


viewWiresPlayer1 : WiresState -> Html FrontendMsg
viewWiresPlayer1 state =
    let
        cutWires =
            state.cutOrder
    in
    div [ class "player-view" ]
        [ div [ class "view-label" ] [ text "Vous voyez les fils" ]
        , p [ class "instruction" ] [ text "Votre partenaire sait l'ordre. Decrivez les fils !" ]
        , div [ class "wires-display" ]
            (List.map
                (\wire ->
                    let
                        isCut =
                            List.member wire.color cutWires

                        wireClass =
                            "wire wire-" ++ wireColorToClass wire.color ++ (if isCut then " cut" else "") ++ (if wire.hasStripe then " striped" else "")
                    in
                    button
                        [ class wireClass
                        , onClick (CutWire wire.color)
                        , disabled isCut
                        ]
                        [ text
                            (wireColorToString wire.color
                                ++ (if wire.hasStripe then
                                        " (raye)"

                                    else
                                        ""
                                   )
                            )
                        ]
                )
                state.wires
            )
        , div [ class "cut-progress" ]
            [ text ("Fils coupes: " ++ String.fromInt (List.length state.cutOrder) ++ "/5") ]
        ]


viewWiresPlayer2 : WiresState -> Html FrontendMsg
viewWiresPlayer2 state =
    div [ class "player-view" ]
        [ div [ class "view-label" ] [ text "Vous avez les instructions" ]
        , p [ class "instruction" ] [ text "Dites a votre partenaire quel fil couper !" ]
        , div [ class "rules-display" ]
            [ h3 [] [ text "Ordre de coupe :" ]
            , ol []
                (List.indexedMap
                    (\i color ->
                        let
                            isDone =
                                i < List.length state.cutOrder
                        in
                        li [ class (if isDone then "done" else "") ]
                            [ text (wireColorToString color) ]
                    )
                    state.correctOrder
                )
            ]
        ]


wireColorToClass : WireColor -> String
wireColorToClass c =
    case c of
        WireRed ->
            "red"

        WireBlue ->
            "blue"

        WireYellow ->
            "yellow"

        WireGreen ->
            "green"

        WireWhite ->
            "white"


wireColorToString : WireColor -> String
wireColorToString c =
    case c of
        WireRed ->
            "ROUGE"

        WireBlue ->
            "BLEU"

        WireYellow ->
            "JAUNE"

        WireGreen ->
            "VERT"

        WireWhite ->
            "BLANC"



-- ===========================================
-- PUZZLE 4: CIPHER
-- ===========================================


viewCipherPuzzle : PlayerRole -> CipherState -> FrontendModel -> Html FrontendMsg
viewCipherPuzzle role state model =
    div [ class "puzzle cipher-puzzle" ]
        [ h2 [] [ text "Enigme 4: Le Message Code" ]
        , if state.solved then
            viewPuzzleSolved

          else
            case role of
                Player1 ->
                    viewCipherPlayer1 state

                Player2 ->
                    viewCipherPlayer2 state model
        ]


viewCipherPlayer1 : CipherState -> Html FrontendMsg
viewCipherPlayer1 state =
    div [ class "player-view" ]
        [ div [ class "view-label" ] [ text "Vous voyez le message crypte" ]
        , p [ class "instruction" ] [ text "Epeler ce message a votre partenaire !" ]
        , div [ class "encrypted-message" ]
            [ text state.encryptedMessage ]
        , p [ class "hint" ] [ text ("Indice: " ++ state.hint) ]
        ]


viewCipherPlayer2 : CipherState -> FrontendModel -> Html FrontendMsg
viewCipherPlayer2 state model =
    div [ class "player-view" ]
        [ div [ class "view-label" ] [ text "Vous avez la cle de dechiffrement" ]
        , p [ class "instruction" ] [ text "Demandez les lettres et decodez !" ]
        , div [ class "decryption-key" ]
            (state.decryptionKey
                |> Dict.toList
                |> List.map
                    (\( encrypted, original ) ->
                        div [ class "key-item" ]
                            [ span [ class "encrypted" ] [ text (String.fromChar encrypted) ]
                            , span [] [ text " -> " ]
                            , span [ class "decoded" ] [ text (String.fromChar original) ]
                            ]
                    )
            )
        , div [ class "answer-entry" ]
            [ input
                [ type_ "text"
                , placeholder "Entrez le mot decode"
                , value model.cipherInput
                , onInput CipherInputChanged
                ]
                []
            , button [ class "btn btn-primary", onClick SubmitCipher ] [ text "Valider" ]
            ]
        ]



-- ===========================================
-- PUZZLE 5: SWITCHES
-- ===========================================


viewSwitchesPuzzle : PlayerRole -> SwitchesState -> Html FrontendMsg
viewSwitchesPuzzle role state =
    div [ class "puzzle switches-puzzle" ]
        [ h2 [] [ text "Enigme 5: Les Interrupteurs" ]
        , if state.solved then
            viewPuzzleSolved

          else
            case role of
                Player1 ->
                    viewSwitchesPlayer1 state

                Player2 ->
                    viewSwitchesPlayer2 state
        ]


viewSwitchesPlayer1 : SwitchesState -> Html FrontendMsg
viewSwitchesPlayer1 state =
    div [ class "player-view" ]
        [ div [ class "view-label" ] [ text "Vous controlez les interrupteurs" ]
        , p [ class "instruction" ] [ text "Votre partenaire voit les voyants. Suivez ses instructions !" ]
        , div [ class "switches-panel" ]
            (List.indexedMap
                (\i isOn ->
                    button
                        [ class
                            ("switch"
                                ++ (if isOn then
                                        " on"

                                    else
                                        " off"
                                   )
                            )
                        , onClick (ToggleSwitch i)
                        ]
                        [ text (String.fromInt (i + 1)) ]
                )
                state.switches
            )
        ]


viewSwitchesPlayer2 : SwitchesState -> Html FrontendMsg
viewSwitchesPlayer2 state =
    div [ class "player-view" ]
        [ div [ class "view-label" ] [ text "Vous voyez les voyants" ]
        , p [ class "instruction" ] [ text "Guidez votre partenaire pour atteindre la configuration cible !" ]
        , div [ class "lights-panel" ]
            [ h4 [] [ text "Etat actuel:" ]
            , div [ class "lights" ]
                (List.indexedMap
                    (\i isOn ->
                        div
                            [ class
                                ("light"
                                    ++ (if isOn then
                                            " on"

                                        else
                                            " off"
                                       )
                                )
                            ]
                            [ text (String.fromInt (i + 1)) ]
                    )
                    state.switches
                )
            ]
        , div [ class "lights-panel target" ]
            [ h4 [] [ text "Objectif:" ]
            , div [ class "lights" ]
                (List.indexedMap
                    (\i isOn ->
                        div
                            [ class
                                ("light target-light"
                                    ++ (if isOn then
                                            " on"

                                        else
                                            " off"
                                       )
                                )
                            ]
                            [ text (String.fromInt (i + 1)) ]
                    )
                    state.targetPattern
                )
            ]
        , div [ class "effect-hint" ]
            [ h4 [] [ text "Chaque interrupteur affecte plusieurs voyants !" ]
            ]
        ]



-- ===========================================
-- COMMON VIEWS
-- ===========================================


viewPuzzleSolved : Html FrontendMsg
viewPuzzleSolved =
    div [ class "puzzle-result success" ]
        [ h3 [] [ text "ENIGME RESOLUE !" ]
        , button [ class "btn btn-primary", onClick NextPuzzle ] [ text "Continuer" ]
        ]


viewPuzzleFailed : String -> Html FrontendMsg
viewPuzzleFailed reason =
    div [ class "puzzle-result failure" ]
        [ h3 [] [ text "ECHEC !" ]
        , p [] [ text reason ]
        , button [ class "btn btn-warning", onClick ResetPuzzle ] [ text "Recommencer" ]
        ]


viewVictory : FrontendModel -> Html FrontendMsg
viewVictory _ =
    div [ class "victory-page" ]
        [ h1 [] [ text "FELICITATIONS !" ]
        , div [ class "victory-text" ]
            [ p [] [ text "Vous vous etes echappes ensemble !" ]
            , p [] [ text "Votre communication a ete la cle du succes." ]
            ]
        , button [ class "btn btn-primary", onClick LeaveRoom ] [ text "Nouvelle partie" ]
        ]



-- ===========================================
-- UPDATE
-- ===========================================


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        SelectRole role ->
            ( { model | selectedRole = Just role, page = HomePage }, Cmd.none )

        BackToRoleSelection ->
            ( { model | page = RoleSelectionPage }, Cmd.none )

        CreateRoomClicked ->
            case model.selectedRole of
                Just role ->
                    ( { model | error = Nothing }, Lamdera.sendToBackend (CreateRoom role) )

                Nothing ->
                    ( { model | error = Just "Choisissez d'abord un role" }, Cmd.none )

        JoinRoomClicked ->
            case model.selectedRole of
                Just role ->
                    if String.length model.roomCodeInput >= 4 then
                        ( { model | error = Nothing }, Lamdera.sendToBackend (JoinRoom model.roomCodeInput role) )

                    else
                        ( { model | error = Just "Entrez un code de 4 caracteres" }, Cmd.none )

                Nothing ->
                    ( { model | error = Just "Choisissez d'abord un role" }, Cmd.none )

        RoomCodeInputChanged input ->
            ( { model | roomCodeInput = String.toUpper input }, Cmd.none )

        CodeInputChanged input ->
            ( { model | codeInput = input }, Cmd.none )

        SubmitCode ->
            ( model, Lamdera.sendToBackend (PlayerAction (SubmitColorCode model.codeInput)) )

        MovePlayer dir ->
            ( model, Lamdera.sendToBackend (PlayerAction (Move dir)) )

        CutWire color ->
            ( model, Lamdera.sendToBackend (PlayerAction (Cut color)) )

        CipherInputChanged input ->
            ( { model | cipherInput = input }, Cmd.none )

        SubmitCipher ->
            ( model, Lamdera.sendToBackend (PlayerAction (SubmitCipherAnswer model.cipherInput)) )

        ToggleSwitch idx ->
            ( model, Lamdera.sendToBackend (PlayerAction (FlipSwitch idx)) )

        NextPuzzle ->
            ( model, Lamdera.sendToBackend RequestNextPuzzle )

        ResetPuzzle ->
            ( model, Lamdera.sendToBackend RequestResetPuzzle )

        LeaveRoom ->
            ( { model
                | page = RoleSelectionPage
                , roomCode = ""
                , selectedRole = Nothing
                , playerRole = Nothing
                , gameState = Nothing
                , puzzleStates = Nothing
                , currentPuzzle = 1
                , codeInput = ""
                , cipherInput = ""
              }
            , Lamdera.sendToBackend LeaveRoomRequest
            )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        RoomCreated code role ->
            ( { model
                | page = WaitingRoom
                , roomCode = code
                , playerRole = Just role
              }
            , Cmd.none
            )

        RoomJoined code role ->
            ( { model
                | page = WaitingRoom
                , roomCode = code
                , playerRole = Just role
              }
            , Cmd.none
            )

        RoomError err ->
            ( { model | error = Just err }, Cmd.none )

        GameStateUpdate state puzzle puzzles ->
            let
                newPage =
                    case state of
                        Victory ->
                            VictoryPage

                        Playing ->
                            GameRoom

                        _ ->
                            model.page
            in
            ( { model
                | page = newPage
                , gameState = Just state
                , currentPuzzle = puzzle
                , puzzleStates = Just puzzles
              }
            , Cmd.none
            )

        PlayerLeft ->
            ( { model
                | page = WaitingRoom
                , gameState = Just WaitingForPlayers
              }
            , Cmd.none
            )

        PuzzleSolved _ ->
            case model.puzzleStates of
                Just puzzles ->
                    let
                        newPuzzles =
                            updatePuzzleSolvedState model.currentPuzzle puzzles
                    in
                    ( { model | puzzleStates = Just newPuzzles }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        PuzzleFailed _ ->
            case model.puzzleStates of
                Just puzzles ->
                    let
                        newPuzzles =
                            updatePuzzleFailedState model.currentPuzzle puzzles
                    in
                    ( { model | puzzleStates = Just newPuzzles }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


updatePuzzleSolvedState : Int -> PuzzleStates -> PuzzleStates
updatePuzzleSolvedState puzzleNum puzzles =
    case puzzleNum of
        1 ->
            let
                cc =
                    puzzles.colorCode
            in
            { puzzles | colorCode = { cc | solved = True } }

        2 ->
            let
                m =
                    puzzles.maze
            in
            { puzzles | maze = { m | solved = True } }

        3 ->
            let
                w =
                    puzzles.wires
            in
            { puzzles | wires = { w | solved = True } }

        4 ->
            let
                c =
                    puzzles.cipher
            in
            { puzzles | cipher = { c | solved = True } }

        5 ->
            let
                s =
                    puzzles.switches
            in
            { puzzles | switches = { s | solved = True } }

        _ ->
            puzzles


updatePuzzleFailedState : Int -> PuzzleStates -> PuzzleStates
updatePuzzleFailedState puzzleNum puzzles =
    case puzzleNum of
        2 ->
            let
                m =
                    puzzles.maze
            in
            { puzzles | maze = { m | hitTrap = True } }

        3 ->
            let
                w =
                    puzzles.wires
            in
            { puzzles | wires = { w | exploded = True } }

        _ ->
            puzzles


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.none
