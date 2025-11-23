module Backend exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId, onDisconnect, sendToFrontend)
import Types exposing (..)


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { rooms = Dict.empty
      , nextSeed = 12345
      }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        ClientDisconnected _ clientId ->
            let
                ( updatedRooms, cmds ) =
                    Dict.foldl
                        (\code room ( accRooms, accCmds ) ->
                            if room.player1 == Just clientId then
                                let
                                    newRoom =
                                        { room | player1 = Nothing, gameState = WaitingForPlayers }

                                    notifyCmd =
                                        case room.player2 of
                                            Just p2 ->
                                                sendToFrontend p2 PlayerLeft

                                            Nothing ->
                                                Cmd.none
                                in
                                ( Dict.insert code newRoom accRooms, notifyCmd :: accCmds )

                            else if room.player2 == Just clientId then
                                let
                                    newRoom =
                                        { room | player2 = Nothing, gameState = WaitingForPlayers }

                                    notifyCmd =
                                        case room.player1 of
                                            Just p1 ->
                                                sendToFrontend p1 PlayerLeft

                                            Nothing ->
                                                Cmd.none
                                in
                                ( Dict.insert code newRoom accRooms, notifyCmd :: accCmds )

                            else
                                ( Dict.insert code room accRooms, accCmds )
                        )
                        ( Dict.empty, [] )
                        model.rooms
            in
            ( { model | rooms = updatedRooms }, Cmd.batch cmds )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        CreateRoom ->
            let
                roomCode =
                    generateRoomCode model.nextSeed

                puzzles =
                    generatePuzzles model.nextSeed

                newRoom =
                    { code = roomCode
                    , player1 = Just clientId
                    , player2 = Nothing
                    , gameState = WaitingForPlayers
                    , currentPuzzle = 1
                    , puzzleStates = puzzles
                    , seed = model.nextSeed
                    }
            in
            ( { model
                | rooms = Dict.insert roomCode newRoom model.rooms
                , nextSeed = model.nextSeed + 1
              }
            , sendToFrontend clientId (RoomCreated roomCode Player1)
            )

        JoinRoom roomCode ->
            case Dict.get (String.toUpper roomCode) model.rooms of
                Nothing ->
                    ( model, sendToFrontend clientId (RoomError "Room not found") )

                Just room ->
                    if room.player1 == Nothing then
                        let
                            newRoom =
                                { room | player1 = Just clientId }

                            updatedRoom =
                                if newRoom.player2 /= Nothing then
                                    { newRoom | gameState = Playing }

                                else
                                    newRoom

                            cmds =
                                if updatedRoom.gameState == Playing then
                                    Cmd.batch
                                        [ sendToFrontend clientId (RoomJoined roomCode Player1)
                                        , sendToFrontend clientId (GameStateUpdate updatedRoom.gameState updatedRoom.currentPuzzle updatedRoom.puzzleStates)
                                        , case updatedRoom.player2 of
                                            Just p2 ->
                                                sendToFrontend p2 (GameStateUpdate updatedRoom.gameState updatedRoom.currentPuzzle updatedRoom.puzzleStates)

                                            Nothing ->
                                                Cmd.none
                                        ]

                                else
                                    sendToFrontend clientId (RoomJoined roomCode Player1)
                        in
                        ( { model | rooms = Dict.insert roomCode updatedRoom model.rooms }, cmds )

                    else if room.player2 == Nothing then
                        let
                            newRoom =
                                { room | player2 = Just clientId, gameState = Playing }

                            cmds =
                                Cmd.batch
                                    [ sendToFrontend clientId (RoomJoined roomCode Player2)
                                    , sendToFrontend clientId (GameStateUpdate newRoom.gameState newRoom.currentPuzzle newRoom.puzzleStates)
                                    , case newRoom.player1 of
                                        Just p1 ->
                                            sendToFrontend p1 (GameStateUpdate newRoom.gameState newRoom.currentPuzzle newRoom.puzzleStates)

                                        Nothing ->
                                            Cmd.none
                                    ]
                        in
                        ( { model | rooms = Dict.insert roomCode newRoom model.rooms }, cmds )

                    else
                        ( model, sendToFrontend clientId (RoomError "Room is full") )

        PlayerAction action ->
            handlePlayerAction clientId action model

        RequestNextPuzzle ->
            case findRoomByClient clientId model.rooms of
                Nothing ->
                    ( model, Cmd.none )

                Just room ->
                    if room.currentPuzzle >= 5 then
                        let
                            newRoom =
                                { room | gameState = Victory }

                            cmds =
                                broadcastToRoom newRoom (GameStateUpdate Victory room.currentPuzzle room.puzzleStates)
                        in
                        ( { model | rooms = Dict.insert room.code newRoom model.rooms }, cmds )

                    else
                        let
                            newRoom =
                                { room | currentPuzzle = room.currentPuzzle + 1 }

                            cmds =
                                broadcastToRoom newRoom (GameStateUpdate Playing newRoom.currentPuzzle newRoom.puzzleStates)
                        in
                        ( { model | rooms = Dict.insert room.code newRoom model.rooms }, cmds )

        RequestResetPuzzle ->
            case findRoomByClient clientId model.rooms of
                Nothing ->
                    ( model, Cmd.none )

                Just room ->
                    let
                        newPuzzles =
                            resetCurrentPuzzle room.currentPuzzle room.seed room.puzzleStates

                        newRoom =
                            { room | puzzleStates = newPuzzles }

                        cmds =
                            broadcastToRoom newRoom (GameStateUpdate Playing newRoom.currentPuzzle newRoom.puzzleStates)
                    in
                    ( { model | rooms = Dict.insert room.code newRoom model.rooms }, cmds )

        LeaveRoomRequest ->
            case findRoomByClient clientId model.rooms of
                Nothing ->
                    ( model, Cmd.none )

                Just room ->
                    let
                        ( newRoom, otherPlayer ) =
                            if room.player1 == Just clientId then
                                ( { room | player1 = Nothing, gameState = WaitingForPlayers }, room.player2 )

                            else
                                ( { room | player2 = Nothing, gameState = WaitingForPlayers }, room.player1 )

                        notifyCmd =
                            case otherPlayer of
                                Just p ->
                                    sendToFrontend p PlayerLeft

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model | rooms = Dict.insert room.code newRoom model.rooms }, notifyCmd )


handlePlayerAction : ClientId -> PlayerAction -> BackendModel -> ( BackendModel, Cmd BackendMsg )
handlePlayerAction clientId action model =
    case findRoomByClient clientId model.rooms of
        Nothing ->
            ( model, Cmd.none )

        Just room ->
            let
                puzzles =
                    room.puzzleStates

                ( newPuzzles, result ) =
                    case action of
                        SubmitColorCode code ->
                            processColorCode code puzzles

                        Move dir ->
                            processMaze dir puzzles

                        Cut wireColor ->
                            processWireCut wireColor puzzles

                        SubmitCipherAnswer answer ->
                            processCipher answer puzzles

                        FlipSwitch idx ->
                            processSwitch idx puzzles

                newRoom =
                    { room | puzzleStates = newPuzzles }

                cmds =
                    case result of
                        PuzzleOk ->
                            broadcastToRoom newRoom (GameStateUpdate Playing newRoom.currentPuzzle newRoom.puzzleStates)

                        PuzzleWon ->
                            broadcastToRoom newRoom (PuzzleSolved newRoom.currentPuzzle)

                        PuzzleLost reason ->
                            broadcastToRoom newRoom (PuzzleFailed reason)
            in
            ( { model | rooms = Dict.insert room.code newRoom model.rooms }, cmds )


type PuzzleResult
    = PuzzleOk
    | PuzzleWon
    | PuzzleLost String


processColorCode : String -> PuzzleStates -> ( PuzzleStates, PuzzleResult )
processColorCode code puzzles =
    let
        cc =
            puzzles.colorCode

        newAttempts =
            cc.attempts + 1
    in
    if String.toUpper code == cc.correctCode then
        ( { puzzles | colorCode = { cc | solved = True, enteredCode = code } }, PuzzleWon )

    else if newAttempts >= 3 then
        ( { puzzles | colorCode = { cc | attempts = newAttempts, enteredCode = code } }, PuzzleLost "Too many wrong attempts!" )

    else
        ( { puzzles | colorCode = { cc | attempts = newAttempts, enteredCode = code } }, PuzzleOk )


processMaze : Direction -> PuzzleStates -> ( PuzzleStates, PuzzleResult )
processMaze dir puzzles =
    let
        maze =
            puzzles.maze

        ( x, y ) =
            maze.playerPos

        newPos =
            case dir of
                Up ->
                    ( x, y - 1 )

                Down ->
                    ( x, y + 1 )

                Left ->
                    ( x - 1, y )

                Right ->
                    ( x + 1, y )

        ( nx, ny ) =
            newPos

        isWall =
            List.member newPos maze.walls || nx < 0 || nx > 6 || ny < 0 || ny > 6

        isTrap =
            List.member newPos maze.traps

        isExit =
            newPos == maze.exitPos
    in
    if isWall then
        ( puzzles, PuzzleOk )

    else if isTrap then
        ( { puzzles | maze = { maze | playerPos = newPos, hitTrap = True } }, PuzzleLost "You hit a trap!" )

    else if isExit then
        ( { puzzles | maze = { maze | playerPos = newPos, solved = True } }, PuzzleWon )

    else
        ( { puzzles | maze = { maze | playerPos = newPos } }, PuzzleOk )


processWireCut : WireColor -> PuzzleStates -> ( PuzzleStates, PuzzleResult )
processWireCut color puzzles =
    let
        wires =
            puzzles.wires

        newCutOrder =
            wires.cutOrder ++ [ color ]

        expectedSoFar =
            List.take (List.length newCutOrder) wires.correctOrder

        isCorrectSoFar =
            newCutOrder == expectedSoFar

        isComplete =
            List.length newCutOrder == List.length wires.correctOrder
    in
    if not isCorrectSoFar then
        ( { puzzles | wires = { wires | cutOrder = newCutOrder, exploded = True } }, PuzzleLost "BOOM! Wrong wire!" )

    else if isComplete then
        ( { puzzles | wires = { wires | cutOrder = newCutOrder, solved = True } }, PuzzleWon )

    else
        ( { puzzles | wires = { wires | cutOrder = newCutOrder } }, PuzzleOk )


processCipher : String -> PuzzleStates -> ( PuzzleStates, PuzzleResult )
processCipher answer puzzles =
    let
        cipher =
            puzzles.cipher
    in
    if String.toUpper (String.trim answer) == String.toUpper cipher.correctAnswer then
        ( { puzzles | cipher = { cipher | solved = True, enteredAnswer = answer } }, PuzzleWon )

    else
        ( { puzzles | cipher = { cipher | enteredAnswer = answer } }, PuzzleOk )


processSwitch : Int -> PuzzleStates -> ( PuzzleStates, PuzzleResult )
processSwitch idx puzzles =
    let
        sw =
            puzzles.switches

        newSwitches =
            List.indexedMap
                (\i v ->
                    if i == idx then
                        not v

                    else
                        v
                )
                sw.switches

        effects =
            List.drop idx sw.switchEffects |> List.head |> Maybe.withDefault []

        toggleLight : Int -> List Bool -> List Bool
        toggleLight lightIdx lights =
            List.indexedMap
                (\i v ->
                    if i == lightIdx then
                        not v

                    else
                        v
                )
                lights

        newLights =
            List.foldl toggleLight newSwitches effects

        isSolved =
            newLights == sw.targetPattern
    in
    if isSolved then
        ( { puzzles | switches = { sw | switches = newLights, solved = True } }, PuzzleWon )

    else
        ( { puzzles | switches = { sw | switches = newLights } }, PuzzleOk )


findRoomByClient : ClientId -> Dict RoomCode Room -> Maybe Room
findRoomByClient clientId rooms =
    Dict.values rooms
        |> List.filter (\r -> r.player1 == Just clientId || r.player2 == Just clientId)
        |> List.head


broadcastToRoom : Room -> ToFrontend -> Cmd BackendMsg
broadcastToRoom room msg =
    Cmd.batch
        [ case room.player1 of
            Just p1 ->
                sendToFrontend p1 msg

            Nothing ->
                Cmd.none
        , case room.player2 of
            Just p2 ->
                sendToFrontend p2 msg

            Nothing ->
                Cmd.none
        ]


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    onDisconnect ClientDisconnected



-- ===========================================
-- PUZZLE GENERATION
-- ===========================================


generateRoomCode : Int -> String
generateRoomCode seed =
    let
        chars =
            "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"

        charList =
            String.toList chars

        pickChar s =
            let
                idx =
                    modBy (String.length chars) s
            in
            List.drop idx charList |> List.head |> Maybe.withDefault 'A'
    in
    String.fromList
        [ pickChar seed
        , pickChar (seed * 7 + 3)
        , pickChar (seed * 13 + 7)
        , pickChar (seed * 17 + 11)
        ]


generatePuzzles : Int -> PuzzleStates
generatePuzzles seed =
    { colorCode = generateColorCode seed
    , maze = generateMaze seed
    , wires = generateWires seed
    , cipher = generateCipher seed
    , switches = generateSwitches seed
    }


generateColorCode : Int -> ColorCodeState
generateColorCode seed =
    let
        symbols =
            [ Star, Moon, Sun, Heart ]

        colors =
            [ Red, Blue, Green, Yellow ]

        coloredSymbols =
            List.indexedMap
                (\i sym ->
                    { symbol = sym
                    , color =
                        List.drop (modBy 4 (seed + i * 3)) colors
                            |> List.head
                            |> Maybe.withDefault Red
                    }
                )
                symbols

        legend =
            List.indexedMap
                (\i sym -> ( sym, modBy 10 (seed + i * 7 + 1) ))
                symbols

        correctCode =
            coloredSymbols
                |> List.map
                    (\cs ->
                        legend
                            |> List.filter (\( s, _ ) -> s == cs.symbol)
                            |> List.head
                            |> Maybe.map (\( _, n ) -> String.fromInt n)
                            |> Maybe.withDefault "0"
                    )
                |> String.concat
    in
    { symbols = coloredSymbols
    , legend = legend
    , enteredCode = ""
    , correctCode = correctCode
    , solved = False
    , attempts = 0
    }


generateMaze : Int -> MazeState
generateMaze seed =
    let
        walls =
            [ ( 1, 1 ), ( 2, 1 ), ( 3, 1 ), ( 1, 3 ), ( 2, 3 ), ( 4, 2 ), ( 4, 3 ), ( 4, 4 ), ( 5, 4 ), ( 3, 5 ) ]

        traps =
            [ ( 2, 2 ), ( 3, 4 ), ( 5, 2 ), ( 1, 5 ) ]
    in
    { playerPos = ( 0, 0 )
    , exitPos = ( 6, 6 )
    , walls = walls
    , traps = traps
    , solved = False
    , hitTrap = False
    }


generateWires : Int -> WiresState
generateWires seed =
    let
        allColors =
            [ WireRed, WireBlue, WireYellow, WireGreen, WireWhite ]

        wires =
            List.indexedMap
                (\i c ->
                    { color = c
                    , hasStripe = modBy 2 (seed + i) == 0
                    , position = i
                    }
                )
                allColors

        correctOrder =
            case modBy 3 seed of
                0 ->
                    [ WireBlue, WireYellow, WireRed, WireWhite, WireGreen ]

                1 ->
                    [ WireYellow, WireGreen, WireBlue, WireRed, WireWhite ]

                _ ->
                    [ WireWhite, WireRed, WireGreen, WireYellow, WireBlue ]
    in
    { wires = wires
    , cutOrder = []
    , correctOrder = correctOrder
    , solved = False
    , exploded = False
    }


generateCipher : Int -> CipherState
generateCipher seed =
    let
        messages =
            [ ( "LIBERTE", "FREEDOM awaits" )
            , ( "ESCAPE", "Run away" )
            , ( "UNLOCK", "Open the door" )
            ]

        ( correctAnswer, hint ) =
            List.drop (modBy 3 seed) messages
                |> List.head
                |> Maybe.withDefault ( "ESCAPE", "Run away" )

        shift =
            modBy 5 seed + 1

        encrypt c =
            if Char.isAlpha c then
                let
                    base =
                        if Char.isUpper c then
                            Char.toCode 'A'

                        else
                            Char.toCode 'a'

                    code =
                        Char.toCode c - base

                    shifted =
                        modBy 26 (code + shift)
                in
                Char.fromCode (base + shifted)

            else
                c

        encrypted =
            String.map encrypt correctAnswer

        decryptionKey =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                |> String.toList
                |> List.map (\c -> ( encrypt c, c ))
                |> Dict.fromList
    in
    { encryptedMessage = encrypted
    , decryptionKey = decryptionKey
    , enteredAnswer = ""
    , correctAnswer = correctAnswer
    , solved = False
    , hint = hint
    }


generateSwitches : Int -> SwitchesState
generateSwitches seed =
    let
        initial =
            [ False, False, False, False, False ]

        target =
            [ True, False, True, True, False ]

        effects =
            [ [ 0, 2 ]
            , [ 1, 3 ]
            , [ 0, 1, 4 ]
            , [ 2, 3 ]
            , [ 1, 4 ]
            ]
    in
    { switches = initial
    , targetPattern = target
    , switchEffects = effects
    , solved = False
    }


resetCurrentPuzzle : Int -> Int -> PuzzleStates -> PuzzleStates
resetCurrentPuzzle puzzleNum seed puzzles =
    case puzzleNum of
        1 ->
            { puzzles | colorCode = generateColorCode seed }

        2 ->
            { puzzles | maze = generateMaze seed }

        3 ->
            { puzzles | wires = generateWires seed }

        4 ->
            { puzzles | cipher = generateCipher seed }

        5 ->
            { puzzles | switches = generateSwitches seed }

        _ ->
            puzzles
