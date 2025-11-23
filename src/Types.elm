module Types exposing (..)

import Browser
import Dict exposing (Dict)
import Lamdera


-- ===========================================
-- PLAYER & ROOM
-- ===========================================


type PlayerRole
    = Player1
    | Player2


type alias RoomCode =
    String


type alias Room =
    { code : RoomCode
    , player1 : Maybe Lamdera.ClientId
    , player2 : Maybe Lamdera.ClientId
    , gameState : GameState
    , currentPuzzle : Int
    , puzzleStates : PuzzleStates
    , seed : Int
    }


type GameState
    = WaitingForPlayers
    | Playing
    | Victory
    | GameOver


-- ===========================================
-- PUZZLE STATES
-- ===========================================


type alias PuzzleStates =
    { colorCode : ColorCodeState
    , maze : MazeState
    , wires : WiresState
    , cipher : CipherState
    , switches : SwitchesState
    }


-- Puzzle 1: Color Code
-- Player1 sees colored symbols, Player2 sees symbol->number legend
type alias ColorCodeState =
    { symbols : List ColoredSymbol
    , legend : List ( Symbol, Int )
    , enteredCode : String
    , correctCode : String
    , solved : Bool
    , attempts : Int
    }


type alias ColoredSymbol =
    { symbol : Symbol
    , color : Color
    }


type Symbol
    = Star
    | Moon
    | Sun
    | Heart
    | Diamond
    | Circle


type Color
    = Red
    | Blue
    | Green
    | Yellow
    | Purple
    | Orange


-- Puzzle 2: Maze
-- Player1 sees position & can move, Player2 sees trap locations
type alias MazeState =
    { playerPos : ( Int, Int )
    , exitPos : ( Int, Int )
    , walls : List ( Int, Int )
    , traps : List ( Int, Int )
    , solved : Bool
    , hitTrap : Bool
    }


-- Puzzle 3: Wires
-- Player1 sees wires to cut, Player2 sees cutting rules
type alias WiresState =
    { wires : List Wire
    , cutOrder : List WireColor
    , correctOrder : List WireColor
    , solved : Bool
    , exploded : Bool
    }


type alias Wire =
    { color : WireColor
    , hasStripe : Bool
    , position : Int
    }


type WireColor
    = WireRed
    | WireBlue
    | WireYellow
    | WireGreen
    | WireWhite


-- Puzzle 4: Cipher
-- Player1 sees encrypted message, Player2 sees decryption key
type alias CipherState =
    { encryptedMessage : String
    , decryptionKey : Dict Char Char
    , enteredAnswer : String
    , correctAnswer : String
    , solved : Bool
    , hint : String
    }


-- Puzzle 5: Switches
-- Player1 has switches (can't see lights), Player2 sees lights (can't touch switches)
type alias SwitchesState =
    { switches : List Bool -- True = On
    , targetPattern : List Bool
    , switchEffects : List (List Int) -- Each switch affects these light indices
    , solved : Bool
    }


-- ===========================================
-- FRONTEND MODEL
-- ===========================================


type alias FrontendModel =
    { key : Lamdera.Key
    , page : Page
    , roomCode : String
    , roomCodeInput : String
    , playerRole : Maybe PlayerRole
    , gameState : Maybe GameState
    , currentPuzzle : Int
    , puzzleStates : Maybe PuzzleStates
    , error : Maybe String
    , codeInput : String -- For color code puzzle
    , cipherInput : String -- For cipher puzzle
    }


type Page
    = HomePage
    | WaitingRoom
    | GameRoom
    | VictoryPage


-- ===========================================
-- BACKEND MODEL
-- ===========================================


type alias BackendModel =
    { rooms : Dict RoomCode Room
    , nextSeed : Int
    }


-- ===========================================
-- MESSAGES
-- ===========================================


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Lamdera.Url
    | NoOpFrontendMsg
    -- Home page
    | CreateRoomClicked
    | JoinRoomClicked
    | RoomCodeInputChanged String
    -- Game actions
    | CodeInputChanged String
    | SubmitCode
    | MovePlayer Direction
    | CutWire WireColor
    | CipherInputChanged String
    | SubmitCipher
    | ToggleSwitch Int
    | NextPuzzle
    | ResetPuzzle
    | LeaveRoom


type Direction
    = Up
    | Down
    | Left
    | Right


type BackendMsg
    = NoOpBackendMsg
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToBackend
    = CreateRoom
    | JoinRoom RoomCode
    | PlayerAction PlayerAction
    | RequestNextPuzzle
    | RequestResetPuzzle
    | LeaveRoomRequest


type PlayerAction
    = SubmitColorCode String
    | Move Direction
    | Cut WireColor
    | SubmitCipherAnswer String
    | FlipSwitch Int


type ToFrontend
    = RoomCreated RoomCode PlayerRole
    | RoomJoined RoomCode PlayerRole
    | RoomError String
    | GameStateUpdate GameState Int PuzzleStates
    | PlayerLeft
    | PuzzleSolved Int
    | PuzzleFailed String
