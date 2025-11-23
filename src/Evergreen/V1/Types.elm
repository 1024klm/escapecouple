module Evergreen.V1.Types exposing (..)

import Browser
import Dict
import Lamdera


type Page
    = HomePage
    | WaitingRoom
    | GameRoom
    | VictoryPage


type PlayerRole
    = Player1
    | Player2


type GameState
    = WaitingForPlayers
    | Playing
    | Victory
    | GameOver


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


type alias ColoredSymbol =
    { symbol : Symbol
    , color : Color
    }


type alias ColorCodeState =
    { symbols : List ColoredSymbol
    , legend : List ( Symbol, Int )
    , enteredCode : String
    , correctCode : String
    , solved : Bool
    , attempts : Int
    }


type alias MazeState =
    { playerPos : ( Int, Int )
    , exitPos : ( Int, Int )
    , walls : List ( Int, Int )
    , traps : List ( Int, Int )
    , solved : Bool
    , hitTrap : Bool
    }


type WireColor
    = WireRed
    | WireBlue
    | WireYellow
    | WireGreen
    | WireWhite


type alias Wire =
    { color : WireColor
    , hasStripe : Bool
    , position : Int
    }


type alias WiresState =
    { wires : List Wire
    , cutOrder : List WireColor
    , correctOrder : List WireColor
    , solved : Bool
    , exploded : Bool
    }


type alias CipherState =
    { encryptedMessage : String
    , decryptionKey : Dict.Dict Char Char
    , enteredAnswer : String
    , correctAnswer : String
    , solved : Bool
    , hint : String
    }


type alias SwitchesState =
    { switches : List Bool
    , targetPattern : List Bool
    , switchEffects : List (List Int)
    , solved : Bool
    }


type alias PuzzleStates =
    { colorCode : ColorCodeState
    , maze : MazeState
    , wires : WiresState
    , cipher : CipherState
    , switches : SwitchesState
    }


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
    , codeInput : String
    , cipherInput : String
    }


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


type alias BackendModel =
    { rooms : Dict.Dict RoomCode Room
    , nextSeed : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Lamdera.Url
    | NoOpFrontendMsg
    | CreateRoomClicked
    | JoinRoomClicked
    | RoomCodeInputChanged String
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


type PlayerAction
    = SubmitColorCode String
    | Move Direction
    | Cut WireColor
    | SubmitCipherAnswer String
    | FlipSwitch Int


type ToBackend
    = CreateRoom
    | JoinRoom RoomCode
    | PlayerAction PlayerAction
    | RequestNextPuzzle
    | RequestResetPuzzle
    | LeaveRoomRequest


type BackendMsg
    = NoOpBackendMsg
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = RoomCreated RoomCode PlayerRole
    | RoomJoined RoomCode PlayerRole
    | RoomError String
    | GameStateUpdate GameState Int PuzzleStates
    | PlayerLeft
    | PuzzleSolved Int
    | PuzzleFailed String
