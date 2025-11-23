module Evergreen.V1.Types exposing (..)

import Browser
import Lamdera


type alias FrontendModel =
    { key : Lamdera.Key
    , message : String
    }


type alias BackendModel =
    { counter : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Lamdera.Url
    | SendMessage
    | NoOpFrontendMsg


type BackendMsg
    = NoOpBackendMsg


type ToBackend
    = UserClickedButton
    | NoOpToBackend


type ToFrontend
    = MessageFromBackend String
    | NoOpToFrontend