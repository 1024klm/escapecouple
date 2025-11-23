module Frontend exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Lamdera
import Types exposing (..)


app : { init : Lamdera.Url -> Lamdera.Key -> ( FrontendModel, Cmd FrontendMsg )
      , view : FrontendModel -> Html FrontendMsg
      , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
      , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
      , subscriptions : FrontendModel -> Sub FrontendMsg
      , onUrlRequest : Browser.UrlRequest -> FrontendMsg
      , onUrlChange : Lamdera.Url -> FrontendMsg
      }
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
init url key =
    ( { key = key
      , message = "🚀 escapecouple - Ready to ship!"
      }
    , Cmd.none
    )


view : FrontendModel -> Html FrontendMsg
view model =
    div [ class "container" ]
        [ h1 [] [ text "escapecouple" ]
        , p [] [ text model.message ]
        , button [ onClick SendMessage ] [ text "Send to Backend" ]
        ]


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        SendMessage ->
            ( model, Lamdera.sendToBackend UserClickedButton )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        MessageFromBackend newMessage ->
            ( { model | message = newMessage }, Cmd.none )

        NoOpToFrontend ->
            ( model, Cmd.none )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.none