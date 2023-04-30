port module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Platform.Cmd as Cmd


type alias Model =
    { lightState : Bool
    , websocketMsgs : List WebSocketMsg
    , messageBoxValue : String
    }


type WebSocketMsg
    = IncomingMsg
    | OutgoingMsg


type alias Message =
    { contents : String
    , messageType : WebSocketMsg
    }


type Msg
    = LightSwitched Bool
    | WebSocketSendMsg
    | MessageBoxChanged String
    | WebSocketGotMsg String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model False [] "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    case msg of
        LightSwitched state ->
            update WebSocketSendMsg { model | lightState = state }

        WebSocketSendMsg ->
            ( model, outgoingWebsocketMsg "hello" )

        MessageBoxChanged _ ->
            ( model, Cmd.none )

        WebSocketGotMsg _ ->
            ( model, Cmd.none )


desk_light_switch : Model -> Html Msg
desk_light_switch model =
    Html.label
        [ Attributes.class "desk_light_switch" ]
        [ Html.text "state of light"
        , Html.input
            [ Attributes.type_ "checkbox"
            , Attributes.checked model.lightState
            , Events.onCheck LightSwitched
            ]
            []
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Desk light"
    , body = [ desk_light_switch model ]
    }


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    incomingWebSocketMsg WebSocketGotMsg


port incomingWebSocketMsg : (String -> msg) -> Sub msg


port outgoingWebsocketMsg : String -> Cmd msg
