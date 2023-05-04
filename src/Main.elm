port module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Platform.Cmd as Cmd


type alias Model =
    { lightState : LightState
    }


type WebSocketMsg
    = IncomingMsg
    | OutgoingMsg


type alias Message =
    { contents : String
    , messageType : WebSocketMsg
    }


type LightState
    = LightOn
    | LightOff


lightStateToWebSocketMsg : LightState -> String
lightStateToWebSocketMsg lightState =
    case lightState of
        LightOn ->
            "ON"

        LightOff ->
            "OFF"


lightStateToChecked : LightState -> Bool
lightStateToChecked state =
    case state of
        LightOn ->
            True

        LightOff ->
            False


checkedToLightState : Bool -> LightState
checkedToLightState isChecked =
    case isChecked of
        True ->
            LightOn

        False ->
            LightOff


toggleLightSwitch : LightState -> LightState
toggleLightSwitch lightState =
    case lightState of
        LightOn ->
            LightOff

        LightOff ->
            LightOn


type Msg
    = LightSwitched LightState
    | WebSocketSendMsg
    | WebSocketGotMsg String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model LightOff, Cmd.none )


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    case msg of
        LightSwitched state ->
            { model | lightState = state }
                |> update WebSocketSendMsg

        WebSocketSendMsg ->
            ( model, outgoingWebsocketMsg (lightStateToWebSocketMsg model.lightState) )

        WebSocketGotMsg _ ->
            ( model, Cmd.none )


desk_light_switch : Model -> Html Msg
desk_light_switch model =
    Html.label
        [ Attributes.class "desk_light_switch" ]
        [ Html.text "state of light"
        , Html.button
            [ Events.onClick (LightSwitched (toggleLightSwitch model.lightState)) ]
            [ Html.text (lightStateToWebSocketMsg model.lightState) ]
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
