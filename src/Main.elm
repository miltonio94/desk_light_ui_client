module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Module.ColourPicker as ColourPicker
import Platform.Cmd as Cmd
import Ports
import Queue exposing (Queue)


type alias Model =
    { lightState : LightState
    , rgb : ColourPicker.RGB
    , webSocketQueue : Queue String
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
    | AddToQueue String
    | WebSocketGotMsg String
    | ColourChange ColourPicker.Colour String


initModel : Model
initModel =
    { lightState = LightOff
    , rgb = ColourPicker.initRgb
    , webSocketQueue = Queue.empty
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    case msg of
        LightSwitched state ->
            ( { model | lightState = state }
            , Ports.outgoingWebsocketMsg
                (lightStateToWebSocketMsg state)
            )

        WebSocketSendMsg ->
            ( model, Ports.outgoingWebsocketMsg (lightStateToWebSocketMsg model.lightState) )

        WebSocketGotMsg string ->
            ( updateModelFromWebsocketMsg string model, Cmd.none )

        ColourChange colourType colour ->
            ( ColourPicker.updateColour
                colourType
                colour
                model
            , Ports.outgoingWebsocketMsg
                (ColourPicker.colourToString colour colourType)
            )

        AddToQueue value ->
            ( { model
                | webSocketQueue =
                    Queue.enqueue value model.webSocketQueue
              }
            , Cmd.none
            )


updateModelFromWebsocketMsg : String -> Model -> Model
updateModelFromWebsocketMsg msg model =
    let
        msgSplit =
            String.split "_" msg

        command =
            List.head msgSplit
                |> Maybe.withDefault ""

        maybeValue =
            List.tail msgSplit
                |> Maybe.map
                    (\l ->
                        List.head l
                            |> Maybe.withDefault "0"
                    )
    in
    case ( command, maybeValue ) of
        ( "R", Just value ) ->
            ColourPicker.updateColour ColourPicker.Red value model

        ( "G", Just value ) ->
            ColourPicker.updateColour ColourPicker.Green value model

        ( "B", Just value ) ->
            ColourPicker.updateColour ColourPicker.Blue value model

        ( "STATE", Just value ) ->
            if value == "ON" then
                { model | lightState = LightOn }

            else
                { model | lightState = LightOff }

        ( "CONNECTED", Nothing ) ->
            model

        _ ->
            model


desk_light_switch : LightState -> Html Msg
desk_light_switch state =
    Html.div []
        [ Html.label
            [ Attributes.class "desk_light_switch" ]
            [ Html.text "state of light"
            , Html.button
                [ Events.onClick (LightSwitched (toggleLightSwitch state)) ]
                [ Html.text (lightStateToWebSocketMsg state) ]
            ]
        ]


body : Model -> Html Msg
body model =
    Html.div []
        [ desk_light_switch model.lightState
        , ColourPicker.colourPicker ColourChange model.rgb
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Desk light"
    , body = [ body model ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.incomingWebSocketMsg WebSocketGotMsg ]
