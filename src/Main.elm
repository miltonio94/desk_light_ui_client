module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Module.ColourPicker as ColourPicker
import Page.Connecting.Main as Connecting
import Platform.Cmd as Cmd
import Ports
import Task


type Model
    = Syncing Connecting.State
    | Connected State


getConnectingState : Model -> Connecting.State
getConnectingState model =
    case model of
        Syncing state ->
            state

        _ ->
            Connecting.init


type alias State =
    { lightIsOn : Bool
    , rgba : ColourPicker.RGBa
    }


lightStateToWebSocketMsg : Bool -> String
lightStateToWebSocketMsg lightState =
    if lightState then
        "STATE_ON"

    else
        "STATE_OFF"


lightStateToStr : Bool -> String
lightStateToStr lightState =
    if lightState then
        "ON"

    else
        "OFF"


type Msg
    = LightSwitched Bool
    | WebSocketGotMsg String
    | ConnectingMsg Connecting.Msg
    | ColourChange ColourPicker.Colour String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Syncing Connecting.init, Cmd.none )


syncingToConnected : Model -> Model
syncingToConnected model =
    model


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    case msg of
        LightSwitched _ ->
            case model of
                Connected state ->
                    state
                        |> updateState msg
                        |> Tuple.mapFirst Connected

                Syncing _ ->
                    ( model, Cmd.none )

        WebSocketGotMsg str ->
            updateStateFromWebsocketMsg str model

        ConnectingMsg connectingMsg ->
            ( Syncing
                (Connecting.update
                    connectingMsg
                    (getConnectingState model)
                )
            , Cmd.none
            )
                |> (\( m, cmd ) ->
                        case m of
                            Syncing mc ->
                                mc.syncingState
                                    |> Connecting.getConnectedVal
                                    |> (\mt ->
                                            case mt of
                                                Just ( rgba, state ) ->
                                                    ( Connected (State state rgba), cmd )

                                                Nothing ->
                                                    ( m, cmd )
                                       )

                            _ ->
                                ( m, cmd )
                   )

        ColourChange _ _ ->
            case model of
                Connected state ->
                    state
                        |> updateState msg
                        |> Tuple.mapFirst Connected

                Syncing _ ->
                    ( model, Cmd.none )


updateState : Msg -> State -> ( State, Cmd.Cmd Msg )
updateState msg state =
    case msg of
        LightSwitched lightState ->
            ( { state | lightIsOn = lightState }
            , Ports.outgoingWebsocketMsg
                (lightStateToWebSocketMsg lightState)
            )

        ColourChange colourType colour ->
            ( ColourPicker.updateColour
                colourType
                colour
                state
            , Ports.outgoingWebsocketMsg
                (ColourPicker.colourToString colour colourType)
            )

        _ ->
            ( state, Cmd.none )


syncingOnConnectToModel : Connecting.State -> Model
syncingOnConnectToModel syncingState =
    case syncingState.syncingState of
        Connecting.Connected rgba state ->
            Connected (State state rgba)

        _ ->
            Syncing syncingState


websocketMsgToConnectingMsg : String -> Connecting.Connecting -> Cmd Msg
websocketMsgToConnectingMsg msg connecting =
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
    Cmd.none
        |> Cmd.map
            (always
                (ConnectingMsg
                    (Connecting.Sync
                        connecting
                        ( command, maybeValue )
                    )
                )
            )


updateStateFromWebsocketMsg : String -> Model -> ( Model, Cmd.Cmd Msg )
updateStateFromWebsocketMsg msg model =
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
    case model of
        Syncing syncingStatus ->
            ( model
            , Task.perform
                (always
                    (ConnectingMsg
                        (Connecting.Sync
                            syncingStatus.syncingState
                            ( command, maybeValue )
                        )
                    )
                )
                (Task.succeed ())
            )

        Connected state ->
            Tuple.pair
                (Connected <|
                    case ( command, maybeValue ) of
                        ( "R", Just value ) ->
                            ColourPicker.updateColour ColourPicker.Red value state

                        ( "G", Just value ) ->
                            ColourPicker.updateColour ColourPicker.Green value state

                        ( "B", Just value ) ->
                            ColourPicker.updateColour ColourPicker.Blue value state

                        ( "A", Just value ) ->
                            ColourPicker.updateColour ColourPicker.Alpha value state

                        ( "STATE", Just value ) ->
                            if value == "ON" then
                                { state | lightIsOn = True }

                            else
                                { state | lightIsOn = False }

                        _ ->
                            state
                )
                Cmd.none


desk_light_switch : Bool -> Html Msg
desk_light_switch state =
    Html.div []
        [ Html.label
            [ Attributes.class "desk_light_switch" ]
            [ Html.text "state of light"
            , Html.button
                [ Events.onClick (LightSwitched (not state)) ]
                [ Html.text (lightStateToStr state) ]
            ]
        ]


body : Model -> Html Msg
body model =
    case model of
        Connected state ->
            Html.div []
                [ desk_light_switch state.lightIsOn
                , ColourPicker.colourPicker ColourChange state.rgba
                ]

        Syncing _ ->
            Connecting.view


view : Model -> Browser.Document Msg
view model =
    { title = "PIXIE"
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
