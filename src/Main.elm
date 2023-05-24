module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Module.ColourPicker as ColourPicker
import Platform.Cmd as Cmd
import Ports


type Model
    = Syncing SyncingOnConnect
    | Connected State


type alias State =
    { lightState : LightState
    , rgba : ColourPicker.RGBa
    }


type SyncingOnConnect
    = NotSynced
    | Syncing_R String
    | Syncing_RG String String
    | Syncing_RGB String String String
    | Syncing_RGBA ColourPicker.RGBa
    | Synced ColourPicker.RGBa LightState


type LightState
    = LightOn
    | LightOff


stringToLightState : String -> LightState
stringToLightState str =
    case str of
        "STATE_ON" ->
            LightOn

        "STATE_OFF" ->
            LightOff

        _ ->
            LightOff


lightStateToWebSocketMsg : LightState -> String
lightStateToWebSocketMsg lightState =
    case lightState of
        LightOn ->
            "STATE_ON"

        LightOff ->
            "STATE_OFF"


lightStateToStr : LightState -> String
lightStateToStr lightState =
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
    | WebSocketGotMsg String
    | ColourChange ColourPicker.Colour String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Syncing NotSynced, Cmd.none )


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    case msg of
        WebSocketGotMsg string ->
            ( updateStateFromWebsocketMsg string model, Cmd.none )

        _ ->
            case model of
                Syncing _ ->
                    ( model, Cmd.none )

                Connected state ->
                    state
                        |> updateState msg
                        |> Tuple.mapFirst Connected


updateState : Msg -> State -> ( State, Cmd.Cmd Msg )
updateState msg state =
    case msg of
        LightSwitched lightState ->
            ( { state | lightState = lightState }
            , Ports.outgoingWebsocketMsg
                (lightStateToWebSocketMsg lightState)
            )

        -- WebSocketGotMsg string ->
        --     ( updateStateFromWebsocketMsg string state, Cmd.none )
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


updateFromSyncingStatus : String -> Maybe String -> SyncingOnConnect -> SyncingOnConnect
updateFromSyncingStatus cmd value syncingStatus =
    case ( syncingStatus, value ) of
        ( NotSynced, Just val ) ->
            if cmd == "R" then
                Syncing_R val

            else
                syncingStatus

        ( Syncing_R r, Just val ) ->
            if cmd == "G" then
                Syncing_RG r val

            else
                syncingStatus

        ( Syncing_RG r g, Just val ) ->
            if cmd == "B" then
                Syncing_RGB r g val

            else
                syncingStatus

        ( Syncing_RGB r g b, Just val ) ->
            if cmd == "A" then
                Syncing_RGBA (ColourPicker.RGBa r g b val)

            else
                syncingStatus

        ( Syncing_RGBA rgba, Just val ) ->
            if cmd == "STATE" then
                Synced rgba (stringToLightState (cmd ++ "_" ++ val))

            else
                syncingStatus

        _ ->
            syncingStatus


updateStateFromWebsocketMsg : String -> Model -> Model
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
            syncingStatus
                |> updateFromSyncingStatus command maybeValue
                |> (\ss ->
                        case ss of
                            Synced rgba state ->
                                Connected (State state rgba)

                            _ ->
                                Syncing ss
                   )

        Connected state ->
            Connected <|
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
                            { state | lightState = LightOn }

                        else
                            { state | lightState = LightOff }

                    _ ->
                        state


desk_light_switch : LightState -> Html Msg
desk_light_switch state =
    Html.div []
        [ Html.label
            [ Attributes.class "desk_light_switch" ]
            [ Html.text "state of light"
            , Html.button
                [ Events.onClick (LightSwitched (toggleLightSwitch state)) ]
                [ Html.text (lightStateToStr state) ]
            ]
        ]


body : Model -> Html Msg
body model =
    case model of
        Connected state ->
            Html.div []
                [ desk_light_switch state.lightState
                , ColourPicker.colourPicker ColourChange state.rgba
                ]

        Syncing _ ->
            Html.text "Connecting"


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
