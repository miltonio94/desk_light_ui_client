module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Module.ColourPicker as ColourPicker
import Page.Connecting as Connecting
import Platform.Cmd as Cmd
import Ports


type Model
    = Syncing SyncingOnConnect
    | Connected State


type alias State =
    { lightIsOn : Bool
    , rgba : ColourPicker.RGBa
    }


type SyncingOnConnect
    = NotSynced
    | Syncing_R String
    | Syncing_RG String String
    | Syncing_RGB String String String
    | Syncing_RGBA ColourPicker.RGBa
    | Synced ColourPicker.RGBa Bool


stringToLightState : String -> Bool
stringToLightState str =
    case str of
        "STATE_ON" ->
            True

        "STATE_OFF" ->
            False

        _ ->
            False


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


updateFromSyncingStatus : String -> Maybe String -> SyncingOnConnect -> SyncingOnConnect
updateFromSyncingStatus cmd value syncingStatus =
    case ( syncingStatus, value, cmd ) of
        ( NotSynced, Just val, "R" ) ->
            Syncing_R val

        ( Syncing_R r, Just val, "G" ) ->
            Syncing_RG r val

        ( Syncing_RG r g, Just val, "B" ) ->
            Syncing_RGB r g val

        ( Syncing_RGB r g b, Just val, "A" ) ->
            Syncing_RGBA (ColourPicker.RGBa r g b val)

        ( Syncing_RGBA rgba, Just val, "STATE" ) ->
            Synced rgba (stringToLightState (cmd ++ "_" ++ val))

        _ ->
            syncingStatus


syncingOnConnectToModel : SyncingOnConnect -> Model
syncingOnConnectToModel syncingState =
    case syncingState of
        Synced rgba state ->
            Connected (State state rgba)

        _ ->
            Syncing syncingState


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
                |> syncingOnConnectToModel

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
                            { state | lightIsOn = True }

                        else
                            { state | lightIsOn = False }

                    _ ->
                        state


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
