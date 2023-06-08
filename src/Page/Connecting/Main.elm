module Page.Connecting.Main exposing
    ( Connecting(..)
    , Msg(..)
    , State
    , SyncingVal
    , getConnectedVal
    , init
    , isConnected
    , update
    , updateSyncing
    , view
    )

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Module.ColourPicker as ColourPicker exposing (RGBa)
import Module.Icon.NeoPixel exposing (neoPixel)


type alias State =
    { animatedPixels : List RGBa
    , syncingState : Connecting
    }


init : State
init =
    { animatedPixels =
        [ ColourPicker.init
        , ColourPicker.init
        , ColourPicker.init
        , ColourPicker.init
        , ColourPicker.init
        ]
    , syncingState = NotConnected
    }


type alias SyncingVal =
    ( String, Maybe String )


type Msg
    = AnimateNextFrame
    | Sync Connecting SyncingVal


type Connecting
    = NotConnected
    | Syncing_R String
    | Syncing_RG String String
    | Syncing_RGB String String String
    | Syncing_RGBA ColourPicker.RGBa
    | Connected ColourPicker.RGBa Bool


getConnectedVal : Connecting -> Maybe ( ColourPicker.RGBa, Bool )
getConnectedVal connecting =
    case connecting of
        Connected rgba state ->
            Just ( rgba, state )

        _ ->
            Nothing


isConnected : Connecting -> Bool
isConnected connecting =
    case connecting of
        Connected _ _ ->
            True

        _ ->
            False


update : Msg -> State -> State
update msg state =
    case msg of
        AnimateNextFrame ->
            state

        Sync syncing ( cmd, val ) ->
            { state | syncingState = updateSyncing cmd val syncing }


updateSyncing : String -> Maybe String -> Connecting -> Connecting
updateSyncing cmd value state =
    case ( state, value, cmd ) of
        ( NotConnected, Just val, "R" ) ->
            Syncing_R val

        ( Syncing_R r, Just val, "G" ) ->
            Syncing_RG r val

        ( Syncing_RG r g, Just val, "B" ) ->
            Syncing_RGB r g val

        ( Syncing_RGB r g b, Just val, "A" ) ->
            Syncing_RGBA (ColourPicker.RGBa r g b val)

        ( Syncing_RGBA rgba, Just val, "STATE" ) ->
            Connected rgba (stringToLightState (cmd ++ "_" ++ val))

        _ ->
            state


stringToLightState : String -> Bool
stringToLightState str =
    case str of
        "STATE_ON" ->
            True

        "STATE_OFF" ->
            False

        _ ->
            False


view : Html ms
view =
    div
        [ class "pixels" ]
        [ neoPixel "red"
        , neoPixel "red"
        , neoPixel "red"
        , neoPixel "red"
        , neoPixel "red"
        ]
