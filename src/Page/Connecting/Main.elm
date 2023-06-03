module Page.Connecting.Main exposing (Model(..), update, view)

import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (class)
import Module.ColourPicker as ColourPicker


type Model
    = NotSynced
    | Syncing_R String
    | Syncing_RG String String
    | Syncing_RGB String String String
    | Syncing_RGBA ColourPicker.RGBa
    | Synced ColourPicker.RGBa Bool


update : String -> Maybe String -> Model -> Model
update cmd value syncingStatus =
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
        [ div
            [ class "pixel" ]
            []
        , div
            [ class "pixel" ]
            []
        , div
            [ class "pixel" ]
            []
        ]
