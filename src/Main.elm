module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes


type alias Model =
    { lightState : Bool
    }


initialModel =
    Model True


type Msg
    = LightSwitched


update : Msg -> Model -> Model
update msg model =
    case msg of
        LightSwitched ->
            { model | lightState = not model.lightState }


desk_light_switch model =
    Html.label
        [ Attributes.class "desk_light_switch" ]
        [ Html.text "state of light"
        , Html.input
            [ Attributes.type_ "checkbox"
            , Attributes.checked model.lightState
            ]
            []
        ]


view : Model -> Html msg
view model =
    Html.div
        []
        [ desk_light_switch model
        ]


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
