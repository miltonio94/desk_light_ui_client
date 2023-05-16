port module Main exposing (..) 

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Platform.Cmd as Cmd


type alias Model =
    { lightState : LightState
    , rgb : RGB
    }


type alias RGB =
    { r : Maybe Int
    , g : Maybe Int
    , b : Maybe Int
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
    | WebSocketGotMsg String
    | ColourChage Colour String


type Colour
    = Red
    | Blue
    | Green


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model LightOff (RGB Nothing Nothing Nothing), Cmd.none )


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

        ColourChage colourType colour ->
            ( updateColour colourType colour model, Cmd.none )


updateColour : Colour -> String -> Model -> Model
updateColour colourType colour model =
    case colourType of
        Red ->
            { model
                | rgb =
                    updateRgb
                        (\rgb ->
                            { rgb
                                | r = String.toInt colour
                            }
                        )
                        model.rgb
            }

        Green ->
            { model
                | rgb =
                    updateRgb
                        (\rgb ->
                            { rgb
                                | g = String.toInt colour
                            }
                        )
                        model.rgb
            }

        Blue ->
            { model
                | rgb =
                    updateRgb
                        (\rgb ->
                            { rgb
                                | b = String.toInt colour
                            }
                        )
                        model.rgb
            }


updateRgb : (RGB -> RGB) -> RGB -> RGB
updateRgb updater rgb =
    updater rgb


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


colourSlider : Colour -> Html Msg
colourSlider colourType =
    Html.div
        []
        [ Html.input
            [ Attributes.type_ "range"
            , Attributes.min "0"
            , Attributes.max "255"
            , Events.onInput (ColourChage colourType)
            ]
            [ Html.text "" ]
        ]


colourPicker : RGB -> Html Msg
colourPicker rgb =
    Html.div
        []
        [ colourSlider Red
        , colourSlider Green
        , colourSlider Blue
        ]


body : Model -> Html Msg
body model =
    Html.div []
        [ desk_light_switch model.lightState
        , colourPicker model.rgb
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Desk light"
    , body = [ body model ]
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
