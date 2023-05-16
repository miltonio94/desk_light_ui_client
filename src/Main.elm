port module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Platform.Cmd as Cmd
import Svg
import Svg.Attributes as SvgAtt


type alias Model =
    { lightState : LightState
    , rgb : RGB
    }


type alias RGB =
    { r : String
    , g : String
    , b : String
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
    ( Model LightOff (RGB "0" "0" "0"), Cmd.none )


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
                                | r = colour
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
                                | g = colour
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
                                | b = colour
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


colourSlider : Colour -> RGB -> Html Msg
colourSlider colourType rgb =
    Html.div
        []
        [ Html.input
            [ Attributes.type_ "range"
            , Attributes.min "0"
            , Attributes.max "255"
            , Attributes.value (getValueFromColourType colourType rgb)
            , Events.onInput (ColourChage colourType)
            ]
            [ Html.text "" ]
        ]


colourPicker : RGB -> Html Msg
colourPicker rgb =
    Html.div
        []
        [ colourShower rgb
        , colourSlider Red rgb
        , colourSlider Green rgb
        , colourSlider Blue rgb
        ]


getValueFromColourType : Colour -> RGB -> String
getValueFromColourType colour rgb =
    case colour of
        Red ->
            rgb.r

        Green ->
            rgb.g

        Blue ->
            rgb.b


colourShower : RGB -> Html Msg
colourShower rgb =
    Html.div
        []
        [ Svg.svg
            [ SvgAtt.viewBox "0 0 300 300"
            , SvgAtt.width "255"
            , SvgAtt.height "255"
            , SvgAtt.fill (rgbToHtmlRgb rgb)
            ]
            [ Svg.rect
                [ SvgAtt.width "300px"
                , SvgAtt.height "300px"
                ]
                []
            ]
        ]


rgbToHtmlRgb : RGB -> String
rgbToHtmlRgb rgb =
    "rgb(%r, %g, %b)"
        |> String.replace "%r" rgb.r
        |> String.replace "%g" rgb.g
        |> String.replace "%b" rgb.b


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
