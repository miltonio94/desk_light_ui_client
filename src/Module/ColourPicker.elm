module Module.ColourPicker exposing (Colour(..), RGB, colourPicker, colourToString, initRgb, updateColour)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Svg
import Svg.Attributes as SvgAtt


type alias RGB =
    { r : String
    , g : String
    , b : String
    }


type Colour
    = Red
    | Blue
    | Green


colourToString : String -> Colour -> String
colourToString colourVal colour =
    case colour of
        Red ->
            "R_" ++ colourVal

        Green ->
            "G_" ++ colourVal

        Blue ->
            "B_" ++ colourVal


updateRgb : (RGB -> RGB) -> RGB -> RGB
updateRgb updater rgb =
    updater rgb


colourSlider : (Colour -> String -> msg) -> Colour -> RGB -> Html msg
colourSlider messenger colourType rgb =
    Html.div
        []
        [ Html.input
            [ Attributes.type_ "range"
            , Attributes.min "0"
            , Attributes.max "255"
            , Attributes.value (getValueFromColourType colourType rgb)
            , Events.onInput (messenger colourType)
            ]
            [ Html.text "" ]
        ]


colourPicker : (Colour -> String -> msg) -> RGB -> Html msg
colourPicker msg rgb =
    Html.div
        []
        [ colourShower rgb
        , colourSlider msg Red rgb
        , colourSlider msg Green rgb
        , colourSlider msg Blue rgb
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


colourShower : RGB -> Html msg
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


initRgb : RGB
initRgb =
    { r = "0", g = "0", b = "0" }


updateColour : Colour -> String -> { rgb | rgb : RGB } -> { rgb | rgb : RGB }
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
