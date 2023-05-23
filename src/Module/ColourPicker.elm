module Module.ColourPicker exposing (Colour(..), RGBa, colourPicker, colourToString, initRgb, updateColour)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Svg
import Svg.Attributes as SvgAtt


type alias RGBa =
    { r : String
    , g : String
    , b : String
    , a : String
    }


type Colour
    = Red
    | Blue
    | Green
    | Alpha


colourToString : String -> Colour -> String
colourToString colourVal colour =
    case colour of
        Red ->
            "R_" ++ colourVal

        Green ->
            "G_" ++ colourVal

        Blue ->
            "B_" ++ colourVal

        Alpha ->
            "A_" ++ colourVal


updateRgb : (RGBa -> RGBa) -> RGBa -> RGBa
updateRgb updater rgb =
    updater rgb


colourSlider : (Colour -> String -> msg) -> Colour -> RGBa -> Html msg
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


colourPicker : (Colour -> String -> msg) -> RGBa -> Html msg
colourPicker msg rgb =
    Html.div
        []
        [ colourShower rgb
        , colourSlider msg Red rgb
        , colourSlider msg Green rgb
        , colourSlider msg Blue rgb
        , colourSlider msg Alpha rgb
        ]


getValueFromColourType : Colour -> RGBa -> String
getValueFromColourType colour rgb =
    case colour of
        Red ->
            rgb.r

        Green ->
            rgb.g

        Blue ->
            rgb.b

        Alpha ->
            rgb.a


colourShower : RGBa -> Html msg
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


rgbToHtmlRgb : RGBa -> String
rgbToHtmlRgb rgb =
    "rgb(%r, %g, %b)"
        |> String.replace "%r" rgb.r
        |> String.replace "%g" rgb.g
        |> String.replace "%b" rgb.b


initRgb : RGBa
initRgb =
    { r = "0", g = "0", b = "0", a = "0" }


updateColour : Colour -> String -> { rgb | rgb : RGBa } -> { rgb | rgb : RGBa }
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

        Alpha ->
            { model
                | rgb =
                    updateRgb
                        (\rgb ->
                            { rgb
                                | a = colour
                            }
                        )
                        model.rgb
            }
