module Module.ColourPicker exposing (Colour(..), RGBa, colourPicker, colourToString, initRgba, updateColour)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Svg
import Svg.Attributes as SvgAtt


type alias RGBa =
    { r : Colour
    , g : Colour
    , b : Colour
    , a : Colour
    }


type Colour
    = Red String
    | Blue String
    | Green String
    | Alpha String


colourToString : Colour -> String
colourToString colour =
    case colour of
        Red val ->
            "R_" ++ val

        Green val ->
            "G_" ++ val

        Blue val ->
            "B_" ++ val

        Alpha val ->
            "A_" ++ val


updateRgba : (RGBa -> RGBa) -> RGBa -> RGBa
updateRgba updater rgba =
    updater rgba


colourSlider : (Colour -> String -> msg) -> Colour -> Html msg
colourSlider messenger colourType =
    Html.div
        []
        [ Html.input
            [ Attributes.type_ "range"
            , Attributes.min "0"
            , Attributes.max "255"
            , Attributes.value (getValueFromColourType colourType)
            , Events.onInput (messenger colourType)
            ]
            [ Html.text "" ]
        ]


colourPicker : (Colour -> String -> msg) -> RGBa -> Html msg
colourPicker msg rgba =
    Html.div
        []
        [ colourShower rgba
        , colourSlider msg rgba.r
        , colourSlider msg rgba.g
        , colourSlider msg rgba.b
        , colourSlider msg rgba.a
        ]


getValueFromColourType : Colour -> String
getValueFromColourType colour =
    case colour of
        Red val ->
            val

        Green val ->
            val

        Blue val ->
            val

        Alpha val ->
            val


colourShower : RGBa -> Html msg
colourShower rgba =
    Html.div
        []
        [ Svg.svg
            [ SvgAtt.viewBox "0 0 300 300"
            , SvgAtt.width "255"
            , SvgAtt.height "255"
            , SvgAtt.fill (rgbaToHtmlRgb rgba)
            ]
            [ Svg.rect
                [ SvgAtt.width "300px"
                , SvgAtt.height "300px"
                ]
                []
            ]
        ]


rgbaToHtmlRgb : RGBa -> String
rgbaToHtmlRgb rgba =
    "rgb(%r, %g, %b)"
        |> String.replace "%r" (getValueFromColourType rgba.r)
        |> String.replace "%g" (getValueFromColourType rgba.g)
        |> String.replace "%b" (getValueFromColourType rgba.b)


initRgba : RGBa
initRgba =
    { r = Red "0", g = Green "0", b = Blue "0", a = Alpha "0" }


updateColour : Colour -> String -> { rgba | rgba : RGBa } -> { rgba | rgba : RGBa }
updateColour colourType val model =
    case colourType of
        Red _ ->
            { model
                | rgba =
                    updateRgba
                        (\rgba ->
                            { rgba
                                | r = Red val
                            }
                        )
                        model.rgba
            }

        Green _ ->
            { model
                | rgba =
                    updateRgba
                        (\rgba ->
                            { rgba
                                | g = Green val
                            }
                        )
                        model.rgba
            }

        Blue _ ->
            { model
                | rgba =
                    updateRgba
                        (\rgba ->
                            { rgba
                                | b = Blue val
                            }
                        )
                        model.rgba
            }

        Alpha _ ->
            { model
                | rgba =
                    updateRgba
                        (\rgba ->
                            { rgba
                                | a = Alpha val
                            }
                        )
                        model.rgba
            }
