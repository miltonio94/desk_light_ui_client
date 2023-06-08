module Module.ColourPicker exposing
    ( Colour(..)
    , RGBa
    , colourPicker
    , colourToString
    , init
    , initRgba
    , setA
    , setB
    , setG
    , setR
    , updateColour
    )

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


init : RGBa
init =
    RGBa "0" "0" "0" "0"


setR : RGBa -> String -> RGBa
setR rgba r =
    { rgba | r = r }


setG : RGBa -> String -> RGBa
setG rgba g =
    { rgba | g = g }


setB : RGBa -> String -> RGBa
setB rgba b =
    { rgba | b = b }


setA : RGBa -> String -> RGBa
setA rgba a =
    { rgba | a = a }


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


updateRgba : (RGBa -> RGBa) -> RGBa -> RGBa
updateRgba updater rgba =
    updater rgba


colourSlider : (Colour -> String -> msg) -> Colour -> RGBa -> Html msg
colourSlider messenger colourType rgba =
    Html.div
        []
        [ Html.input
            [ Attributes.type_ "range"
            , Attributes.min "0"
            , Attributes.max "255"
            , Attributes.value (getValueFromColourType colourType rgba)
            , Events.onInput (messenger colourType)
            ]
            [ Html.text "" ]
        ]


colourPicker : (Colour -> String -> msg) -> RGBa -> Html msg
colourPicker msg rgba =
    Html.div
        []
        [ colourShower rgba
        , colourSlider msg Red rgba
        , colourSlider msg Green rgba
        , colourSlider msg Blue rgba
        , colourSlider msg Alpha rgba
        ]


getValueFromColourType : Colour -> RGBa -> String
getValueFromColourType colour rgba =
    case colour of
        Red ->
            rgba.r

        Green ->
            rgba.g

        Blue ->
            rgba.b

        Alpha ->
            rgba.a


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
        |> String.replace "%r" rgba.r
        |> String.replace "%g" rgba.g
        |> String.replace "%b" rgba.b


initRgba : RGBa
initRgba =
    { r = "0", g = "0", b = "0", a = "0" }


updateColour : Colour -> String -> { rgba | rgba : RGBa } -> { rgba | rgba : RGBa }
updateColour colourType colour state =
    case colourType of
        Red ->
            { state
                | rgba =
                    setR state.rgba colour
            }

        Green ->
            { state
                | rgba =
                    setG state.rgba colour
            }

        Blue ->
            { state
                | rgba =
                    setB state.rgba colour
            }

        Alpha ->
            { state
                | rgba =
                    setA state.rgba colour
            }
