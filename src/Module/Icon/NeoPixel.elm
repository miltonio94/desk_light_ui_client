module Module.Icon.NeoPixel exposing (neoPixel)

import Html exposing (Html)
import Svg
import Svg.Attributes
    exposing
        ( cx
        , cy
        , fill
        , height
        , id
        , rx
        , ry
        , stroke
        , strokeWidth
        , transform
        , viewBox
        , width
        , x
        , xlinkHref
        , y
        )


neoPixel : String -> Html msg
neoPixel colour =
    let
        solderingPadHeight =
            1.92

        solderingPadWidth =
            solderingPadHeight / 2

        solderingpadGap =
            solderingPadHeight + 0.25
    in
    Svg.svg
        [ width "75.0"
        , height "100.0"
        , viewBox "0 0 7.5 10.0"
        , id "NeoPixel-svg"
        ]
        [ Svg.g
            [ id "NeoPixel--g" ]
            [ Svg.rect
                [ stroke "#000"
                , width "7.5"
                , height "10.0"
                , x "0"
                , y "0"
                , strokeWidth ".1"
                , id "NeoPixel--rect-body"
                ]
                []
            , Svg.rect
                [ id "NeoPixel--rect-inner-body"
                , width "4.0"
                , height "4.0"
                , x "1.75"
                , y "3.0"
                , stroke "#000"
                , strokeWidth ".1"
                ]
                []
            , Svg.ellipse
                [ id "NeoPixel--colour"
                , stroke "#000"
                , cx "3.75"
                , cy "5.0"
                , rx "1.8"
                , ry "1.8"
                , strokeWidth ".1"
                , fill colour
                ]
                []
            , Svg.g
                [ id "NeoPixel--solder-pads" ]
                ([ 1.0, 3.17, 5.34, 7.51 ]
                    |> List.map
                        (\i ->
                            Svg.rect
                                [ id "rect1063"
                                , solderingPadWidth
                                    |> String.fromFloat
                                    |> width
                                , solderingPadHeight
                                    |> String.fromFloat
                                    |> height
                                , x "0.0"
                                , y (String.fromFloat i)
                                , fill "#b99075"
                                ]
                                []
                        )
                )
            , Svg.use
                [ xlinkHref "#NeoPixel--solder-pads"
                , transform "scale(-1, 1) translate(-7.5, 0)"
                ]
                []
            ]
        ]
