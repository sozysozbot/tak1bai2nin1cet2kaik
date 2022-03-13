module SvgElements exposing (..)

import Html.Attributes
import SvgColor exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Tak1Bai2Types exposing (..)


glyph : Profession -> String -> List (Svg msg)
glyph profession color =
    let
        style =
            [ fill "transparent", stroke color, strokeWidth "6", strokeLinecap "round" ]
    in
    case profession of
        Mun1 ->
            [ Svg.path (d "M 21 52 h 62" :: style) []
            , Svg.path (d "M 52 21 v 62" :: style) []
            ]

        Nuak1 ->
            [ Svg.path (d "M 24 24 l  56 56" :: style) []
            , Svg.path (d "M 80 24 l -56 56" :: style) []
            ]

        _ ->
            [ circle ([ cx "52", cy "52", r "27" ] ++ style) []
            ]


goalCandidateRedSvg : msg -> Coordinate -> Svg msg
goalCandidateRedSvg msgToBeSent coord =
    g
        [ transform ("translate(" ++ String.fromInt (coord.x * 100) ++ " " ++ String.fromInt (coord.y * 100) ++ ")")
        , Svg.Events.onClick msgToBeSent
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ rect [ x "36", y "36", width "32", height "32", fill redCandidateColor ] [] ]


spacing : Int -> Float
spacing n =
    {- 0.846 * 5 + 0.80 == 5.03 -}
    {- Adding to this two halves of width 1 borders gives 504px. -}
    {- This exactly matches (100 pixel * 5) + two halves of width 4 borders -}
    if n <= 6 then
        0.846

    else
        {-
           0.846 * (6-1) + 0.8 == x * (n-1) + 0.8
           x = 0.846 * 5 / (n-1)
        -}
        0.846 * 5.0 / toFloat (n - 1)


pieceSvg__ : (msg -> String) -> { a | color : String, width : String } -> msg -> CardOnBoard -> Svg msg
pieceSvg__ toIcon strok msgToBeSent p =
    g
        [ transform ("translate(" ++ String.fromInt (p.coord.x * 100) ++ " " ++ String.fromInt (p.coord.y * 100) ++ ")")
        , Html.Attributes.style "cursor"
            (toIcon msgToBeSent)
        , Svg.Events.onClick msgToBeSent
        ]
        (rect
            [ x "12"
            , y "12"
            , width "80"
            , height "80"
            , fill (backgroundColor p.cardColor)
            , stroke
                strok.color
            , strokeWidth
                strok.width
            ]
            []
            :: glyph p.prof (foregroundColor p.cardColor)
        )
