module Buttons exposing (..)

import Html exposing (Html)
import Html.Attributes
import Sizes exposing (..)
import Svg exposing (Svg, animate, g, text)
import Svg.Attributes exposing (attributeName, dur, fill, repeatCount, transform, values)
import Svg.Events exposing (onClick)
import Tak1Bai2Types exposing (..)


eyeButton : { eyeIsOpen : Bool } -> Html Msg
eyeButton a =
    if a.eyeIsOpen then
        Html.input [ Html.Attributes.type_ "image", onClick CloseTheEye, Html.Attributes.src "../img/eye.svg", Html.Attributes.height 50 ] []

    else
        Html.input [ Html.Attributes.type_ "image", onClick OpenTheEye, Html.Attributes.src "../img/sleeping_eye.svg", Html.Attributes.height 50 ] []


simpleCancelButton : { eyeIsOpen : Bool } -> Html Msg
simpleCancelButton a =
    Html.button
        [ Html.Attributes.type_ "button"
        , Html.Attributes.style "cursor"
            (if a.eyeIsOpen then
                "not-allowed"

             else
                "pointer"
            )
        , onClick Cancel
        , Html.Attributes.style "background-color" "#ffaaaa"
        , Html.Attributes.style "font-size" "100%"
        ]
        [ text "キャンセル" ]


matchButton : { eyeIsOpen : Bool } -> Html Msg
matchButton a =
    Html.button
        [ Html.Attributes.type_ "button"
        , Html.Attributes.style "cursor"
            (if a.eyeIsOpen then
                "not-allowed"

             else
                "pointer"
            )
        , onClick Match
        , Html.Attributes.style "background-color" "#aaffaa"
        , Html.Attributes.style "font-size" "150%"
        ]
        [ text "マッチ！" ]


mismatchButton : { eyeIsOpen : Bool } -> Html Msg
mismatchButton a =
    Html.button
        [ Html.Attributes.type_ "button"
        , Html.Attributes.style "cursor"
            (if a.eyeIsOpen then
                "not-allowed"

             else
                "pointer"
            )
        , onClick Mismatch
        , Html.Attributes.style "background-color" "#aaaaff"
        , Html.Attributes.style "font-size" "150%"
        ]
        [ text "ミスマッチ……" ]


candidateYellowSvg : { eyeIsOpen : Bool } -> msg -> Coordinate -> Svg msg
candidateYellowSvg a msgToBeSent coord =
    g
        [ transform ("translate(" ++ String.fromFloat (toFloat coord.x * lattice_size) ++ " " ++ String.fromFloat (toFloat coord.y * lattice_size) ++ ")")
        , Svg.Events.onClick msgToBeSent
        , Html.Attributes.style "cursor"
            (if a.eyeIsOpen then
                "not-allowed"

             else
                "pointer"
            )
        ]
        [ Svg.circle [ Svg.Attributes.cx "0", Svg.Attributes.cy "0", Svg.Attributes.r "25", fill (fromUIColor Yellow) ]
            [ animate [ attributeName "opacity", dur "1.62s", values "0;1;0", repeatCount "indefinite" ] []
            ]
        ]


candidateGreenSvg : { eyeIsOpen : Bool } -> msg -> Coordinate -> Svg msg
candidateGreenSvg a msgToBeSent coord =
    g
        [ transform ("translate(" ++ String.fromFloat (toFloat coord.x * lattice_size) ++ " " ++ String.fromFloat (toFloat coord.y * lattice_size) ++ ")")
        , Svg.Events.onClick msgToBeSent
        , Html.Attributes.style "cursor"
            (if a.eyeIsOpen then
                "not-allowed"

             else
                "pointer"
            )
        ]
        [ Svg.rect [ Svg.Attributes.transform "rotate(45)", Svg.Attributes.x "-23", Svg.Attributes.y "-23", Svg.Attributes.width "46", Svg.Attributes.height "46", fill (fromUIColor Green) ]
            [ animate [ attributeName "opacity", dur "1.62s", values "0;1;0", repeatCount "indefinite" ] []
            ]
        ]
