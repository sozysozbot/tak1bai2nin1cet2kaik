module KeseRimaSvgElements exposing (..)
import KeseRimaTypes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import KeseRimaSvgColor exposing (..)
import Html.Attributes

glyph : Profession -> String -> List (Svg msg)
glyph profession color =
    let
        style =
            [ fill "transparent", stroke color, strokeWidth "6", strokeLinecap "round" ]
    in
    case profession of
        HorizontalVertical ->
            [ Svg.path (d "M 21 52 h 62" :: style) []
            , Svg.path (d "M 52 21 v 62" :: style) []
            ]

        Diagonal ->
            [ Svg.path (d "M 24 24 l  56 56" :: style) []
            , Svg.path (d "M 80 24 l -56 56" :: style) []
            ]

        Circle ->
            [ circle ([ cx "52", cy "52", r "27" ] ++ style) []
            ]

        All ->
            glyph HorizontalVertical color ++ glyph Diagonal color ++ glyph Circle color

goalCandidateYellowSvg : msg -> Coordinate -> Svg msg
goalCandidateYellowSvg msgToBeSent coord =
    g
        [ transform ("translate(" ++ String.fromInt (coord.x * 100) ++ " " ++ String.fromInt (coord.y * 100) ++ ")")
        , Svg.Events.onClick msgToBeSent
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ circle [ cx "52", cy "52", r "16", fill yellowCandidateColor ] [] ]


goalCandidateRedSvg : msg -> Coordinate -> Svg msg
goalCandidateRedSvg msgToBeSent coord =
    g
        [ transform ("translate(" ++ String.fromInt (coord.x * 100) ++ " " ++ String.fromInt (coord.y * 100) ++ ")")
        , Svg.Events.onClick msgToBeSent
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ rect [ x "36", y "36", width "32", height "32", fill redCandidateColor ] [] ]

pieceWaitingForAdditionalCommandSvg : PieceOnBoard -> Svg msg
pieceWaitingForAdditionalCommandSvg p =
    g
        [ transform ("translate(" ++ String.fromFloat (toFloat p.coord.x * 100.0 - 5.0) ++ " " ++ String.fromFloat (toFloat p.coord.y * 100.0 + 5.0) ++ ")")
        , Html.Attributes.style "cursor" "not-allowed"
        ]
        (rect
            [ x "12"
            , y "12"
            , width "80"
            , height "80"
            , fill (backgroundColor p.pieceColor)
            , stroke floatingPieceBorderColor
            , strokeWidth "2"
            ]
            []
            :: glyph p.prof (foregroundColor p.pieceColor)
        )

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


trashBinSvg : List (Svg msg)
trashBinSvg =
    {- trash bin -}
    [ Svg.path [ d "M 0.8 22.4 l 11.8 67.4 c 1 4.4 5 7.4 9.4 7.4 c 0 0 0 0 0 0 h 45.4 c 4.4 0 8.2 -3.2 9.4 -7.4 v 0 l 11.8 -67.4 z m 43.8 11.6 c 1.6 0 2.6 1.2 2.6 2.6 v 43.6 c 0 1.4 -1 2.6 -2.6 2.6 c -1.4 0 -2.6 -1.2 -2.6 -2.6 v -43.6 c 0 -1.4 1.2 -2.6 2.6 -2.6 z m -21 0 c 1.4 0 2.6 1.2 2.6 2.4 l 3.8 43.6 c 0.2 1.4 -0.8 2.6 -2.4 2.8 c -1.4 0 -2.6 -1 -2.8 -2.4 l -3.8 -43.4 c -0.2 -1.6 1 -2.8 2.4 -3 c 0.2 0 0.2 0 0.2 0 z m 42 0 c 0 0 0 0 0.2 0 c 1.4 0.2 2.6 1.4 2.4 3 l -3.8 43.4 c -0.2 1.4 -1.4 2.4 -2.8 2.4 c -1.6 -0.2 -2.6 -1.4 -2.4 -2.8 l 3.8 -43.6 c 0 -1.2 1.2 -2.4 2.6 -2.4 z" ] []
    , Svg.path [ d "m 40 0 c -1.4 0 -2.6 1.2 -2.6 2.6 V 6 L 2.6 9 A 3 3 90 0 0 0 12 v 0 v 5.8 H 89.2 v -5.8 v 0 a 3 3 90 0 0 -2.6 -3 l -34.6 -3 V 2.6 c 0 -1.4 -1 -2.6 -2.4 -2.6 z" ] []
    ]


playerSvg : WhoseTurn -> { a | victoryCrown : Bool, bigAndBlurred : Bool } -> Svg msg
playerSvg playerColor o =
    let
        id_ =
            case playerColor of
                KeseTurn ->
                    "kesePlayer"

                RimaTurn ->
                    "rimaPlayer"

        translateY =
            case playerColor of
                KeseTurn ->
                    442.0

                RimaTurn ->
                    56.75

        color =
            toColor playerColor

        scale =
            if o.bigAndBlurred then
                5.5

            else
                4.0

        transf =
            "translate(727," ++ String.fromFloat translateY ++ ") scale(" ++ String.fromFloat scale ++ ")"

        person =
            [ circle [ cx "0", cy "0", r "12", fill (backgroundColor color) ] []
            , circle [ cx "0", cy "-5.5", r "4", fill (foregroundColor color) ] []
            , Svg.path [ fill (foregroundColor color), d "m 0,0.5 c -3,0 -5.8,1 -8,3 v 3 h 16 v -3 c -2.2,-2 -5,-3 -8,-3 z" ] []
            ]

        crownStyle =
            [ x "-12", y "-12", width "24", height "24", fill crownColor ]

        crown =
            [ rect crownStyle []
            , rect (transform "rotate(30) " :: crownStyle) []
            , rect (transform "rotate(-30)" :: crownStyle) []
            ]

        blur =
            circle [ cx "0", cy "0", r "12", fill (backgroundColor color), Svg.Attributes.style ("fill:" ++ blurShadowColor ++ ";fill-opacity:1;filter:url(#blur)") ] []
    in
    if o.bigAndBlurred then
        if o.victoryCrown then
            g [ id id_, transform transf ] (crown ++ blur :: person)

        else
            g [ id id_, transform transf ] (blur :: person)

    else
        g [ id id_, transform transf ] person


trashBinSvg_ : Bool -> Svg OriginalMsg
trashBinSvg_ clickable =
    if clickable then
        g
            [ Svg.Events.onClick SendToTrashBinPart2
            , Html.Attributes.style "cursor" "pointer"
            , fill (trashBinColor clickable)
            ]
            (trashBinSvg ++ [ circle [ cx "45", cy "55", r "16", fill yellowCandidateColor ] [] ])

    else
        g [ fill (trashBinColor clickable) ] trashBinSvg

pieceSvg__ : (msg -> String) -> { a | color : String, width : String } -> msg -> PieceWithFloatPosition -> Svg msg
pieceSvg__ toIcon strok msgToBeSent p =
    g
        [ transform ("translate(" ++ String.fromFloat (p.coord.x * 100.0) ++ " " ++ String.fromFloat (p.coord.y * 100.0) ++ ")")
        , Html.Attributes.style "cursor"
            (toIcon msgToBeSent)
        , Svg.Events.onClick msgToBeSent
        ]
        (rect
            [ x "12"
            , y "12"
            , width "80"
            , height "80"
            , fill (backgroundColor p.pieceColor)
            , stroke
                strok.color
            , strokeWidth
                strok.width
            ]
            []
            :: glyph p.prof (foregroundColor p.pieceColor)
        )

twoDecks : { a | keseDeck : List b, rimaDeck : List c } -> List (Svg msg)
twoDecks model =
    [ g [ id "keseDeck" ]
        (List.indexedMap
            (\i _ ->
                rect
                    [ x "535.7"
                    , y (String.fromInt (504 - 80 - 10 * i))
                    , width "80"
                    , height "80"
                    , fill (backgroundColor Kese)
                    , strokeWidth "1"
                    , stroke (strokeColor Kese)
                    ]
                    []
            )
            model.keseDeck
        )
    , g [ id "rimaDeck" ]
        (List.indexedMap
            (\i _ ->
                rect
                    [ x "535.7"
                    , y (String.fromInt (10 * i))
                    , width "80"
                    , height "80"
                    , fill (backgroundColor Rima)
                    , strokeWidth "1"
                    , stroke (strokeColor Rima)
                    ]
                    []
            )
            model.rimaDeck
        )
    ]