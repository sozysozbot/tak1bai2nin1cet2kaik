module SvgColor exposing (..)

import Tak1Bai2Types exposing (..)


backgroundColor : CardColor -> String
backgroundColor pieceColor =
    case pieceColor of
        Red ->
            "#c8beb7"

        Black ->
            "#483e37"


foregroundColor : CardColor -> String
foregroundColor pieceColor =
    case pieceColor of
        Red ->
            "#c8beb7"

        Black ->
            "#483e37"


crownColor : String
crownColor =
    "#ffff00"


strokeColor : CardColor -> String
strokeColor c =
    case c of
        Red ->
            "#000"

        Black ->
            "#eee"


blurShadowColor : String
blurShadowColor =
    "#404040"


redCandidateColor : String
redCandidateColor =
    "#ff0000"


yellowCandidateColor : String
yellowCandidateColor =
    "#ffff00"


floatingPieceBorderColor : String
floatingPieceBorderColor =
    "#ffff00"
