module KeseRimaSvgColor exposing (..)

import Tak1Bai2Types exposing (..)


boardBackgroundColor : Coordinate -> String
boardBackgroundColor coord =
    "#ccc"


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


borderColor : CardColor -> String
borderColor c =
    case c of
        Red ->
            "#005242"

        Black ->
            "#00b592"


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


boardBorderColor : String
boardBorderColor =
    "#000"


trashBinColor : Bool -> String
trashBinColor c =
    if c then
        "#555"

    else
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
