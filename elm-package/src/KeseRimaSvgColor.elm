module KeseRimaSvgColor exposing (..)

import KeseRimaTypes exposing (..)


boardBackgroundColor : Coordinate -> String
boardBackgroundColor coord =
    if isWater coord then
        "#5e93b8"

    else
        "#ccc"


backgroundColor : PieceColor -> String
backgroundColor pieceColor =
    case pieceColor of
        Rima ->
            "#c8beb7"

        Kese ->
            "#483e37"

        Ship ->
            "#60859d"


foregroundColor : PieceColor -> String
foregroundColor pieceColor =
    case pieceColor of
        Kese ->
            "#c8beb7"

        Rima ->
            "#483e37"

        Ship ->
            "#222c2f"


borderColor : PieceColor -> String
borderColor c =
    case c of
        Rima ->
            "#005242"

        Kese ->
            "#00b592"

        Ship ->
            "#005242"


crownColor : String
crownColor =
    "#ffff00"


strokeColor : PieceColor -> String
strokeColor c =
    case c of
        Rima ->
            "#000"

        Kese ->
            "#eee"

        Ship ->
            {- not used -}
            "#777"


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
