module Tak1Bai2Types exposing (..)


type alias Coordinate =
    { x : Int, y : Int }


type alias CoordinateFloat =
    { x : Float, y : Float }


type CardColor
    = Red
    | Black


type Profession
    = Nuak1
    | Mun1
    | Kauk2
    | Gua2
    | Kaun1
    | Dau2
    | Maun1
    | Kua2
    | Tuk2
    | Uai1
    | Io
    | Tam2


type alias CardOnBoard =
    { prof : Profession, cardColor : CardColor, coord : Coordinate, shown : Bool }


type alias HistoryString =
    String


type alias FloatingMover_ =
    { mover : CardOnBoard, remaining : Board }


type MoveCommand
    = HorizVert
    | Diag


type CurrentStatus
    = NothingSelected Board
    | GameTerminated
        { board : List CardOnBoard
        }
    | FirstHalfCompletedByHop { from : Coordinate, to : Coordinate } Board
    | FirstHalfCompletedBySlide { from : Coordinate, to : Coordinate } Board
    | SecondHalfCompleted { first_from : Coordinate, first_to : Coordinate, second_from : Coordinate, second_to : Coordinate } Board



type OriginalMsg
    = None
    | Slide { from : Coordinate, to : Coordinate }
    | Hop { from : Coordinate, to : Coordinate }
    | Cancel
    | TurnEnd {- whether it is a capture or not is determined by whether there is an overlap -}
    | SendToTrashBinPart2
    | MovementToward Coordinate


profToHistoryStr : Profession -> String
profToHistoryStr prof =
    case prof of
        Nuak1 ->
            "船"

        Mun1 ->
            "無"

        Kauk2 ->
            "兵"

        Gua2 ->
            "弓"

        Kaun1 ->
            "車"

        Dau2 ->
            "虎"

        Maun1 ->
            "馬"

        Kua2 ->
            "筆"

        Tuk2 ->
            "巫"

        Uai1 ->
            "将"

        Io ->
            "王"

        Tam2 ->
            "皇"


coordToHistoryStr : Coordinate -> String
coordToHistoryStr coord =
    String.fromInt (coord.x + 1) ++ String.fromInt (coord.y + 1)


type alias Board =
    { cards : List CardOnBoard, empty : Coordinate }


type alias CardEncodedAsInt =
    Int


getMidPoint : Coordinate -> Coordinate -> Coordinate
getMidPoint from to =
    { x = (from.x + to.x) // 2, y = (from.y + to.y) // 2 }
