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
    | NowWaitingForAdditionalSacrifice FloatingMover_


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


filterWhetherMemberOf : List a -> List a -> List a
filterWhetherMemberOf judges =
    List.filter (\c -> List.member c judges)


robIth : Int -> List a -> ( List a, List a )
robIth ind list =
    let
        newList =
            List.take ind list ++ List.drop (ind + 1) list

        xs =
            case List.drop ind list of
                x :: _ ->
                    [ x ]

                {- This path is never taken -}
                [] ->
                    []
    in
    ( xs, newList )


allCoord : List Coordinate
allCoord =
    List.concatMap
        (\y_ind ->
            List.map
                (\x_ind ->
                    { y = y_ind, x = x_ind }
                )
                [ 0, 1, 2, 3, 4 ]
        )
        [ 0, 1, 2, 3, 4 ]


addDelta : Coordinate -> ( Int, Int ) -> List Coordinate
addDelta coord ( deltaX, deltaY ) =
    let
        x =
            coord.x + deltaX

        y =
            coord.y + deltaY
    in
    if 0 <= x && x <= 4 && 0 <= y && y <= 4 then
        [ { x = x, y = y } ]

    else
        []


type alias Board =
    { cards : List CardOnBoard, empty : Coordinate }


type alias CardEncodedAsInt =
    Int


initialBoard : List CardEncodedAsInt -> Board
initialBoard cards =
    let
        foo : Int -> CardEncodedAsInt -> CardOnBoard
        foo i card =
            { coord = { x = remainderBy 7 i, y = i // 7 }
            , cardColor =
                if remainderBy 2 card == 0 then
                    Black

                else
                    Red
            , prof =
                case card // 2 of
                    0 ->
                        Mun1

                    1 ->
                        Kauk2

                    2 ->
                        Gua2

                    3 ->
                        Kaun1

                    4 ->
                        Dau2

                    5 ->
                        Maun1

                    6 ->
                        Kua2

                    7 ->
                        Tuk2

                    8 ->
                        Uai1

                    9 ->
                        Io

                    10 ->
                        Tam2

                    _ ->
                        Nuak1
            , shown = False
            }
    in
    { -- the former half has indices 0 to 23
      -- the latter half has indices 25 to 48
      cards =
        List.indexedMap foo (List.take 24 cards)
            ++ List.indexedMap (\i card -> foo (i + 25) card) (List.drop 24 cards)
    , empty = { x = 3, y = 3 }
    }


updateStatus : OriginalMsg -> CurrentStatus -> CurrentStatus -> CurrentStatus
updateStatus msg modl saved =
    case ( modl, msg ) of
        ( _, Cancel ) ->
            -- no matter what the state is, abort it and revert to what was saved last
            saved

        ( NothingSelected cardState, Hop { from, to } ) ->
            FirstHalfCompletedByHop { from = from, to = to } cardState

        ( NothingSelected cardState, Slide { from, to } ) ->
            FirstHalfCompletedBySlide { from = from, to = to } cardState

        _ ->
            modl
