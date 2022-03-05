module KeseRimaTypes exposing (..)
import Regex
import List.Extra exposing (filterNot)

type alias Coordinate =
    { x : Int, y : Int }


type alias CoordinateFloat =
    { x : Float, y : Float }


type PieceColor
    = Kese
    | Rima
    | Ship


type Profession
    = HorizontalVertical
    | Diagonal
    | Circle
    | All


type WhoseTurn
    = KeseTurn
    | RimaTurn


type alias PieceOnBoard =
    { prof : Profession, pieceColor : PieceColor, coord : Coordinate }


type alias PieceWithFloatPosition =
    { prof : Profession, pieceColor : PieceColor, coord : CoordinateFloat }


type alias HistoryString =
    String


type alias FloatingMover_ a =
    { mover : PieceOnBoard, remaining : StateOfCards_ a }


type MoveCommand
    = HorizVert
    | Diag


type alias StateOfCards_ a =
    -- This `a` should be filled with `Profession` for the main game (because the info is hidden but known)
    -- This `a` should be filled with `()` for the playback (because the info is lost and irrecoverable)
    { board : List PieceOnBoard
    , capturedByKese : List Profession
    , capturedByRima : List Profession
    , keseDeck : List a
    , rimaDeck : List a
    , keseHand : List Profession
    , rimaHand : List Profession
    , whoseTurn : WhoseTurn
    }


type Focus
    = PieceOnTheBoard Coordinate
    | PieceInKeseHand Int
    | PieceInRimaHand Int

type CurrentStatus_ a
    = NothingSelected (StateOfCards_ a)
    | GameTerminated
        { board : List PieceOnBoard
        , capturedByKese : List Profession
        , capturedByRima : List Profession
        , keseDeck : List a
        , rimaDeck : List a
        , keseHand : List Profession
        , rimaHand : List Profession
        , whoseVictory : PieceColor
        }
    | MoverIsSelected Focus (StateOfCards_ a)
    | {- Sacrifice is necessary if currently stepping; otherwise not necessary -} NowWaitingForAdditionalSacrifice (FloatingMover_ a)
    | WaitForTrashBinClick { mover : PieceOnBoard, remaining : (StateOfCards_ a), whoseHand : WhoseTurn, index : Int }
    | AfterSacrifice MoveCommand (FloatingMover_ a)
    | AfterCircleSacrifice (FloatingMover_ a)

type OriginalMsg
    = None
    | Cancel
    | TurnEnd {- whether it is a capture or not is determined by whether there is an overlap -}
    | GiveFocusTo Focus
    | SendToTrashBinPart1 { whoseHand : WhoseTurn, index : Int }
    | SendToTrashBinPart2
    | MovementToward Coordinate


toColor : WhoseTurn -> PieceColor
toColor w =
    case w of
        KeseTurn ->
            Kese

        RimaTurn ->
            Rima


isWater : Coordinate -> Bool
isWater coord =
    case ( coord.x, coord.y ) of
        ( 1, 2 ) ->
            True

        ( 2, 1 ) ->
            True

        ( 2, 2 ) ->
            True

        ( 2, 3 ) ->
            True

        ( 3, 2 ) ->
            True

        _ ->
            False


profToHistoryStr : Profession -> String
profToHistoryStr prof =
    case prof of
        Circle ->
            "o"

        HorizontalVertical ->
            "+"

        Diagonal ->
            "x"

        All ->
            "*"

whoseTurnToHistoryStr : WhoseTurn -> String
whoseTurnToHistoryStr w =
    case w of
        KeseTurn ->
            "K"

        RimaTurn ->
            "R"


invertWhoseTurn : WhoseTurn -> WhoseTurn
invertWhoseTurn w =
    case w of
        KeseTurn ->
            RimaTurn

        RimaTurn ->
            KeseTurn


coordToHistoryStr : Coordinate -> String
coordToHistoryStr coord =
    String.fromInt (coord.x + 1) ++ String.fromInt (coord.y + 1)

twoConsecutivePasses : Regex.Regex
twoConsecutivePasses =
    {- Unforgivable dark magic -}
    Maybe.withDefault Regex.never <|
        Regex.fromString "([RK]o[1-5][1-5]-[1-5][1-5]\\.\\n){2}"

sixConsecutiveShipMovements : Regex.Regex
sixConsecutiveShipMovements =
    {- Unforgivable dark magic -}
    Maybe.withDefault Regex.never <|
        Regex.fromString "([RK]S[^.]*\\.\\n){6}"

getWhoseTurn : CurrentStatus_ a -> Maybe WhoseTurn
getWhoseTurn modl =
    case modl of
        NothingSelected { whoseTurn } ->
            Just whoseTurn

        MoverIsSelected _ { whoseTurn } ->
            Just whoseTurn

        NowWaitingForAdditionalSacrifice { remaining } ->
            Just remaining.whoseTurn

        AfterSacrifice _ { remaining } ->
            Just remaining.whoseTurn

        AfterCircleSacrifice { remaining } ->
            Just remaining.whoseTurn

        WaitForTrashBinClick { remaining } ->
            Just remaining.whoseTurn

        GameTerminated _ ->
            Nothing



isVictorious : List Profession -> Bool
isVictorious list =
    List.member All list || List.all (\p -> List.member p list) [ Diagonal, HorizontalVertical, Circle ]


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

neitherOccupiedNorWater : List PieceOnBoard -> List Coordinate
neitherOccupiedNorWater board =
    allCoord
        |> filterNot (\coord -> List.member coord (List.map .coord board))
        |> filterNot isWater


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


robFocusedPieceFromBoard : Coordinate -> List PieceOnBoard -> Maybe ( PieceOnBoard, List PieceOnBoard )
robFocusedPieceFromBoard coord board =
    case List.filter (\p -> p.coord == coord) board of
        [ piece ] ->
            Just ( piece, List.filter (\p -> p.coord /= coord) board )

        {- This branch is not taken -}
        _ ->
            Nothing


getCandidatesYellow : Bool -> PieceOnBoard -> List PieceOnBoard -> List Coordinate
getCandidatesYellow hasCircleInHand piece robbedBoard =
    getCandidatesYellow_ piece
        hasCircleInHand
        robbedBoard
        (rawCandidates piece.prof piece.coord)


rawCandidates : Profession -> Coordinate -> List Coordinate
rawCandidates prof coord =
    case prof of
        Circle ->
            [ coord ]

        HorizontalVertical ->
            List.concatMap (addDelta coord) [ ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ]

        Diagonal ->
            List.concatMap (addDelta coord) [ ( 1, 1 ), ( -1, -1 ), ( -1, 1 ), ( 1, -1 ) ]

        All ->
            List.concatMap (addDelta coord)
                [ ( 1, 1 ), ( -1, -1 ), ( -1, 1 ), ( 1, -1 ), ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ), ( 0, 0 ) ]


getCandidatesYellow_ : PieceOnBoard -> Bool -> List PieceOnBoard -> List Coordinate -> List Coordinate
getCandidatesYellow_ piece hasCircleInHand robbedBoard raw_candidates =
    let
        shipPositions =
            robbedBoard |> List.filter (\p -> p.pieceColor == Ship) |> List.map .coord
    in
    case piece.pieceColor of
        {- If ship, cannot leave water -}
        Ship ->
            if hasCircleInHand then
                {- Allowed location: water OR ships -}
                List.filter isWater raw_candidates
                    ++ filterWhetherMemberOf shipPositions raw_candidates

            else
                {- Allowed location: water -}
                List.filter isWater raw_candidates

        {- If not ship, then restriction on water -}
        _ ->
            if hasCircleInHand then
                {- Allowed location: non-water OR ships -}
                filterNot isWater raw_candidates
                    ++ filterWhetherMemberOf shipPositions raw_candidates

            else
                {- Allowed location: (non-water AND unoccupied) OR ships -}
                filterWhetherMemberOf (neitherOccupiedNorWater robbedBoard) raw_candidates
                    ++ filterWhetherMemberOf shipPositions raw_candidates


getCandidatesYellowWithCommand : MoveCommand -> Bool -> PieceOnBoard -> List PieceOnBoard -> List Coordinate
getCandidatesYellowWithCommand moveCommand hasCircleInHand piece robbedBoard =
    getCandidatesYellow_ piece
        hasCircleInHand
        robbedBoard
        (case moveCommand of
            HorizVert ->
                List.concatMap (addDelta piece.coord) [ ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ]

            Diag ->
                List.concatMap (addDelta piece.coord) [ ( 1, 1 ), ( -1, -1 ), ( -1, 1 ), ( 1, -1 ) ]
        )


allCoordsOccupiedBy : PieceColor -> List PieceOnBoard -> List Coordinate
allCoordsOccupiedBy color board =
    board |> List.filter (\p -> p.pieceColor == color) |> List.map .coord


initialBoard : { a | rimaDice : Bool, keseDice : Bool, shipDice : Bool } -> List PieceOnBoard
initialBoard flags =
    [ { coord = { x = 0, y = 0 }
      , pieceColor = Rima
      , prof =
            if flags.rimaDice then
                HorizontalVertical

            else
                Diagonal
      }
    , { coord = { x = 1, y = 0 }, pieceColor = Rima, prof = Circle }
    , { coord = { x = 2, y = 0 }, pieceColor = Rima, prof = All }
    , { coord = { x = 3, y = 0 }, pieceColor = Rima, prof = Circle }
    , { coord = { x = 4, y = 0 }
      , pieceColor = Rima
      , prof =
            if not flags.rimaDice then
                HorizontalVertical

            else
                Diagonal
      }
    , { coord = { x = 0, y = 4 }
      , pieceColor = Kese
      , prof =
            if flags.keseDice then
                HorizontalVertical

            else
                Diagonal
      }
    , { coord = { x = 1, y = 4 }, pieceColor = Kese, prof = Circle }
    , { coord = { x = 2, y = 4 }, pieceColor = Kese, prof = All }
    , { coord = { x = 3, y = 4 }, pieceColor = Kese, prof = Circle }
    , { coord = { x = 4, y = 4 }
      , pieceColor = Kese
      , prof =
            if not flags.keseDice then
                HorizontalVertical

            else
                Diagonal
      }
    , { coord = { x = 1, y = 2 }
      , pieceColor = Ship
      , prof =
            if flags.shipDice then
                HorizontalVertical

            else
                Diagonal
      }
    , { coord = { x = 3, y = 2 }
      , pieceColor = Ship
      , prof =
            if not flags.shipDice then
                HorizontalVertical

            else
                Diagonal
      }
    ]

drawUpToThree : List a -> ( List a, List a )
drawUpToThree xs =
    case xs of
        a :: b :: c :: ys ->
            ( [ a, b, c ], ys )

        _ ->
            ( xs, [] )

updateStatus_ : (List a -> b -> List Profession) -> b -> OriginalMsg -> CurrentStatus_ a -> CurrentStatus_ a -> CurrentStatus_ a
updateStatus_ cardsDrawnInfoGen d msg modl saved =
    -- cardsDrawnInfoGen should be (\xyz () -> xyz) in `play/`
    -- because the deck info is already known but hidden.
    -- No further info is needed to determine the cards that are drawn,
    -- so `d` can be ().
    -- In `playback/`, the `d` should supply the card infos when needed.
    -- [x, y, z] argument can be ignored, since that is always a [(), (), ()].
    case ( modl, msg ) of
        ( _, Cancel ) ->
            -- no matter what the state is, abort it and revert to what was saved last
            saved

        ( NothingSelected cardState, GiveFocusTo focus ) ->
            MoverIsSelected focus cardState

        ( MoverIsSelected from cardState, MovementToward to ) ->
            case from of
                PieceOnTheBoard coord ->
                    case robFocusedPieceFromBoard coord cardState.board of
                        {- This branch is not taken -}
                        Nothing ->
                            modl

                        Just ( piece, robbedBoard ) ->
                            NowWaitingForAdditionalSacrifice
                                { {- Updates the position here -} mover = { piece | coord = to }
                                , remaining = { cardState | board = robbedBoard }
                                }

                {- Parachuting from KeseHand -}
                PieceInKeseHand ind ->
                    let
                        ( profs {- always a singleton -}, newKeseHand ) =
                            robIth ind cardState.keseHand

                        newBoard =
                            List.map (\prof -> { pieceColor = Kese, coord = to, prof = prof }) profs
                                ++ cardState.board
                    in
                    case ( newKeseHand, cardState.keseDeck ) of
                        ( [], x :: y :: z :: zs ) ->
                            NothingSelected
                                { cardState
                                    | board = newBoard
                                    , keseHand = cardsDrawnInfoGen [ x, y, z ] d
                                    , whoseTurn = RimaTurn
                                    , keseDeck = zs
                                }

                        _ ->
                            NothingSelected { cardState | board = newBoard, keseHand = newKeseHand, whoseTurn = RimaTurn }

                {- Parachuting from RimaHand -}
                PieceInRimaHand ind ->
                    let
                        ( profs {- always a singleton -}, newRimaHand ) =
                            robIth ind cardState.rimaHand

                        newBoard =
                            List.map (\prof -> { pieceColor = Rima, coord = to, prof = prof }) profs
                                ++ cardState.board
                    in
                    case ( newRimaHand, cardState.rimaDeck ) of
                        ( [], x :: y :: z :: zs ) ->
                            NothingSelected
                                { cardState
                                    | board = newBoard
                                    , rimaHand = cardsDrawnInfoGen [ x, y, z ] d
                                    , whoseTurn = KeseTurn
                                    , rimaDeck = zs
                                }

                        _ ->
                            NothingSelected { cardState | board = newBoard, rimaHand = newRimaHand, whoseTurn = KeseTurn }

        ( AfterSacrifice _ { mover, remaining }, MovementToward to ) ->
            NowWaitingForAdditionalSacrifice { mover = { mover | coord = to }, remaining = remaining }

        ( NowWaitingForAdditionalSacrifice { mover, remaining }, SendToTrashBinPart1 { whoseHand, index } ) ->
            WaitForTrashBinClick { mover = mover, remaining = remaining, whoseHand = whoseHand, index = index }

        ( NowWaitingForAdditionalSacrifice { mover, remaining }, TurnEnd ) ->
            let
                cardDrawn =
                    if List.isEmpty remaining.keseHand then
                        let
                            ( keseHand, keseDeck ) =
                                drawUpToThree remaining.keseDeck
                        in
                        { remaining
                            | keseHand = cardsDrawnInfoGen keseHand d
                            , keseDeck = keseDeck
                        }

                    else if List.isEmpty remaining.rimaHand then
                        let
                            ( rimaHand, rimaDeck ) =
                                drawUpToThree remaining.rimaDeck
                        in
                        { remaining
                            | rimaHand = cardsDrawnInfoGen rimaHand d
                            , rimaDeck = rimaDeck
                        }

                    else
                        remaining
            in
            case List.filter (\p -> p.coord == mover.coord) remaining.board of
                [] ->
                    NothingSelected
                        { cardDrawn
                            | whoseTurn =
                                case cardDrawn.whoseTurn of
                                    KeseTurn ->
                                        RimaTurn

                                    RimaTurn ->
                                        KeseTurn
                            , board = mover :: cardDrawn.board
                        }

                captured :: _ ->
                    {- capture -}
                    let
                        newBoard =
                            mover :: List.Extra.remove captured remaining.board
                    in
                    case remaining.whoseTurn of
                        KeseTurn ->
                            let
                                newCapturedByKese =
                                    captured.prof :: remaining.capturedByKese
                            in
                            if isVictorious newCapturedByKese then
                                {- end the game without drawing cards -}
                                GameTerminated
                                    { whoseVictory = Kese
                                    , board = newBoard
                                    , capturedByKese = newCapturedByKese
                                    , capturedByRima = remaining.capturedByRima
                                    , keseDeck = remaining.keseDeck
                                    , rimaDeck = remaining.rimaDeck
                                    , keseHand = remaining.keseHand
                                    , rimaHand = remaining.rimaHand
                                    }

                            else
                                NothingSelected
                                    { cardDrawn
                                        | whoseTurn = RimaTurn
                                        , board = newBoard
                                        , capturedByKese = newCapturedByKese
                                    }

                        RimaTurn ->
                            let
                                newCapturedByRima =
                                    captured.prof :: remaining.capturedByRima
                            in
                            if isVictorious newCapturedByRima then
                                {- end the game without drawing cards -}
                                GameTerminated
                                    { whoseVictory = Rima
                                    , board = newBoard
                                    , capturedByRima = newCapturedByRima
                                    , capturedByKese = remaining.capturedByKese
                                    , keseDeck = remaining.keseDeck
                                    , rimaDeck = remaining.rimaDeck
                                    , keseHand = remaining.keseHand
                                    , rimaHand = remaining.rimaHand
                                    }

                            else
                                NothingSelected
                                    { cardDrawn
                                        | whoseTurn = KeseTurn
                                        , board = newBoard
                                        , capturedByRima = newCapturedByRima
                                    }

        ( WaitForTrashBinClick { mover, remaining, whoseHand, index }, SendToTrashBinPart2 ) ->
            case whoseHand of
                KeseTurn ->
                    let
                        ( sacrifices {- always a singleton -}, newKeseHand ) =
                            robIth index remaining.keseHand

                        new =
                            { mover = mover, remaining = { remaining | keseHand = newKeseHand } }
                    in
                    case sacrifices of
                        [ Circle ] ->
                            AfterCircleSacrifice new

                        [ HorizontalVertical ] ->
                            AfterSacrifice HorizVert new

                        [ Diagonal ] ->
                            AfterSacrifice Diag new

                        {- this path shall not be taken -}
                        _ ->
                            modl

                RimaTurn ->
                    let
                        ( sacrifices {- always a singleton -}, newRimaHand ) =
                            robIth index remaining.rimaHand

                        new =
                            { mover = mover, remaining = { remaining | rimaHand = newRimaHand } }
                    in
                    case sacrifices of
                        {- FIXME -}
                        [ Circle ] ->
                            AfterCircleSacrifice new

                        [ HorizontalVertical ] ->
                            AfterSacrifice HorizVert new

                        [ Diagonal ] ->
                            AfterSacrifice Diag new

                        {- this path shall not be taken -}
                        _ ->
                            modl

        ( AfterCircleSacrifice { mover, remaining }, SendToTrashBinPart1 { whoseHand, index } ) ->
            WaitForTrashBinClick { mover = mover, remaining = remaining, whoseHand = whoseHand, index = index }

        _ ->
            modl
