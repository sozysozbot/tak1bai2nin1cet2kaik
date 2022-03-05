module Main exposing (init, main, view)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (href)
import KeseRimaSvgColor exposing (..)
import KeseRimaSvgElements exposing (..)
import KeseRimaTypes exposing (..)
import List.Extra
import Regex
import Svg exposing (Attribute, Svg, defs, feGaussianBlur, g, rect, svg, text)
import Svg.Attributes exposing (fill, height, id, result, stdDeviation, stroke, strokeWidth, transform, viewBox, width, x, y)
import Svg.Events exposing (onClick)
import Url.Builder exposing (crossOrigin)


type alias StateOfCards =
    StateOfCards_ Profession


type alias Flags =
    { keseGoesFirst : Bool, keseDice : Bool, rimaDice : Bool, shipDice : Bool, keseDeck : List Int, rimaDeck : List Int }


type alias CurrentStatus =
    CurrentStatus_ Profession


type Model
    = Model
        { saved : CurrentStatus -- Reverts to here when canceled
        , historyString : HistoryString
        , currentStatus : CurrentStatus
        }


main : Program Flags Model OriginalMsg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


update : OriginalMsg -> Model -> ( Model, Cmd OriginalMsg )
update msg (Model { historyString, currentStatus, saved }) =
    let
        newHist =
            historyString ++ newHistory msg currentStatus

        newStat =
            updateStatus msg currentStatus saved
    in
    if Regex.contains twoConsecutivePasses newHist || Regex.contains sixConsecutiveShipMovements newHist then
        case newStat of
            NothingSelected cardState ->
                let
                    gameEnd =
                        GameTerminated
                            { whoseVictory = Ship
                            , board = cardState.board
                            , capturedByKese = cardState.capturedByKese
                            , capturedByRima = cardState.capturedByRima
                            , keseDeck = cardState.keseDeck
                            , rimaDeck = cardState.rimaDeck
                            , keseHand = cardState.keseHand
                            , rimaHand = cardState.rimaHand
                            }
                in
                ( Model
                    { historyString = String.dropRight 1 newHist ++ "--------------------------------\nKeseRima"
                    , currentStatus =
                        gameEnd
                    , saved = gameEnd
                    }
                , Cmd.none
                )

            _ ->
                ( Model { historyString = newHist, currentStatus = newStat, saved = saved }, Cmd.none )

    else
        case newStat of
            NothingSelected cardState ->
                ( Model
                    { historyString = newHist
                    , currentStatus = newStat
                    , saved = NothingSelected cardState -- update `saved`
                    }
                , Cmd.none
                )

            _ ->
                ( Model { historyString = newHist, currentStatus = newStat, saved = saved }, Cmd.none )


newHistory : OriginalMsg -> CurrentStatus -> String
newHistory msg modl =
    let
        unwrap =
            Maybe.withDefault "ERROR!!!!!!!!!!!!!!!!!!!!"
    in
    case ( modl, msg ) of
        ( _, Cancel ) ->
            "~~~ " ++ unwrap (Maybe.map whoseTurnToHistoryStr (getWhoseTurn modl))

        ( NothingSelected cardState, GiveFocusTo (PieceInKeseHand index) ) ->
            List.Extra.getAt index cardState.keseHand |> Maybe.map profToHistoryStr |> unwrap

        ( NothingSelected cardState, GiveFocusTo (PieceInRimaHand index) ) ->
            List.Extra.getAt index cardState.rimaHand |> Maybe.map profToHistoryStr |> unwrap

        ( NothingSelected cardState, GiveFocusTo (PieceOnTheBoard coord) ) ->
            case List.filter (\p -> p.coord == coord) cardState.board of
                [ p ] ->
                    (if p.pieceColor == Ship then
                        "S"

                     else
                        ""
                    )
                        ++ profToHistoryStr p.prof
                        ++ coordToHistoryStr p.coord

                _ ->
                    "ERROR!!!!!!!!"

        ( MoverIsSelected from cardState, MovementToward to ) ->
            case from of
                PieceOnTheBoard _ ->
                    "-" ++ coordToHistoryStr to

                {- Parachuting from KeseHand -}
                PieceInKeseHand _ ->
                    case ( cardState.keseHand, cardState.keseDeck ) of
                        ( [ _ ], x :: y :: z :: _ ) ->
                            coordToHistoryStr to ++ "{" ++ String.join "" (List.map profToHistoryStr [ x, y, z ]) ++ "}.\nR"

                        _ ->
                            coordToHistoryStr to ++ ".\nR"

                {- Parachuting from RimaHand -}
                PieceInRimaHand _ ->
                    case ( cardState.rimaHand, cardState.rimaDeck ) of
                        ( [ _ ], x :: y :: z :: _ ) ->
                            coordToHistoryStr to ++ "{" ++ String.join "" (List.map profToHistoryStr [ x, y, z ]) ++ "}.\nK"

                        _ ->
                            coordToHistoryStr to ++ ".\nK"

        ( AfterSacrifice _ _, MovementToward to ) ->
            coordToHistoryStr to

        ( NowWaitingForAdditionalSacrifice { remaining }, SendToTrashBinPart1 { whoseHand, index } ) ->
            case whoseHand of
                KeseTurn ->
                    List.Extra.getAt index remaining.keseHand |> Maybe.map profToHistoryStr |> unwrap

                RimaTurn ->
                    List.Extra.getAt index remaining.rimaHand |> Maybe.map profToHistoryStr |> unwrap

        ( NowWaitingForAdditionalSacrifice { mover, remaining }, TurnEnd ) ->
            let
                cardDrawn =
                    if List.isEmpty remaining.keseHand then
                        let
                            ( keseHand, _ ) =
                                drawUpToThree remaining.keseDeck
                        in
                        "{" ++ String.join "" (List.map profToHistoryStr keseHand) ++ "}"

                    else if List.isEmpty remaining.rimaHand then
                        let
                            ( rimaHand, _ ) =
                                drawUpToThree remaining.rimaDeck
                        in
                        "{" ++ String.join "" (List.map profToHistoryStr rimaHand) ++ "}"

                    else
                        ""
            in
            case List.filter (\p -> p.coord == mover.coord) remaining.board of
                [] ->
                    cardDrawn ++ ".\n" ++ whoseTurnToHistoryStr (invertWhoseTurn remaining.whoseTurn)

                captured :: _ ->
                    {- capture -}
                    case remaining.whoseTurn of
                        KeseTurn ->
                            let
                                newCapturedByKese =
                                    captured.prof :: remaining.capturedByKese
                            in
                            if isVictorious newCapturedByKese then
                                "[" ++ profToHistoryStr captured.prof ++ "]" ++ cardDrawn ++ ".\n--------------------------------\nKese"

                            else
                                "[" ++ profToHistoryStr captured.prof ++ "]" ++ cardDrawn ++ ".\n" ++ whoseTurnToHistoryStr (invertWhoseTurn remaining.whoseTurn)

                        RimaTurn ->
                            let
                                newCapturedByRima =
                                    captured.prof :: remaining.capturedByRima
                            in
                            if isVictorious newCapturedByRima then
                                "[" ++ profToHistoryStr captured.prof ++ "]" ++ cardDrawn ++ ".\n--------------------------------\nRima"

                            else
                                "[" ++ profToHistoryStr captured.prof ++ "]" ++ cardDrawn ++ ".\n" ++ whoseTurnToHistoryStr (invertWhoseTurn remaining.whoseTurn)

        ( WaitForTrashBinClick _, SendToTrashBinPart2 ) ->
            ""

        ( AfterCircleSacrifice { remaining }, SendToTrashBinPart1 { whoseHand, index } ) ->
            case whoseHand of
                KeseTurn ->
                    List.Extra.getAt index remaining.keseHand |> Maybe.map profToHistoryStr |> unwrap

                RimaTurn ->
                    List.Extra.getAt index remaining.rimaHand |> Maybe.map profToHistoryStr |> unwrap

        _ ->
            {- Do nothing -}
            ""


updateStatus : OriginalMsg -> CurrentStatus_ Profession -> CurrentStatus_ Profession -> CurrentStatus_ Profession
updateStatus =
    updateStatus_ (\xyz () -> xyz) ()


boardSvg : List (Svg OriginalMsg)
boardSvg =
    [ g [ id "board" ]
        (List.map
            (\coord ->
                rect
                    [ x (String.fromInt (coord.x * 100 + 2))
                    , y (String.fromInt (coord.y * 100 + 2))
                    , width "100"
                    , height "100"
                    , fill (boardBackgroundColor coord)
                    , stroke boardBorderColor
                    , strokeWidth "4"
                    ]
                    []
            )
            allCoord
        )
    ]


pieceSvgOnGrid : Bool -> OriginalMsg -> PieceOnBoard -> Svg OriginalMsg
pieceSvgOnGrid focused msg { coord, prof, pieceColor } =
    pieceSvg focused msg { coord = { x = toFloat coord.x, y = toFloat coord.y }, prof = prof, pieceColor = pieceColor }


pieceSvg : Bool -> OriginalMsg -> PieceWithFloatPosition -> Svg OriginalMsg
pieceSvg focused msgToBeSent p =
    let
        strok =
            if focused then
                { color = borderColor p.pieceColor
                , width = "10"
                }

            else
                { color = "none"
                , width = "none"
                }
    in
    pieceSvg_
        strok
        msgToBeSent
        p


msgToIcon : OriginalMsg -> String
msgToIcon msgToBeSent =
    case msgToBeSent of
        None ->
            "not-allowed"

        _ ->
            "pointer"


pieceSvg_ : { a | color : String, width : String } -> OriginalMsg -> PieceWithFloatPosition -> Svg OriginalMsg
pieceSvg_ =
    pieceSvg__ msgToIcon


drawUpToThree : List a -> ( List a, List a )
drawUpToThree xs =
    case xs of
        a :: b :: c :: ys ->
            ( [ a, b, c ], ys )

        _ ->
            ( xs, [] )


displayCapturedCardsAndTwoDecks : { a | keseDeck : List b, rimaDeck : List c, capturedByKese : List Profession, capturedByRima : List Profession } -> List (Svg OriginalMsg)
displayCapturedCardsAndTwoDecks model =
    twoDecks model
        ++ [ g [ id "capturedByKese" ]
                (List.indexedMap
                    (\i prof ->
                        pieceSvg_
                            { color = strokeColor Rima

                            {- what is captured by Kese turns out to be Rima -}
                            , width = "1"
                            }
                            None
                            { coord =
                                { x =
                                    -0.115
                                        {- to handle the automatic offset and the 3px difference in the border -} + toFloat i
                                        * (KeseRimaSvgElements.spacing <| List.length <| model.capturedByKese)
                                , y = 6.0
                                }
                            , prof = prof
                            , pieceColor = Rima
                            }
                    )
                    model.capturedByKese
                )
           , g [ id "capturedByRima" ]
                (List.indexedMap
                    (\i prof ->
                        pieceSvg_
                            { color = strokeColor Kese, width = "1" }
                            None
                            { coord =
                                { x =
                                    -0.115
                                        {- to handle the automatic offset and the 3px difference in the border -} + 5.0
                                        * 0.846
                                        - toFloat i
                                        * (KeseRimaSvgElements.spacing <| List.length <| model.capturedByRima)
                                , y = -2.0
                                }
                            , prof = prof
                            , pieceColor = Kese
                            }
                    )
                    model.capturedByRima
                )
           ]


stationaryPart : StateOfCards -> List (Svg OriginalMsg)
stationaryPart cardState =
    defs []
        [ Svg.filter [ Svg.Attributes.style "color-interpolation-filters:sRGB", id "blur" ]
            [ feGaussianBlur [ stdDeviation "1.5 1.5", result "blur" ] []
            ]
        ]
        :: boardSvg
        ++ displayCapturedCardsAndTwoDecks cardState
        ++ [ playerSvg KeseTurn { victoryCrown = False, bigAndBlurred = KeseTurn == cardState.whoseTurn }
           , playerSvg RimaTurn { victoryCrown = False, bigAndBlurred = RimaTurn == cardState.whoseTurn }
           ]


twoTrashBinsSvg : Maybe WhoseTurn -> List (Svg OriginalMsg)
twoTrashBinsSvg trashBinFocus =
    [ g [ id "keseTrashBin", transform "translate(530 560)" ] [ trashBinSvg_ (trashBinFocus == Just KeseTurn) ]
    , g [ id "rimaTrashBin", transform "translate(530 -150)" ] [ trashBinSvg_ (trashBinFocus == Just RimaTurn) ]
    ]


targetBlankLink : List (Attribute msg) -> List (Html msg) -> Html msg
targetBlankLink attributes =
    Html.a (Html.Attributes.target "_blank" :: attributes)


view_ : Bool -> HistoryString -> List (Svg msg) -> List (Html msg) -> Html msg
view_ gameEndTweet history svgContent buttons =
    Html.div [ Html.Attributes.style "display" "flex" ]
        [ Html.div [ Html.Attributes.style "padding" "0px 20px 0 20px", Html.Attributes.style "min-width" "360px" ]
            [ Html.h2 [] [ Html.text "紙机戦ソリティア「衣糸紙机戦」" ]
            , Html.ul []
                (List.map (\p -> Html.li [] [ p ])
                    [ targetBlankLink [ href "https://https://sites.google.com/view/cet2kaik" ] [ Html.text "日本机戦連盟公式サイト" ]
                    ]
                )
            , Html.div [ Html.Attributes.style "font-size" "50%" ]
                (List.map (\t -> Html.p [] [ Html.text t ])
                    [ "ここに開発ログ"
                    ]
                )
            , Html.p [ Html.Attributes.style "font-size" "80%" ]
                [ targetBlankLink
                    [ href "https://github.com/sozysozbot/tak1bai2nin1cet2kaik/issues/new" ]
                    [ Html.text "バグなどありましたらここをクリックしてご報告ください" ]
                ]
            ]
        , Html.div []
            (svg [ viewBox "0 -200 900 900", width "540" ] svgContent
                :: Html.br [] []
                :: List.intersperse (Html.text " ") buttons
            )
        , Html.div []
            [ Html.textarea
                [ Html.Attributes.rows 20
                , Html.Attributes.cols 40
                , Html.Attributes.readonly True
                , Html.Attributes.style "font-family" "monospace"
                ]
                [ Html.text history ]
            , Html.br [] []
            , targetBlankLink
                [ href
                    (crossOrigin
                        "https://twitter.com"
                        [ "intent", "tweet" ]
                        [ Url.Builder.string "text"
                            ("架空伝統ゲーム「ケセリマ」(@keserima)を遊びました！ #keserima #ケセリマ\u{000D}\n"
                                ++ crossOrigin "https://keserima.github.io"
                                    [ "playback", "index.html" ]
                                    [ Url.Builder.string "playback" history
                                    ]
                            )
                        ]
                    )
                , Html.Attributes.style "font-size"
                    (if gameEndTweet then
                        "250%"

                     else
                        "120%"
                    )
                , Html.Attributes.style "font-weight" "bold"
                ]
                [ Html.text
                    (if gameEndTweet then
                        "棋譜をツイートしましょう！！"

                     else
                        "ここまでの棋譜をツイートする"
                    )
                , Html.br [] []
                , Html.img [ Html.Attributes.src "../imgs/keserima.png", Html.Attributes.height 200 ] []
                ]
            ]
        ]


view : Model -> Html OriginalMsg
view (Model { historyString, currentStatus }) =
    case currentStatus of
        NothingSelected cardState ->
            view_ False
                historyString
                (stationaryPart cardState
                    ++ twoTrashBinsSvg Nothing
                    ++ List.map
                        (\piece ->
                            pieceSvgOnGrid False
                                {- You can move the piece if it is a ship or if it belongs to you. -}
                                (if piece.pieceColor == Ship || piece.pieceColor == toColor cardState.whoseTurn then
                                    GiveFocusTo (PieceOnTheBoard piece.coord)

                                 else
                                    None
                                )
                                piece
                        )
                        cardState.board
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (if cardState.whoseTurn == KeseTurn then
                                    GiveFocusTo (PieceInKeseHand i)

                                 else
                                    None
                                )
                                (keseHandPos i prof)
                        )
                        cardState.keseHand
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (if cardState.whoseTurn == RimaTurn then
                                    GiveFocusTo (PieceInRimaHand i)

                                 else
                                    None
                                )
                                (rimaHandPos i prof)
                        )
                        cardState.rimaHand
                )
                [{- default state. no need to cancel everything: the state has been saved -}]

        GameTerminated cardState ->
            view_ True
                historyString
                (defs []
                    [ Svg.filter [ Svg.Attributes.style "color-interpolation-filters:sRGB", id "blur" ]
                        [ feGaussianBlur [ stdDeviation "1.5 1.5", result "blur" ] []
                        ]
                    ]
                    :: boardSvg
                    ++ displayCapturedCardsAndTwoDecks cardState
                    ++ [ {- /= is used so that, in case of a draw, both sides receive the crown -}
                         playerSvg KeseTurn
                            { victoryCrown = Rima /= cardState.whoseVictory
                            , bigAndBlurred = Rima /= cardState.whoseVictory
                            }
                       , playerSvg RimaTurn
                            { victoryCrown = Kese /= cardState.whoseVictory
                            , bigAndBlurred = Kese /= cardState.whoseVictory
                            }
                       ]
                    ++ twoTrashBinsSvg Nothing
                    ++ List.map (pieceSvgOnGrid False None) cardState.board
                    ++ List.indexedMap
                        (\i prof -> pieceSvg False None (keseHandPos i prof))
                        cardState.keseHand
                    ++ List.indexedMap
                        (\i prof -> pieceSvg False None (rimaHandPos i prof))
                        cardState.rimaHand
                )
                [{- The game has ended. No cancelling allowed. -}]

        MoverIsSelected focus cardState ->
            let
                dynamicPart =
                    case focus of
                        PieceOnTheBoard focus_coord ->
                            case robFocusedPieceFromBoard focus_coord cardState.board of
                                {- This branch is not taken -}
                                Nothing ->
                                    []

                                Just ( focused_piece, robbedBoard ) ->
                                    let
                                        hasCircleInHand =
                                            List.any ((==) Circle)
                                                (case cardState.whoseTurn of
                                                    KeseTurn ->
                                                        cardState.keseHand

                                                    RimaTurn ->
                                                        cardState.rimaHand
                                                )

                                        candidatesYellow =
                                            getCandidatesYellow hasCircleInHand focused_piece robbedBoard

                                        candidatesRed =
                                            case focused_piece.pieceColor of
                                                Ship ->
                                                    []

                                                Kese ->
                                                    getCandidatesYellow True focused_piece robbedBoard
                                                        |> filterWhetherMemberOf (allCoordsOccupiedBy Rima robbedBoard)

                                                Rima ->
                                                    getCandidatesYellow True focused_piece robbedBoard
                                                        |> filterWhetherMemberOf (allCoordsOccupiedBy Kese robbedBoard)
                                    in
                                    List.map
                                        (\piece -> pieceSvgOnGrid (piece.coord == focus_coord) None piece)
                                        cardState.board
                                        ++ (candidatesRed |> List.map (\coord -> goalCandidateRedSvg (MovementToward coord) coord))
                                        ++ (candidatesYellow
                                                |> List.map (\coord -> goalCandidateYellowSvg (MovementToward coord) coord)
                                           )
                                        ++ List.indexedMap
                                            (\i prof ->
                                                pieceSvg False None (keseHandPos i prof)
                                            )
                                            cardState.keseHand
                                        ++ List.indexedMap
                                            (\i prof ->
                                                pieceSvg False None (rimaHandPos i prof)
                                            )
                                            cardState.rimaHand

                        _ ->
                            {- Parachuting -}
                            List.map (pieceSvgOnGrid False None) cardState.board
                                {- You cannot capture a piece by parachuting; hence no red -}
                                ++ (neitherOccupiedNorWater cardState.board
                                        |> List.map (\coord -> goalCandidateYellowSvg (MovementToward coord) coord)
                                   )
                                ++ List.indexedMap
                                    (\i prof ->
                                        case focus of
                                            PieceInKeseHand ind ->
                                                pieceSvg (ind == i) None (keseHandPos i prof)

                                            _ ->
                                                pieceSvg False None (keseHandPos i prof)
                                    )
                                    cardState.keseHand
                                ++ List.indexedMap
                                    (\i prof ->
                                        case focus of
                                            PieceInRimaHand ind ->
                                                pieceSvg (ind == i) None (rimaHandPos i prof)

                                            _ ->
                                                pieceSvg False None (rimaHandPos i prof)
                                    )
                                    cardState.rimaHand
            in
            view_ False
                historyString
                (stationaryPart cardState ++ twoTrashBinsSvg Nothing ++ dynamicPart)
                [ simpleCancelButton ]

        NowWaitingForAdditionalSacrifice { mover, remaining } ->
            let
                isSacrificingCircleRequired =
                    case List.filter (\c -> c.coord == mover.coord) remaining.board of
                        {- If there is no stepping, circle is never required -}
                        [] ->
                            False

                        steppedOn :: _ ->
                            case ( mover.pieceColor, steppedOn.pieceColor ) of
                                ( Ship, Ship ) ->
                                    True

                                ( _, Ship ) ->
                                    False

                                ( _, _ ) ->
                                    True
            in
            view_ False
                historyString
                (stationaryPart remaining
                    ++ twoTrashBinsSvg Nothing
                    ++ List.map
                        (pieceSvgOnGrid False None {- You cannot click any piece on the board while waiting for additional sacrifices. -})
                        remaining.board
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (if remaining.whoseTurn == KeseTurn && (isSacrificingCircleRequired == (prof == Circle)) then
                                    SendToTrashBinPart1 { whoseHand = KeseTurn, index = i }

                                 else
                                    None
                                )
                                (keseHandPos i prof)
                        )
                        remaining.keseHand
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (if remaining.whoseTurn == RimaTurn && (isSacrificingCircleRequired == (prof == Circle)) then
                                    SendToTrashBinPart1 { whoseHand = RimaTurn, index = i }

                                 else
                                    None
                                )
                                (rimaHandPos i prof)
                        )
                        remaining.rimaHand
                    ++ [ pieceWaitingForAdditionalCommandSvg mover ]
                )
                (case List.filter (\p -> p.coord == mover.coord) remaining.board of
                    [ p ] ->
                        case p.pieceColor of
                            Ship ->
                                {- cannot end the turn if stepping on a Ship -}
                                [ cancelAllButton ]

                            Kese ->
                                if mover.pieceColor == Rima then
                                    [ captureAndTurnEndButton
                                    , cancelAllButton
                                    ]

                                else
                                    [ cancelAllButton ]

                            Rima ->
                                if mover.pieceColor == Kese then
                                    [ captureAndTurnEndButton
                                    , cancelAllButton
                                    ]

                                else
                                    [ cancelAllButton ]

                    _ ->
                        {- The resulting square is empty, so it is always possible to declare TurnEnd -}
                        [ turnEndButton, cancelAllButton ]
                )

        WaitForTrashBinClick { mover, remaining, whoseHand, index } ->
            view_ False
                historyString
                (stationaryPart remaining
                    ++ twoTrashBinsSvg (Just whoseHand)
                    ++ List.map
                        (pieceSvgOnGrid False None {- You cannot click any piece on the board while waiting for additional sacrifices. -})
                        remaining.board
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg (whoseHand == KeseTurn && i == index)
                                None
                                (keseHandPos i prof)
                        )
                        remaining.keseHand
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg (whoseHand == RimaTurn && i == index)
                                None
                                (rimaHandPos i prof)
                        )
                        remaining.rimaHand
                    ++ [ pieceWaitingForAdditionalCommandSvg mover ]
                )
                [ cancelAllButton ]

        AfterSacrifice command { mover, remaining } ->
            let
                hasCircleInHand =
                    List.any ((==) Circle)
                        (case remaining.whoseTurn of
                            KeseTurn ->
                                remaining.keseHand

                            RimaTurn ->
                                remaining.rimaHand
                        )

                candidatesYellow =
                    getCandidatesYellowWithCommand command hasCircleInHand mover remaining.board

                candidatesRed =
                    case mover.pieceColor of
                        Ship ->
                            []

                        Kese ->
                            getCandidatesYellowWithCommand command True mover remaining.board
                                |> filterWhetherMemberOf (allCoordsOccupiedBy Rima remaining.board)

                        Rima ->
                            getCandidatesYellowWithCommand command True mover remaining.board
                                |> filterWhetherMemberOf (allCoordsOccupiedBy Kese remaining.board)

                dynamicPart =
                    List.map
                        (pieceSvgOnGrid False None)
                        remaining.board
                        ++ (candidatesRed
                                |> List.map (\coord -> goalCandidateRedSvg (MovementToward coord) coord)
                           )
                        ++ (candidatesYellow
                                |> List.map (\coord -> goalCandidateYellowSvg (MovementToward coord) coord)
                           )
                        ++ List.indexedMap (\i prof -> pieceSvg False None (keseHandPos i prof)) remaining.keseHand
                        ++ List.indexedMap (\i prof -> pieceSvg False None (rimaHandPos i prof)) remaining.rimaHand
                        ++ [ pieceWaitingForAdditionalCommandSvg mover ]
            in
            view_ False historyString (stationaryPart remaining ++ twoTrashBinsSvg Nothing ++ dynamicPart) [ cancelAllButton ]

        AfterCircleSacrifice { mover, remaining } ->
            view_ False
                historyString
                (stationaryPart remaining
                    ++ twoTrashBinsSvg Nothing
                    ++ List.map
                        (pieceSvgOnGrid False None {- You cannot click any piece on the board while waiting for additional sacrifices. -})
                        remaining.board
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (if remaining.whoseTurn == KeseTurn && prof /= Circle then
                                    SendToTrashBinPart1 { whoseHand = KeseTurn, index = i }

                                 else
                                    None
                                )
                                (keseHandPos i prof)
                        )
                        remaining.keseHand
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (if remaining.whoseTurn == RimaTurn && prof /= Circle then
                                    SendToTrashBinPart1 { whoseHand = RimaTurn, index = i }

                                 else
                                    None
                                )
                                (rimaHandPos i prof)
                        )
                        remaining.rimaHand
                    ++ [ pieceWaitingForAdditionalCommandSvg mover ]
                )
                [ cancelAllButton ]


cancelAllButton : Html OriginalMsg
cancelAllButton =
    Html.button [ onClick Cancel, Html.Attributes.style "background-color" "#ffaaaa", Html.Attributes.style "font-size" "150%" ] [ text "全てをキャンセル" ]


simpleCancelButton : Html OriginalMsg
simpleCancelButton =
    Html.button [ onClick Cancel, Html.Attributes.style "background-color" "#ffaaaa", Html.Attributes.style "font-size" "150%" ] [ text "キャンセル" ]


captureAndTurnEndButton : Html OriginalMsg
captureAndTurnEndButton =
    Html.button [ onClick TurnEnd, Html.Attributes.style "background-color" "#aaffaa", Html.Attributes.style "font-size" "150%" ] [ text "駒を取ってターンエンド" ]


turnEndButton : Html OriginalMsg
turnEndButton =
    Html.button [ onClick TurnEnd, Html.Attributes.style "background-color" "#aaffaa", Html.Attributes.style "font-size" "150%" ] [ text "ターンエンド" ]


keseHandPos : Int -> Profession -> PieceWithFloatPosition
keseHandPos i prof =
    { coord = { x = toFloat i + 1.0, y = 5.0 }, prof = prof, pieceColor = Kese }


rimaHandPos : Int -> Profession -> PieceWithFloatPosition
rimaHandPos i prof =
    { coord = { x = 3.0 - toFloat i, y = -1.0 }, prof = prof, pieceColor = Rima }


numToProf : Int -> Profession
numToProf n =
    case n of
        0 ->
            Circle

        1 ->
            HorizontalVertical

        _ ->
            Diagonal


init : Flags -> ( Model, Cmd OriginalMsg )
init flags =
    let
        ( keseHand, keseDeck ) =
            drawUpToThree (List.map numToProf flags.keseDeck)

        ( rimaHand, rimaDeck ) =
            drawUpToThree (List.map numToProf flags.rimaDeck)

        initialStatus =
            NothingSelected
                { whoseTurn =
                    if flags.keseGoesFirst then
                        KeseTurn

                    else
                        RimaTurn
                , keseDeck = keseDeck
                , rimaDeck = rimaDeck
                , keseHand = keseHand
                , rimaHand = rimaHand
                , board =
                    initialBoard flags
                , capturedByKese = []
                , capturedByRima = []
                }
    in
    ( Model
        { historyString =
            "R"
                ++ (if flags.rimaDice then
                        "+"

                    else
                        "x"
                   )
                ++ "@11 "
                ++ "S"
                ++ (if flags.shipDice then
                        "+"

                    else
                        "x"
                   )
                ++ "@23 K"
                ++ (if flags.keseDice then
                        "+"

                    else
                        "x"
                   )
                ++ "@15\n"
                ++ "K{"
                ++ String.join "" (List.map profToHistoryStr keseHand)
                ++ "} "
                ++ "R{"
                ++ String.join "" (List.map profToHistoryStr rimaHand)
                ++ "}\n--------------------------------\n"
                ++ (if flags.keseGoesFirst then
                        "K"

                    else
                        "R"
                   )
        , currentStatus = initialStatus
        , saved = initialStatus
        }
    , Cmd.none
    )
