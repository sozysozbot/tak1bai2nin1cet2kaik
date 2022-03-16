module Main exposing (init, main, view)

import Browser
import Browser.Events exposing (onKeyDown)
import Buttons exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (href)
import Json.Decode as Decode
import List exposing (isEmpty)
import Round
import Sizes exposing (..)
import Svg exposing (Attribute, Svg, g, path, rect, svg)
import Svg.Attributes exposing (d, fill, height, stroke, strokeWidth, transform, viewBox, width, x, y)
import Tak1Bai2Types exposing (..)
import Time
import Url.Builder exposing (crossOrigin)


type alias Flags =
    { cards : List CardEncodedAsInt }


type Model
    = Model
        { saved : CurrentStatus -- Reverts to here when canceled
        , historyString : HistoryString
        , currentStatus : CurrentStatus
        , eyeIsOpen : Bool
        , currentTimer : TimerStatus
        }


type TimerStatus
    = NotStarted
    | CurrentlyCounting Int
    | StoppedCounting Int


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


keyDecoder : Decode.Decoder KeyValue
keyDecoder =
    Decode.map toKeyValue (Decode.field "key" Decode.string)


toKeyValue : String -> KeyValue
toKeyValue string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map AddKey keyDecoder)
        , Time.every 100 (\_ -> Tick)
        ]


initiateTimerIfOff : TimerStatus -> TimerStatus
initiateTimerIfOff a =
    case a of
        NotStarted ->
            CurrentlyCounting 0

        _ ->
            a


stopTimer : TimerStatus -> TimerStatus
stopTimer a =
    case a of
        NotStarted ->
            StoppedCounting 0

        CurrentlyCounting i ->
            StoppedCounting i

        StoppedCounting i ->
            StoppedCounting i


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model { historyString, currentStatus, saved, eyeIsOpen, currentTimer }) as modl) =
    case msg of
        Tick ->
            let
                newTimer =
                    case currentTimer of
                        NotStarted ->
                            NotStarted

                        CurrentlyCounting i ->
                            CurrentlyCounting (i + 1)

                        StoppedCounting i ->
                            StoppedCounting i
            in
            ( Model
                { historyString = historyString
                , currentStatus = currentStatus
                , saved = saved
                , eyeIsOpen = eyeIsOpen
                , currentTimer = newTimer
                }
            , Cmd.none
            )

        AddKey (Control "Escape") ->
            update Cancel modl

        AddKey (Control "Enter") ->
            let
                enterMeansMatch =
                    case currentStatus of
                        SecondHalfCompleted coords board ->
                            isMatchFromCoords coords board

                        _ ->
                            Nothing
            in
            case enterMeansMatch of
                Just True ->
                    update Match modl

                Just False ->
                    update Mismatch modl

                _ ->
                    ( modl, Cmd.none )

        AddKey (Character 'e') ->
            if eyeIsOpen then
                update CloseTheEye modl

            else
                update OpenTheEye modl

        AddKey (Character 'w') ->
            case currentStatus of
                NothingSelected board ->
                    case possibleSlidePosition board |> List.filter (\c -> c.y == board.empty.y - 1 && c.x == board.empty.x) of
                        [ from ] ->
                            update (Slide { from = from, to = board.empty }) modl

                        _ ->
                            ( modl, Cmd.none )

                _ ->
                    ( modl, Cmd.none )

        AddKey (Character 'a') ->
            case currentStatus of
                NothingSelected board ->
                    case possibleSlidePosition board |> List.filter (\c -> c.x == board.empty.x - 1 && c.y == board.empty.y) of
                        [ from ] ->
                            update (Slide { from = from, to = board.empty }) modl

                        _ ->
                            ( modl, Cmd.none )

                _ ->
                    ( modl, Cmd.none )

        AddKey (Character 's') ->
            case currentStatus of
                NothingSelected board ->
                    case possibleSlidePosition board |> List.filter (\c -> c.y == board.empty.y + 1 && c.x == board.empty.x) of
                        [ from ] ->
                            update (Slide { from = from, to = board.empty }) modl

                        _ ->
                            ( modl, Cmd.none )

                _ ->
                    ( modl, Cmd.none )

        AddKey (Character 'd') ->
            case currentStatus of
                NothingSelected board ->
                    case possibleSlidePosition board |> List.filter (\c -> c.x == board.empty.x + 1 && c.y == board.empty.y) of
                        [ from ] ->
                            update (Slide { from = from, to = board.empty }) modl

                        _ ->
                            ( modl, Cmd.none )

                _ ->
                    ( modl, Cmd.none )

        _ ->
            if eyeIsOpen then
                -- the only thing you can do is to close the eye
                if msg == CloseTheEye then
                    ( Model
                        { historyString = historyString
                        , currentStatus = currentStatus
                        , saved = saved
                        , eyeIsOpen = False
                        , currentTimer = initiateTimerIfOff currentTimer
                        }
                    , Cmd.none
                    )

                else
                    ( modl, Cmd.none )

            else if msg == OpenTheEye then
                ( Model
                    { historyString = historyString
                    , currentStatus = currentStatus
                    , saved = saved
                    , eyeIsOpen = True
                    , currentTimer = initiateTimerIfOff currentTimer
                    }
                , Cmd.none
                )

            else
                let
                    { newStatus, additionToHistory } =
                        updateStatus currentTimer msg currentStatus saved

                    newHist =
                        historyString ++ additionToHistory
                in
                case newStatus of
                    NothingSelected board ->
                        ( Model
                            { historyString = newHist
                            , currentStatus = newStatus
                            , saved = NothingSelected board -- update `saved`
                            , eyeIsOpen = False
                            , currentTimer =
                                if isStuck board then
                                    stopTimer currentTimer

                                else
                                    initiateTimerIfOff currentTimer
                            }
                        , Cmd.none
                        )

                    _ ->
                        ( Model
                            { historyString = newHist
                            , currentStatus = newStatus
                            , saved = saved
                            , eyeIsOpen = False
                            , currentTimer = initiateTimerIfOff currentTimer
                            }
                        , Cmd.none
                        )


getCountFromTimer : TimerStatus -> Int
getCountFromTimer a =
    case a of
        NotStarted ->
            0

        CurrentlyCounting i ->
            i

        StoppedCounting i ->
            i


targetBlankLink : List (Attribute msg) -> List (Html msg) -> Html msg
targetBlankLink attributes =
    Html.a (Html.Attributes.target "_blank" :: attributes)


cardHtmlImage : { a | prof : Profession, cardColor : CardColor } -> Html msg
cardHtmlImage a =
    Html.img [ Html.Attributes.src (toExternalSvgFilePath a), Html.Attributes.height 100, Html.Attributes.style "vertical-align" "middle" ] []


stringFromTimer : TimerStatus -> String
stringFromTimer currentTimer =
    Round.round 1 (0.1 * toFloat (getCountFromTimer currentTimer))


view__ :
    { maybeAudioUrl : Maybe String
    , pairnum : Int
    , gameEnd : Bool
    , history : HistoryString
    , currentTimer : TimerStatus
    }
    -> List (Svg msg)
    -> List (Html msg)
    -> Html msg
view__ { maybeAudioUrl, pairnum, gameEnd, history, currentTimer } svgContent buttons =
    let
        audio =
            case maybeAudioUrl of
                Nothing ->
                    []

                Just audioUrl ->
                    [ Html.audio [ Html.Attributes.controls False, Html.Attributes.autoplay True ]
                        [ Html.source [ Html.Attributes.src audioUrl ] []
                        ]
                    ]
    in
    Html.div [ Html.Attributes.style "display" "flex" ]
        (audio
            ++ [ Html.div [ Html.Attributes.style "padding" "0px 20px 0 20px", Html.Attributes.style "min-width" "360px" ]
                    [ Html.h2 [] [ Html.text "紙机戦ソリティア「衣糸紙机戦」" ]
                    , Html.ul []
                        (List.map (\p -> Html.li [] [ p ])
                            [ targetBlankLink [ href "https://sites.google.com/view/cet2kaik" ] [ Html.text "日本机戦連盟公式サイト" ]
                            , targetBlankLink [ href "https://cet2kaik.booth.pm/" ] [ Html.text "机戦・紙机戦の購入はこちらから" ]
                            , targetBlankLink [ href "https://github.com/sozysozbot/tak1bai2nin1cet2kaik/issues/new" ] [ Html.text "バグを報告/機能を提案" ]
                            ]
                        )
                    , Html.h3 [ Html.Attributes.style "font-size" "80%" ] [ Html.text "ルール" ]
                    , Html.p [ Html.Attributes.style "font-size" "80%" ]
                        [ Html.text "カードをめくって、同一札の黒と赤でペアを作っていく遊びです。"
                        , Html.br [] []
                        , Html.text "空きマスに向かって飛び越えると、飛び越えられたカードがめくられます。"
                        , Html.br [] []
                        , Html.text "一打目では空きマスに向かってスライドすることもでき、"
                        , Html.br [] []
                        , Html.text "その場合はスライドしたカード自身がめくられます。"
                        , Html.br [] []
                        , Html.text "ペアができたらそのペアはずっと表のままです。手詰まりになったら終了。"
                        ]
                    , Html.p [ Html.Attributes.style "font-size" "80%" ]
                        [ Html.text "なお、 "
                        , cardHtmlImage { prof = Dau2, cardColor = Black }
                        , Html.text " = "
                        , cardHtmlImage { prof = Maun1, cardColor = Black }
                        , Html.text " および "
                        , cardHtmlImage { prof = Kua2, cardColor = Black }
                        , Html.text " = "
                        , cardHtmlImage { prof = Tuk2, cardColor = Black }
                        , Html.text " = "
                        , cardHtmlImage { prof = Uai1, cardColor = Black }
                        ]
                    , Html.p [ Html.Attributes.style "font-size" "80%" ]
                        [ Html.text "そして "
                        , cardHtmlImage { prof = Io, cardColor = Black }
                        , Html.text " = "
                        , cardHtmlImage { prof = Tam2, cardColor = Black }
                        , Html.text " に注意。"
                        ]
                    , Html.p [ Html.Attributes.style "font-size" "80%" ]
                        [ Html.h3 [] [ Html.text "キーボードでの操作" ]
                        , Html.text "盤の下までいちいちマウスカーソルを持って行くのが面倒という人のために、"
                        , Html.ul []
                            [ Html.li [] [ Html.text "Esc キーでキャンセル" ]
                            , Html.li [] [ Html.text "E キーで目の開閉（カナ入力になっていると失敗することがある）" ]
                            , Html.li [] [ Html.text "Enter キーで「マッチ」または「ミスマッチ」" ]
                            , Html.li [] [ Html.text "一打目に W,A,S,D キーで[上/左/下/右]方向にあるカードをスライド" ]
                            ]
                        ]
                    ]
               , Html.div []
                    (Html.div
                        [ Html.Attributes.style "min-height" "25px"
                        , Html.Attributes.style "text-align" "center"
                        ]
                        (if gameEnd then
                            [ Html.span
                                [ Html.Attributes.style "font-weight" "bold"
                                , Html.Attributes.style "font-size" "150%"
                                , Html.Attributes.style "color" "#0000ee"
                                ]
                                [ Html.text "詰み！！" ]
                            ]

                         else
                            []
                        )
                        :: Html.div
                            [ Html.Attributes.style "min-height" "35px"
                            , Html.Attributes.style "text-align" "center"
                            ]
                            [ Html.span []
                                [ Html.text
                                    ("現在のペア数: "
                                        ++ String.fromInt pairnum
                                        ++ "\u{3000}\u{3000}経過時間: "
                                        ++ stringFromTimer currentTimer
                                        ++ "秒"
                                    )
                                ]
                            ]
                        :: svg [ viewBox "-100 -100 1050 1050", width "540" ] svgContent
                        :: Html.br [] []
                        :: List.intersperse (Html.text " ") buttons
                    )
               , Html.div [ Html.Attributes.style "margin-left" "15px" ]
                    [ Html.textarea
                        [ Html.Attributes.rows 20
                        , Html.Attributes.cols 60
                        , Html.Attributes.readonly True
                        , Html.Attributes.style "font-family" "monospace"
                        , Html.Attributes.style "font-size" "70%"
                        ]
                        [ Html.text history ]
                    , Html.br [] []
                    , targetBlankLink
                        [ href
                            (crossOrigin
                                "https://twitter.com"
                                [ "intent", "tweet" ]
                                [ Url.Builder.string "text"
                                    ("「衣糸紙机戦」(@cet2kaik)を遊びました！ #紙机戦 #机戦 #nincetkaik #cetkaik\u{000D}\n"
                                        ++ "ペア数: "
                                        ++ String.fromInt pairnum
                                        ++ ", 経過時間: "
                                        ++ stringFromTimer currentTimer
                                        ++ "秒\u{000D}\n"
                                        ++ crossOrigin "https://sozysozbot.github.io"
                                            [ "tak1bai2nin1cet2kaik", "playback", "index.html" ]
                                            [ Url.Builder.string "playback" history
                                            ]
                                    )
                                ]
                            )
                        , Html.Attributes.style "font-size"
                            (if gameEnd then
                                "250%"

                             else
                                "120%"
                            )
                        , Html.Attributes.style "font-weight" "bold"
                        ]
                        [ Html.text
                            (if gameEnd then
                                "棋譜をツイートしましょう！！"

                             else
                                "ここまでの棋譜をツイートする"
                            )
                        , Html.br [] []
                        , Html.img [ Html.Attributes.src "../img/nin1cet2kaik_photo.jpg", Html.Attributes.height 200 ] []
                        ]
                    ]
               ]
        )


displayCard : { eyeIsOpen : Bool } -> CardOnBoard -> Svg msg
displayCard a c =
    let
        parity =
            modBy 2 (c.coord.x + c.coord.y)

        widthHalf =
            if parity == 0 then
                shortEdgeHalf

            else
                longEdgeHalf

        heightHalf =
            if parity == 0 then
                longEdgeHalf

            else
                shortEdgeHalf

        width_text =
            String.fromFloat (widthHalf * 2.0)

        height_text =
            String.fromFloat (heightHalf * 2.0)

        x_coord_mid =
            toFloat c.coord.x * lattice_size

        y_coord_mid =
            toFloat c.coord.y * lattice_size
    in
    g
        [ transform
            ("translate("
                ++ String.fromFloat (x_coord_mid - widthHalf)
                ++ " "
                ++ String.fromFloat (y_coord_mid - heightHalf)
                ++ ")"
            )
        ]
        (if not c.shown && not a.eyeIsOpen then
            [ rect
                [ x "0"
                , y "0"
                , width width_text
                , height height_text
                , fill
                    "#000000"
                , stroke "none"
                , strokeWidth "none"
                ]
                []
            ]

         else if parity == 0 then
            [ rect
                [ x "0"
                , y "0"
                , width width_text
                , height height_text
                , fill
                    "#ffffff"
                , stroke "none"
                , strokeWidth "none"
                ]
                []
            , Svg.image
                [ width width_text
                , height height_text
                , Svg.Attributes.xlinkHref (toExternalSvgFilePath c)
                ]
                []
            ]

         else
            [ rect
                [ x "0"
                , y "0"
                , width width_text
                , height height_text
                , fill
                    "#ffffff"
                , stroke "none"
                , strokeWidth "none"
                ]
                []
            , g [ transform ("translate(" ++ width_text ++ ") rotate(90)") ]
                [ Svg.image
                    [ width height_text -- intentional: rotated
                    , height width_text -- intentional: rotated
                    , Svg.Attributes.xlinkHref (toExternalSvgFilePath c)
                    ]
                    []
                ]
            ]
        )


nthNeighbor : Int -> Coordinate -> List Coordinate
nthNeighbor n coord =
    [ { coord | x = coord.x - n }
    , { coord | y = coord.y - n }
    , { coord | x = coord.x + n }
    , { coord | y = coord.y + n }
    ]
        |> List.filter (\c -> c.x >= 0 && c.x < 7 && c.y >= 0 && c.y < 7)


backgroundWoodenBoard : { a | eyeIsOpen : Bool } -> Svg msg
backgroundWoodenBoard a =
    rect
        [ fill
            (if a.eyeIsOpen then
                "#bfbfbf"

             else
                "#e0c39d"
            )
        , x "-100"
        , y "-100"
        , width "1050"
        , height "1050"
        ]
        []


possibleSlidePosition : Board -> List Coordinate
possibleSlidePosition board =
    nthNeighbor 1 board.empty
        -- A card cannot slide if it is already shown
        |> List.filter (\coord -> isShownAt board coord /= Just True)


possibleHopPosition : Board -> List Coordinate
possibleHopPosition board =
    nthNeighbor 2 board.empty
        |> List.filter (\neighbor -> isShownAt board (getMidpoint neighbor board.empty) /= Just True)


getCardAt : Board -> Coordinate -> Maybe CardOnBoard
getCardAt board coord =
    List.filter (\c -> c.coord == coord) board.cards |> List.head


isShownAt : Board -> Coordinate -> Maybe Bool
isShownAt board coord =
    getCardAt board coord |> Maybe.map .shown


getPairNumFromBoard : Board -> Int
getPairNumFromBoard b =
    List.filter .shown b.cards |> List.length |> (\x -> x // 2)


view : Model -> Html Msg
view (Model { historyString, currentStatus, eyeIsOpen, currentTimer }) =
    case currentStatus of
        NothingSelected board ->
            view__
                { maybeAudioUrl = Nothing
                , pairnum = getPairNumFromBoard board
                , gameEnd = isStuck board
                , history = historyString
                , currentTimer = currentTimer
                }
                (backgroundWoodenBoard { eyeIsOpen = eyeIsOpen }
                    :: List.map (displayCard { eyeIsOpen = eyeIsOpen }) board.cards
                    ++ List.map (\c -> candidateYellowSvg { eyeIsOpen = eyeIsOpen } (Slide { from = c, to = board.empty }) c) (possibleSlidePosition board)
                    ++ List.map (\c -> candidateYellowSvg { eyeIsOpen = eyeIsOpen } (Hop { from = c, to = board.empty }) c) (possibleHopPosition board)
                )
                [ eyeButton { eyeIsOpen = eyeIsOpen } ]

        FirstHalfCompletedByHop { from, to } board ->
            view__
                { maybeAudioUrl = Nothing
                , pairnum = getPairNumFromBoard board
                , gameEnd = False
                , history = historyString
                , currentTimer = currentTimer
                }
                (backgroundWoodenBoard { eyeIsOpen = eyeIsOpen }
                    :: drawArrow Yellow from to
                    :: List.map (displayCard { eyeIsOpen = eyeIsOpen }) board.cards
                    ++ List.map (\c -> candidateGreenSvg { eyeIsOpen = eyeIsOpen } (Hop { from = c, to = board.empty }) c) (possibleHopPosition board)
                )
                [ eyeButton { eyeIsOpen = eyeIsOpen }, simpleCancelButton { eyeIsOpen = eyeIsOpen } ]

        FirstHalfCompletedBySlide { from, to } board ->
            view__
                { maybeAudioUrl = Nothing
                , pairnum = getPairNumFromBoard board
                , gameEnd = False
                , history = historyString
                , currentTimer = currentTimer
                }
                (backgroundWoodenBoard { eyeIsOpen = eyeIsOpen }
                    :: drawArrow Yellow from to
                    :: List.map (displayCard { eyeIsOpen = eyeIsOpen }) board.cards
                    ++ List.map (\c -> candidateGreenSvg { eyeIsOpen = eyeIsOpen } (Hop { from = c, to = board.empty }) c) (possibleHopPosition board)
                )
                [ eyeButton { eyeIsOpen = eyeIsOpen }, simpleCancelButton { eyeIsOpen = eyeIsOpen } ]

        SecondHalfCompleted ({ first_from, first_to, second_from, second_to } as coords) board ->
            let
                isMatching =
                    isMatchFromCoords coords board == Just True
            in
            view__
                { maybeAudioUrl =
                    if isMatching then
                        Just "sound/success.wav"

                    else
                        Just "sound/failure.wav"
                , pairnum =
                    if isMatching then
                        getPairNumFromBoard board

                    else
                        -- two cards are mismatched, so we must subtract it
                        getPairNumFromBoard board - 1
                , gameEnd =
                    False
                , history =
                    historyString
                , currentTimer = currentTimer
                }
                (backgroundWoodenBoard { eyeIsOpen = eyeIsOpen }
                    :: drawArrow Yellow first_from first_to
                    :: drawArrow Green second_from second_to
                    :: List.map (displayCard { eyeIsOpen = eyeIsOpen }) board.cards
                )
                [ eyeButton { eyeIsOpen = eyeIsOpen }
                , if isMatching then
                    matchButton { eyeIsOpen = eyeIsOpen }

                  else
                    mismatchButton { eyeIsOpen = eyeIsOpen }
                ]


isSlide : Coordinate -> Coordinate -> Bool
isSlide a b =
    -- Elm does not allow negative literals in pattern matching
    case ( abs (a.x - b.x), abs (a.y - b.y) ) of
        ( 1, 0 ) ->
            True

        ( 0, 1 ) ->
            True

        _ ->
            False


coordsFlippedInATurn : { first_from : Coordinate, first_to : Coordinate, second_from : Coordinate, second_to : Coordinate } -> { first_flipped : Coordinate, second_flipped : Coordinate }
coordsFlippedInATurn { first_from, first_to, second_from, second_to } =
    { first_flipped =
        if isSlide first_from first_to then
            first_to

        else
            getMidpoint first_from first_to
    , second_flipped = getMidpoint second_from second_to
    }


isMatchFromCoords : { first_from : Coordinate, first_to : Coordinate, second_from : Coordinate, second_to : Coordinate } -> Board -> Maybe Bool
isMatchFromCoords coords board =
    let
        { first_flipped, second_flipped } =
            coordsFlippedInATurn coords
    in
    getCardAt board second_flipped
        |> Maybe.andThen
            (\second_card -> getCardAt board first_flipped |> Maybe.andThen (\first_card -> Just (isMatch first_card second_card)))


isMatch : CardOnBoard -> CardOnBoard -> Bool
isMatch a b =
    if a.cardColor == b.cardColor then
        False
        -- the colors must be different

    else if a.prof == b.prof then
        True

    else if List.member a.prof [ Io, Tam2 ] && List.member b.prof [ Io, Tam2 ] then
        True

    else if List.member a.prof [ Kua2, Tuk2, Uai1 ] && List.member b.prof [ Kua2, Tuk2, Uai1 ] then
        True

    else if List.member a.prof [ Dau2, Maun1 ] && List.member b.prof [ Dau2, Maun1 ] then
        True

    else
        False


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialStatus =
            NothingSelected (initialBoard flags.cards)
    in
    ( Model
        { historyString =
            String.join "," (List.map String.fromInt flags.cards) ++ "\n\n"
        , currentStatus = initialStatus
        , saved = initialStatus
        , eyeIsOpen = False
        , currentTimer = NotStarted
        }
    , Cmd.none
    )


drawArrow : UIColor -> Coordinate -> Coordinate -> Svg msg
drawArrow uiColor from to =
    let
        d_data =
            if from.x == to.x && from.y > to.y then
                -- up arrow
                let
                    delta =
                        toFloat (from.y - to.y)
                in
                "m31.6 "
                    ++ String.fromFloat (51.3 + delta * lattice_size)
                    ++ "h5.8v"
                    ++ String.fromFloat -(34.5 + delta * lattice_size)
                    ++ "l-21.3 31 4.5 3.2 11-16z"

            else if from.x == to.x && from.y < to.y then
                -- down arrow
                let
                    delta =
                        toFloat (to.y - from.y)
                in
                "m31.6 18.7h5.8v" ++ String.fromFloat (34.5 + delta * lattice_size) ++ "l-21.3-31 4.5-3.2 11 16z"

            else if from.y == to.y && from.x > to.x then
                -- left arrow
                let
                    delta =
                        toFloat (from.x - to.x)
                in
                "m"
                    ++ String.fromFloat (51.3 + delta * lattice_size)
                    ++ " 31.6v5.8h"
                    ++ String.fromFloat -(34.5 + delta * lattice_size)
                    ++ "l31-21.3 3.2 4.5-16 11z"

            else if from.y == to.y && from.x < to.x then
                -- right arrow
                let
                    delta =
                        toFloat (to.x - from.x)
                in
                "m18.7 31.6v5.8h" ++ String.fromFloat (34.5 + delta * lattice_size) ++ "l-31-21.3-3.2 4.5 16 11z"

            else
                ""

        top_left =
            { y = min from.y to.y, x = min from.x to.x }
    in
    g
        [ transform ("translate(" ++ String.fromFloat (toFloat top_left.x * lattice_size) ++ "," ++ String.fromFloat (toFloat top_left.y * lattice_size) ++ ")")
        ]
        [ path [ d d_data, fill (fromUIColor uiColor), stroke "#000", strokeWidth "2" ] [] ]


isStuck : Board -> Bool
isStuck board =
    if possibleHopPosition board ++ possibleSlidePosition board |> isEmpty then
        -- The first move cannot be taken; hence stuck
        True

    else
        let
            hops =
                possibleHopPosition board |> List.map (applyHop board)

            slides =
                possibleSlidePosition board |> List.map (applySlide board)
        in
        (hops ++ slides) |> List.all (\b -> possibleHopPosition b |> isEmpty)


applyHop : Board -> Coordinate -> Board
applyHop oldBoard from =
    let
        to =
            oldBoard.empty

        ( cardsToBeMoved, remainingCards ) =
            List.partition (\x -> x.coord == from) oldBoard.cards

        ( cardsToBeFlipped, remainingCards2 ) =
            List.partition (\x -> x.coord == getMidpoint from to) remainingCards
    in
    case ( cardsToBeMoved, cardsToBeFlipped ) of
        ( [ moved ], [ flipped ] ) ->
            { empty = from, cards = { moved | coord = to } :: { flipped | shown = True } :: remainingCards2 }

        _ ->
            -- this path is not taken
            oldBoard


applySlide : Board -> Coordinate -> Board
applySlide oldBoard from =
    let
        to =
            oldBoard.empty

        ( cardsToBeMoved, remainingCards ) =
            List.partition (\x -> x.coord == from) oldBoard.cards
    in
    case cardsToBeMoved of
        [ cardToBeMoved ] ->
            { empty = from, cards = { cardToBeMoved | coord = to, shown = True } :: remainingCards }

        _ ->
            -- this path is not taken
            oldBoard


updateStatus : TimerStatus -> Msg -> CurrentStatus -> CurrentStatus -> { newStatus : CurrentStatus, additionToHistory : String }
updateStatus currentTimer msg modl saved =
    case ( modl, msg ) of
        ( SecondHalfCompleted _ _, Cancel ) ->
            -- this shall not be canceled
            { additionToHistory = "", newStatus = modl }

        ( _, Cancel ) ->
            -- otherwise, abort it and revert to what was saved last
            { additionToHistory = "", newStatus = saved }

        ( NothingSelected oldBoard, Hop { from, to } ) ->
            { additionToHistory = "", newStatus = FirstHalfCompletedByHop { from = from, to = to } (applyHop oldBoard from) }

        ( NothingSelected oldBoard, Slide { from, to } ) ->
            { additionToHistory = "", newStatus = FirstHalfCompletedBySlide { from = from, to = to } (applySlide oldBoard from) }

        ( FirstHalfCompletedByHop first_fromto oldBoard, Hop { from, to } ) ->
            { additionToHistory = ""
            , newStatus =
                SecondHalfCompleted
                    { first_from = first_fromto.from
                    , first_to = first_fromto.to
                    , second_from = from
                    , second_to = to
                    }
                    (applyHop oldBoard from)
            }

        ( FirstHalfCompletedBySlide first_fromto oldBoard, Hop { from, to } ) ->
            { additionToHistory = ""
            , newStatus =
                SecondHalfCompleted
                    { first_from = first_fromto.from
                    , first_to = first_fromto.to
                    , second_from = from
                    , second_to = to
                    }
                    (applyHop oldBoard from)
            }

        ( SecondHalfCompleted coords board, Match ) ->
            { additionToHistory =
                toHistory coords
                    ++ (if isStuck board then
                            " Match&Stuck!\n"

                        else
                            " Match!\n"
                       )
                    ++ "pair: "
                    ++ String.fromInt (getPairNumFromBoard board)
                    ++ ", time: "
                    ++ stringFromTimer currentTimer
                    ++ "s\n"
            , newStatus = NothingSelected board
            }

        ( SecondHalfCompleted coords oldBoard, Mismatch ) ->
            let
                { first_flipped, second_flipped } =
                    coordsFlippedInATurn coords

                ( cardsToBeFlippedBack, remainingCards ) =
                    List.partition (\x -> List.member x.coord [ first_flipped, second_flipped ]) oldBoard.cards

                newBoard =
                    { oldBoard | cards = List.map (\card -> { card | shown = False }) cardsToBeFlippedBack ++ remainingCards }
            in
            { additionToHistory =
                if isStuck newBoard then
                    toHistory coords
                        ++ " Stuck!\n"
                        ++ "pair: "
                        ++ String.fromInt (getPairNumFromBoard newBoard)
                        ++ ", time: "
                        ++ stringFromTimer currentTimer
                        ++ "s\n"

                else
                    toHistory coords ++ ", "
            , newStatus = NothingSelected newBoard
            }

        _ ->
            { additionToHistory = "", newStatus = modl }


toHistory : { first_from : Coordinate, first_to : Coordinate, second_from : Coordinate, second_to : Coordinate } -> String
toHistory a =
    let
        toStr u =
            String.fromInt (1 + u.x) ++ String.fromInt (1 + u.y)
    in
    toStr a.first_from ++ ";" ++ toStr a.second_from


toCardOnBoard : Int -> CardEncodedAsInt -> CardOnBoard
toCardOnBoard index card =
    { coord = { x = remainderBy 7 index, y = index // 7 }
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


initialBoard : List CardEncodedAsInt -> Board
initialBoard cards =
    { -- the former half has indices 0 to 23
      -- the latter half has indices 25 to 48
      cards =
        List.indexedMap toCardOnBoard (List.take 24 cards)
            ++ List.indexedMap (\i card -> toCardOnBoard (i + 25) card) (List.drop 24 cards)
    , empty = { x = 3, y = 3 }
    }
