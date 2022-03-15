module Main exposing (init, main, view)

import Browser
import Buttons exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (href)
import Sizes exposing (..)
import Svg exposing (Attribute, Svg, g, path, rect, svg)
import Svg.Attributes exposing (d, fill, height, stroke, strokeWidth, transform, viewBox, width, x, y)
import Tak1Bai2Types exposing (..)
import Url.Builder exposing (crossOrigin)


type alias Flags =
    { cards : List CardEncodedAsInt }


type Model
    = Model
        { saved : CurrentStatus -- Reverts to here when canceled
        , historyString : HistoryString
        , currentStatus : CurrentStatus
        , eyeIsOpen : Bool
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
update msg ((Model { historyString, currentStatus, saved, eyeIsOpen }) as modl) =
    if eyeIsOpen then
        -- the only thing you can do is to close the eye
        if msg == CloseTheEye then
            ( Model { historyString = historyString, currentStatus = currentStatus, saved = saved, eyeIsOpen = False }, Cmd.none )

        else
            ( modl, Cmd.none )

    else if msg == OpenTheEye then
        ( Model { historyString = historyString, currentStatus = currentStatus, saved = saved, eyeIsOpen = True }, Cmd.none )

    else
        let
            { newStatus, additionToHistory } =
                updateStatus msg currentStatus saved

            newHist =
                historyString ++ additionToHistory
        in
        case newStatus of
            NothingSelected cardState ->
                ( Model
                    { historyString = newHist
                    , currentStatus = newStatus
                    , saved = NothingSelected cardState -- update `saved`
                    , eyeIsOpen = False
                    }
                , Cmd.none
                )

            _ ->
                ( Model { historyString = newHist, currentStatus = newStatus, saved = saved, eyeIsOpen = False }, Cmd.none )


newHistory : OriginalMsg -> CurrentStatus -> String
newHistory msg modl =
    case ( modl, msg ) of
        _ ->
            {- Do nothing -}
            ""


targetBlankLink : List (Attribute msg) -> List (Html msg) -> Html msg
targetBlankLink attributes =
    Html.a (Html.Attributes.target "_blank" :: attributes)


cardHtmlImage : { a | prof : Profession, cardColor : CardColor } -> Html msg
cardHtmlImage a =
    Html.img [ Html.Attributes.src (toExternalSvgFilePath a), Html.Attributes.height 100, Html.Attributes.style "vertical-align" "middle" ] []


view_ : Int -> Bool -> HistoryString -> List (Svg msg) -> List (Html msg) -> Html msg
view_ pairnum gameEndTweet history svgContent buttons =
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
            , Html.p [ Html.Attributes.style "font-size" "80%" ]
                [ Html.text "カードをめくって、同一札の黒と赤でペアを作っていく遊びです。" ]
            , Html.p [ Html.Attributes.style "font-size" "80%" ]
                [ Html.text "ただし "
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
            ]
        , Html.div []
            (Html.div
                [ Html.Attributes.style "min-height" "35px"
                , Html.Attributes.style "margin-top" "25px"
                , Html.Attributes.style "text-align" "center"
                ]
                [ Html.span [] [ Html.text ("現在のペア数: " ++ String.fromInt pairnum) ]
                ]
                :: svg [ viewBox "-100 -100 1050 1050", width "540" ] svgContent
                :: Html.br [] []
                :: List.intersperse (Html.text " ") buttons
            )
        , Html.div [ Html.Attributes.style "margin-left" "15px" ]
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


view : Model -> Html OriginalMsg
view (Model { historyString, currentStatus, eyeIsOpen }) =
    case currentStatus of
        NothingSelected board ->
            view_ (getPairNumFromBoard board)
                False
                historyString
                (backgroundWoodenBoard { eyeIsOpen = eyeIsOpen }
                    :: List.map (displayCard { eyeIsOpen = eyeIsOpen }) board.cards
                    ++ List.map (\c -> candidateYellowSvg { eyeIsOpen = eyeIsOpen } (Slide { from = c, to = board.empty }) c) (possibleSlidePosition board)
                    ++ List.map (\c -> candidateYellowSvg { eyeIsOpen = eyeIsOpen } (Hop { from = c, to = board.empty }) c) (possibleHopPosition board)
                )
                [ eyeButton { eyeIsOpen = eyeIsOpen } ]

        GameTerminated board ->
            view_ (getPairNumFromBoard board)
                False
                historyString
                (backgroundWoodenBoard { eyeIsOpen = eyeIsOpen }
                    :: List.map (displayCard { eyeIsOpen = eyeIsOpen }) board.cards
                )
                [ eyeButton { eyeIsOpen = eyeIsOpen } ]

        FirstHalfCompletedByHop { from, to } board ->
            view_ (getPairNumFromBoard board)
                False
                historyString
                (backgroundWoodenBoard { eyeIsOpen = eyeIsOpen }
                    :: drawArrow Yellow from to
                    :: List.map (displayCard { eyeIsOpen = eyeIsOpen }) board.cards
                    ++ List.map (\c -> candidateGreenSvg { eyeIsOpen = eyeIsOpen } (Hop { from = c, to = board.empty }) c) (possibleHopPosition board)
                )
                [ eyeButton { eyeIsOpen = eyeIsOpen }, simpleCancelButton { eyeIsOpen = eyeIsOpen } ]

        FirstHalfCompletedBySlide { from, to } board ->
            view_ (getPairNumFromBoard board)
                False
                historyString
                (backgroundWoodenBoard { eyeIsOpen = eyeIsOpen }
                    :: drawArrow Yellow from to
                    :: List.map (displayCard { eyeIsOpen = eyeIsOpen }) board.cards
                    ++ List.map (\c -> candidateGreenSvg { eyeIsOpen = eyeIsOpen } (Hop { from = c, to = board.empty }) c) (possibleHopPosition board)
                )
                [ eyeButton { eyeIsOpen = eyeIsOpen }, simpleCancelButton { eyeIsOpen = eyeIsOpen } ]

        SecondHalfCompleted ({ first_from, first_to, second_from, second_to } as coords) board ->
            view_
                (if isMatchFromCoords coords board == Just True then
                    getPairNumFromBoard board

                 else
                    getPairNumFromBoard board - 1
                 -- two cards are mismatched, so we must subtract it
                )
                False
                historyString
                (backgroundWoodenBoard { eyeIsOpen = eyeIsOpen }
                    :: drawArrow Yellow first_from first_to
                    :: drawArrow Green second_from second_to
                    :: List.map (displayCard { eyeIsOpen = eyeIsOpen }) board.cards
                )
                [ eyeButton { eyeIsOpen = eyeIsOpen }
                , if isMatchFromCoords coords board == Just True then
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


init : Flags -> ( Model, Cmd OriginalMsg )
init flags =
    let
        initialStatus =
            NothingSelected (initialBoard flags.cards)
    in
    ( Model
        { historyString =
            "初期配置: " ++ String.join "," (List.map String.fromInt flags.cards) ++ "\n\n"
        , currentStatus = initialStatus
        , saved = initialStatus
        , eyeIsOpen = False
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


updateStatus : OriginalMsg -> CurrentStatus -> CurrentStatus -> { newStatus : CurrentStatus, additionToHistory : String }
updateStatus msg modl saved =
    case ( modl, msg ) of
        ( _, Cancel ) ->
            -- no matter what the state is, abort it and revert to what was saved last
            { additionToHistory = "", newStatus = saved }

        ( NothingSelected oldBoard, Hop { from, to } ) ->
            let
                ( cardsToBeMoved, remainingCards ) =
                    List.partition (\x -> x.coord == from) oldBoard.cards

                ( cardsToBeFlipped, remainingCards2 ) =
                    List.partition (\x -> x.coord == getMidpoint from to) remainingCards

                newBoard =
                    case ( cardsToBeMoved, cardsToBeFlipped ) of
                        ( [ moved ], [ flipped ] ) ->
                            { empty = from, cards = { moved | coord = to } :: { flipped | shown = True } :: remainingCards2 }

                        _ ->
                            -- this path is not taken
                            oldBoard
            in
            { additionToHistory = "", newStatus = FirstHalfCompletedByHop { from = from, to = to } newBoard }

        ( NothingSelected oldBoard, Slide { from, to } ) ->
            let
                ( cardsToBeMoved, remainingCards ) =
                    List.partition (\x -> x.coord == from) oldBoard.cards

                newBoard =
                    case cardsToBeMoved of
                        [ cardToBeMoved ] ->
                            { empty = from, cards = { cardToBeMoved | coord = to, shown = True } :: remainingCards }

                        _ ->
                            -- this path is not taken
                            oldBoard
            in
            { additionToHistory = "", newStatus = FirstHalfCompletedBySlide { from = from, to = to } newBoard }

        ( FirstHalfCompletedByHop first_fromto oldBoard, Hop { from, to } ) ->
            let
                ( cardsToBeMoved, remainingCards ) =
                    List.partition (\x -> x.coord == from) oldBoard.cards

                ( cardsToBeFlipped, remainingCards2 ) =
                    List.partition (\x -> x.coord == getMidpoint from to) remainingCards

                newBoard =
                    case ( cardsToBeMoved, cardsToBeFlipped ) of
                        ( [ moved ], [ flipped ] ) ->
                            { empty = from, cards = { moved | coord = to } :: { flipped | shown = True } :: remainingCards2 }

                        _ ->
                            -- this path is not taken
                            oldBoard
            in
            { additionToHistory = ""
            , newStatus =
                SecondHalfCompleted
                    { first_from = first_fromto.from
                    , first_to = first_fromto.to
                    , second_from = from
                    , second_to = to
                    }
                    newBoard
            }

        ( FirstHalfCompletedBySlide first_fromto oldBoard, Hop { from, to } ) ->
            let
                ( cardsToBeMoved, remainingCards ) =
                    List.partition (\x -> x.coord == from) oldBoard.cards

                ( cardsToBeFlipped, remainingCards2 ) =
                    List.partition (\x -> x.coord == getMidpoint from to) remainingCards

                newBoard =
                    case ( cardsToBeMoved, cardsToBeFlipped ) of
                        ( [ moved ], [ flipped ] ) ->
                            { empty = from, cards = { moved | coord = to } :: { flipped | shown = True } :: remainingCards2 }

                        _ ->
                            -- this path is not taken
                            oldBoard
            in
            { additionToHistory = ""
            , newStatus =
                SecondHalfCompleted
                    { first_from = first_fromto.from
                    , first_to = first_fromto.to
                    , second_from = from
                    , second_to = to
                    }
                    newBoard
            }

        ( SecondHalfCompleted coords oldBoard, Match ) ->
            { additionToHistory = toHistory coords ++ " Match!\n\n", newStatus = NothingSelected oldBoard }

        ( SecondHalfCompleted coords oldBoard, Mismatch ) ->
            let
                { first_flipped, second_flipped } =
                    coordsFlippedInATurn coords

                ( cardsToBeFlippedBack, remainingCards ) =
                    List.partition (\x -> List.member x.coord [ first_flipped, second_flipped ]) oldBoard.cards

                newBoard =
                    { oldBoard | cards = List.map (\card -> { card | shown = False }) cardsToBeFlippedBack ++ remainingCards }
            in
            { additionToHistory = toHistory coords ++ ", ", newStatus = NothingSelected newBoard }

        _ ->
            { additionToHistory = "", newStatus = modl }


toHistory : { first_from : Coordinate, first_to : Coordinate, second_from : Coordinate, second_to : Coordinate } -> String
toHistory a =
    let
        toStr u =
            String.fromInt (1 + u.x) ++ String.fromInt (1 + u.y)
    in
    toStr a.first_from ++ ";" ++ toStr a.second_from


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
