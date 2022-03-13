module Main exposing (init, main, view)

-- import List.Extra exposing (filterNot)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (href)
import Svg exposing (Attribute, Svg, animate, defs, feGaussianBlur, g, path, rect, svg, text)
import Svg.Attributes exposing (attributeName, d, dur, fill, height, id, repeatCount, result, stdDeviation, stroke, strokeWidth, transform, values, viewBox, width, x, y)
import Svg.Events exposing (onClick)
import SvgElements exposing (pieceSvg__)
import Tak1Bai2Types exposing (..)
import Url.Builder exposing (crossOrigin)


type alias Flags =
    { cards : List CardEncodedAsInt }


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
    case ( modl, msg ) of
        _ ->
            {- Do nothing -}
            ""


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
            (svg [ viewBox "-100 -200 1050 1150", width "540" ] svgContent
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


shortEdgeHalf : number
shortEdgeHalf =
    21


longEdgeHalf : number
longEdgeHalf =
    80


spacing : number
spacing =
    40


lattice_size : number
lattice_size =
    shortEdgeHalf + longEdgeHalf + spacing


displayCard : CardOnBoard -> Svg msg
displayCard c =
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
            String.fromInt (widthHalf * 2)

        height_text =
            String.fromInt (heightHalf * 2)

        x_coord_mid =
            c.coord.x * lattice_size

        y_coord_mid =
            c.coord.y * lattice_size
    in
    g
        [ transform
            ("translate("
                ++ String.fromInt (x_coord_mid - widthHalf)
                ++ " "
                ++ String.fromInt (y_coord_mid - heightHalf)
                ++ ")"
            )
        ]
        [ rect
            [ x "0"
            , y "0"
            , width width_text
            , height height_text
            , fill
                (if c.shown then
                    "#ffffff"

                 else
                    "#000000"
                )
            , stroke "none"
            , strokeWidth "none"
            ]
            []
        ]


candidateYellowSvg : msg -> Coordinate -> Svg msg
candidateYellowSvg msgToBeSent coord =
    g
        [ transform ("translate(" ++ String.fromInt (coord.x * lattice_size) ++ " " ++ String.fromInt (coord.y * lattice_size) ++ ")")
        , Svg.Events.onClick msgToBeSent
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ Svg.circle [ Svg.Attributes.cx "0", Svg.Attributes.cy "0", Svg.Attributes.r "25", fill "#ffff00" ]
            [ animate [ attributeName "opacity", dur "1.62s", values "0;1;0", repeatCount "indefinite" ] []
            ]
        ]


candidateGreenSvg : msg -> Coordinate -> Svg msg
candidateGreenSvg msgToBeSent coord =
    g
        [ transform ("translate(" ++ String.fromInt (coord.x * lattice_size) ++ " " ++ String.fromInt (coord.y * lattice_size) ++ ")")
        , Svg.Events.onClick msgToBeSent
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ Svg.rect [ Svg.Attributes.transform "rotate(45)", Svg.Attributes.x "-23", Svg.Attributes.y "-23", Svg.Attributes.width "46", Svg.Attributes.height "46", fill "#aeff01" ]
            [ animate [ attributeName "opacity", dur "1.62s", values "0;1;0", repeatCount "indefinite" ] []
            ]
        ]


nthNeighbor : Int -> Coordinate -> List Coordinate
nthNeighbor n coord =
    [ { coord | x = coord.x - n }
    , { coord | y = coord.y - n }
    , { coord | x = coord.x + n }
    , { coord | y = coord.y + n }
    ]
        |> List.filter (\c -> c.x >= 0 && c.x < 7 && c.y >= 0 && c.y < 7)


backgroundWoodenBoard : Svg msg
backgroundWoodenBoard =
    rect [ fill "#e0c39d", x "-100", y "-100", width "1050", height "1050" ] []


possibleSlidePosition : Board -> List Coordinate
possibleSlidePosition board =
    nthNeighbor 1 board.empty
        -- A card cannot slide if it is already shown
        |> List.filter (\coord -> isShownAt board coord /= Just True)


possibleHopPosition : Board -> List Coordinate
possibleHopPosition board =
    nthNeighbor 2 board.empty
        |> List.filter (\neighbor -> isShownAt board (getMidPoint neighbor board.empty) /= Just True)


getCardAt : Board -> Coordinate -> Maybe CardOnBoard
getCardAt board coord =
    List.filter (\c -> c.coord == coord) board.cards |> List.head


isShownAt : Board -> Coordinate -> Maybe Bool
isShownAt board coord =
    getCardAt board coord |> Maybe.map .shown


view : Model -> Html OriginalMsg
view (Model { historyString, currentStatus }) =
    case currentStatus of
        NothingSelected board ->
            view_ False
                historyString
                (backgroundWoodenBoard
                    :: List.map displayCard board.cards
                    ++ List.map (\c -> candidateYellowSvg (Slide { from = c, to = board.empty }) c) (possibleSlidePosition board)
                    ++ List.map (\c -> candidateYellowSvg (Hop { from = c, to = board.empty }) c) (possibleHopPosition board)
                )
                [{- default state. no need to cancel everything: the state has been saved -}]

        GameTerminated board ->
            view_ False
                historyString
                (backgroundWoodenBoard
                    :: List.map displayCard board.board
                )
                [{- The game has ended. No cancelling allowed. -}]

        FirstHalfCompletedByHop { from, to } board ->
            view_ False
                historyString
                (backgroundWoodenBoard
                    :: drawArrow from to
                    :: List.map displayCard board.cards
                    ++ List.map (\c -> candidateGreenSvg (Hop { from = c, to = board.empty }) c) (possibleHopPosition board)
                )
                [ simpleCancelButton ]

        FirstHalfCompletedBySlide { from, to } board ->
            view_ False
                historyString
                (backgroundWoodenBoard
                    :: drawArrow from to
                    :: List.map displayCard board.cards
                    ++ List.map (\c -> candidateGreenSvg (Hop { from = c, to = board.empty }) c) (possibleHopPosition board)
                )
                [ simpleCancelButton ]

        NowWaitingForAdditionalSacrifice { mover, remaining } ->
            view_ False
                historyString
                []
                (case List.filter (\p -> p.coord == mover.coord) remaining.cards of
                    [] ->
                        [ cancelAllButton ]

                    _ ->
                        {- The resulting square is empty, so it is always possible to declare TurnEnd -}
                        [ turnEndButton, cancelAllButton ]
                )


cancelAllButton : Html OriginalMsg
cancelAllButton =
    Html.button [ onClick Cancel, Html.Attributes.style "background-color" "#ffaaaa", Html.Attributes.style "font-size" "150%" ] [ text "全てをキャンセル" ]


simpleCancelButton : Html OriginalMsg
simpleCancelButton =
    Html.button [ onClick Cancel, Html.Attributes.style "background-color" "#ffaaaa", Html.Attributes.style "font-size" "150%" ] [ text "キャンセル" ]


turnEndButton : Html OriginalMsg
turnEndButton =
    Html.button [ onClick TurnEnd, Html.Attributes.style "background-color" "#aaffaa", Html.Attributes.style "font-size" "150%" ] [ text "ターンエンド" ]


init : Flags -> ( Model, Cmd OriginalMsg )
init flags =
    let
        initialStatus =
            NothingSelected (initialBoard flags.cards)
    in
    ( Model
        { historyString =
            ""
        , currentStatus = initialStatus
        , saved = initialStatus
        }
    , Cmd.none
    )


drawArrow : Coordinate -> Coordinate -> Svg msg
drawArrow from to =
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
        [ transform ("translate(" ++ String.fromInt (top_left.x * lattice_size) ++ "," ++ String.fromInt (top_left.y * lattice_size) ++ ")")
        ]
        [ path [ d d_data, fill "#aeff01", stroke "#000", strokeWidth "2" ] [] ]
