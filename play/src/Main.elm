module Main exposing (init, main, view)

-- import KeseRimaSvgColor exposing (..)
-- import KeseRimaSvgElements exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (href)
import SvgElements exposing (pieceSvg__)
import Svg exposing (Attribute, Svg, animate, defs, feGaussianBlur, g, rect, svg, text)
import Svg.Attributes exposing (attributeName, dur, fill, height, id, repeatCount, result, stdDeviation, stroke, strokeWidth, transform, values, viewBox, width, x, y)
import Svg.Events exposing (onClick)
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
                    , fill "#000000"
                    , stroke "#000000"
                    , strokeWidth "4"
                    ]
                    []
            )
            allCoord
        )
    ]


cardSvgOnGrid : Bool -> OriginalMsg -> CardOnBoard -> Svg OriginalMsg
cardSvgOnGrid focused msg { coord, prof, cardColor } =
    pieceSvg focused msg { coord = { x = coord.x, y = coord.y }, prof = prof, cardColor = cardColor, shown = False }


pieceSvg : Bool -> OriginalMsg -> CardOnBoard -> Svg OriginalMsg
pieceSvg focused msgToBeSent p =
    let
        strok =
            if focused then
                { color = "#000000"
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


pieceSvg_ : { a | color : String, width : String } -> OriginalMsg -> CardOnBoard -> Svg OriginalMsg
pieceSvg_ =
    pieceSvg__ msgToIcon



{-
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


   stationaryPart : Board -> List (Svg OriginalMsg)
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


-}


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


f : Coordinate -> Svg msg
f coord =
    let
        parity =
            modBy 2 (coord.x + coord.y)

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
            coord.x * lattice_size

        y_coord_mid =
            coord.y * lattice_size
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
        [ rect [ x "0", y "0", width width_text, height height_text, fill "#000000", stroke "none", strokeWidth "none" ] [] ]


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


nthNeighbor : Int -> Coordinate -> List Coordinate
nthNeighbor n coord =
    [ { coord | x = coord.x - n }
    , { coord | y = coord.y - n }
    , { coord | x = coord.x + n }
    , { coord | y = coord.y + n }
    ]
        |> List.filter (\c -> c.x >= 0 && c.x < 7 && c.y >= 0 && c.y < 7)


view : Model -> Html OriginalMsg
view (Model { historyString, currentStatus }) =
    case currentStatus of
        NothingSelected board ->
            view_ False
                historyString
                (rect [ fill "#e0c39d", x "-100", y "-100", width "1050", height "1050" ] []
                    :: List.map (\c -> f c.coord) board.cards
                    ++ List.map (\c -> candidateYellowSvg (Slide { from = c, to = board.empty }) c) (nthNeighbor 1 board.empty)
                    ++ List.map (\c -> candidateYellowSvg (Hop { from = c, to = board.empty }) c) (nthNeighbor 2 board.empty)
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
                    ++ List.map (cardSvgOnGrid False None) cardState.board
                )
                [{- The game has ended. No cancelling allowed. -}]

        FirstHalfCompletedByHop { from, to } cardState ->
            let
                dynamicPart =
                    List.map (cardSvgOnGrid False None) cardState.cards
            in
            view_ False
                historyString
                dynamicPart
                [ simpleCancelButton ]

        FirstHalfCompletedBySlide { from, to } cardState ->
            let
                dynamicPart =
                    List.map (cardSvgOnGrid False None) cardState.cards
            in
            view_ False
                historyString
                dynamicPart
                [ simpleCancelButton ]

        NowWaitingForAdditionalSacrifice { mover, remaining } ->
            view_ False
                historyString
                (List.map
                    (cardSvgOnGrid False None {- You cannot click any piece on the board while waiting for additional sacrifices. -})
                    remaining.cards
                )
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
