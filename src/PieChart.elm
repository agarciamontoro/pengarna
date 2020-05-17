module PieChart exposing (Msg, pie)

import Account exposing (SimpleAccount)
import Array exposing (Array)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOver)


type Msg
    = PieHover


sum : List SimpleAccount -> Int
sum accounts =
    List.map .balance accounts |> List.sum


tau : Float
tau =
    2 * pi


type alias Circle =
    { x : Float
    , y : Float
    , r : Float
    , width : Float
    }


unitCirc : Circle
unitCirc =
    Circle 0 0 1 0.5


viewBoxStr : Circle -> String
viewBoxStr circle =
    let
        minX =
            (circle.x - circle.r) - (circle.width / 2)

        minY =
            (circle.y - circle.r) - (circle.width / 2)

        width =
            (circle.r * 2) + circle.width

        height =
            (circle.r * 2) + circle.width
    in
    String.join " "
        [ String.fromFloat minX
        , String.fromFloat minY
        , String.fromFloat width
        , String.fromFloat height
        ]


length : Circle -> Float -> Float -> Float
length circle total amount =
    amount * tau * circle.r / total


type alias Stroke =
    { length : Float
    , offset : Float
    }


dashArray : Circle -> Stroke -> String
dashArray circle stroke =
    String.join " "
        [ String.fromFloat stroke.length
        , String.fromFloat <| tau * circle.r - stroke.length
        ]


dashOffset : Circle -> Stroke -> String
dashOffset _ stroke =
    String.fromFloat (tau - stroke.offset)


mapToStroke : Circle -> List SimpleAccount -> List Stroke
mapToStroke circle accounts =
    let
        totalAmount =
            sum accounts

        f account ( list, offset ) =
            let
                currLength =
                    length circle (toFloat totalAmount) (toFloat account.balance)
            in
            ( Stroke currLength offset :: list, offset + currLength )
    in
    List.sortBy .balance accounts
        |> List.foldr f ( [], tau - tau / 4 )
        |> Tuple.first


colors : Array String
colors =
    Array.fromList [ "red", "blue", "green", "pink" ]


genericPie : Circle -> List SimpleAccount -> Svg Msg
genericPie circData accounts =
    let
        f : Int -> Stroke -> Svg Msg
        f idx elem =
            circle
                [ cx <| String.fromFloat circData.x
                , cy <| String.fromFloat circData.y
                , r <| String.fromFloat circData.r
                , fill "transparent"
                , stroke <| Maybe.withDefault "red" <| Array.get (modBy 4 idx) colors
                , strokeWidth <| String.fromFloat circData.width
                , strokeDasharray <| dashArray circData elem
                , strokeDashoffset <| dashOffset circData elem
                , onMouseOver PieHover
                ]
                []

        arcs =
            List.indexedMap f <| mapToStroke circData accounts
    in
    svg
        [ width "100%"
        , height "100%"
        , viewBox <| viewBoxStr circData
        ]
    <|
        circle
            [ cx <| String.fromFloat circData.x
            , cy <| String.fromFloat circData.y
            , r <| String.fromFloat circData.r
            , fill "#fff"
            ]
            []
            :: arcs


pie : List SimpleAccount -> Svg Msg
pie =
    genericPie unitCirc
