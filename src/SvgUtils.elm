module SvgUtils exposing (..)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOut, onMouseOver)


type alias CartesianPoint =
    { x : Float
    , y : Float
    }


buildPoint : ( Float, Float ) -> CartesianPoint
buildPoint ( x, y ) =
    CartesianPoint x y


pointStr : CartesianPoint -> List String
pointStr point =
    [ String.fromFloat point.x
    , String.fromFloat point.y
    ]


type Orientation
    = CW
    | CCW


type alias ArcCirc =
    { end : CartesianPoint
    , radius : Float
    , orientation : Orientation
    , largerThanPi : Bool
    }


type Path
    = Move CartesianPoint
    | Line CartesianPoint
    | Arc ArcCirc
    | ClosePath


pathStr : Path -> String
pathStr path =
    case path of
        Move point ->
            String.join " " <| "M" :: pointStr point

        Line point ->
            String.join " " <| "L" :: pointStr point

        Arc { end, radius, orientation, largerThanPi } ->
            let
                largeArcFlag =
                    if largerThanPi then
                        1

                    else
                        0

                sweepFlag =
                    case orientation of
                        CW ->
                            1

                        CCW ->
                            0
            in
            String.join " " <| "A" :: ([ radius, radius, 0, largeArcFlag, sweepFlag, end.x, end.y ] |> List.map String.fromFloat)

        ClosePath ->
            "Z"


type alias CircSection =
    { start : Float
    , angleLength : Float
    , outerRadius : Float
    , innerRadius : Float
    }


circSectionPath : CircSection -> Svg.Attribute msg
circSectionPath { start, angleLength, outerRadius, innerRadius } =
    let
        end =
            start + angleLength

        firstPoint =
            fromPolar ( innerRadius, start ) |> buildPoint

        secondPoint =
            fromPolar ( outerRadius, start ) |> buildPoint

        thirdPoint =
            fromPolar ( outerRadius, end ) |> buildPoint

        fourthPoint =
            fromPolar ( innerRadius, end ) |> buildPoint

        largerThanPi =
            end - start > pi
    in
    [ Move firstPoint
    , Line secondPoint
    , Arc <| ArcCirc thirdPoint outerRadius CW largerThanPi
    , Line fourthPoint
    , Arc <| ArcCirc firstPoint innerRadius CCW largerThanPi
    ]
        |> List.map pathStr
        |> String.join " "
        |> d


tau : Float
tau =
    2 * pi


type alias Circle =
    { x : Float
    , y : Float
    , r : Float
    }


unitCirc : Circle
unitCirc =
    Circle 0 0 1
