module PieChart exposing (Model, Msg, getModel, update, view)

import Account exposing (SimpleAccount)
import Balance
import Bulma.Classes as Bulma
import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOut, onMouseOver)
import Svg.Keyed as SvgKeyed


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



-- In Polar coordinates


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


type alias Model =
    { total : Int
    , accounts : Dict String Section
    }


type alias Section =
    { account : SimpleAccount
    , color : String
    , hovered : Bool
    , selected : Bool
    }


{-| Maps every top level account under expenses: to a color
-}
nameToColor : Dict String String
nameToColor =
    Dict.fromList
        [ ( "casa", "crimson" )
        , ( "comida", "green" )
        , ( "autónomos", "brown" )
        , ( "ropa", "orange" )
        , ( "salud", "pink" )
        , ( "baño", "blue" )
        , ( "cocina", "coral" )
        , ( "regalos", "yellow" )
        , ( "detergente", "purple" )
        , ( "suscripciones", "grey" )
        , ( "velas", "beige" )
        , ( "donaciones", "azure" )
        , ( "bolsas", "aquamarine" )
        ]


colorFromName : String -> String
colorFromName account =
    Dict.get (String.toLower account) nameToColor
        |> Maybe.withDefault "black"


getModel : List SimpleAccount -> Model
getModel accounts =
    let
        total =
            List.map .balance accounts |> List.sum

        toDict : SimpleAccount -> Maybe ( String, Section )
        toDict account =
            case account.name of
                "expenses" ->
                    Nothing

                _ ->
                    let
                        cleanName =
                            String.dropLeft (String.length "expenses:") account.name
                    in
                    Just ( account.name, Section account (colorFromName cleanName) False False )
    in
    List.filterMap toDict accounts |> Dict.fromList |> Model total


type Msg
    = OnMouseOver String
    | OnMouseOut String


sum : List Section -> Int
sum sections =
    List.map (.account >> .balance) sections |> List.sum


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


viewBoxStr : String
viewBoxStr =
    let
        minX =
            unitCirc.x - unitCirc.r

        minY =
            unitCirc.y - unitCirc.r

        width =
            unitCirc.r * 2

        height =
            unitCirc.r * 2
    in
    String.join " "
        [ String.fromFloat minX
        , String.fromFloat minY
        , String.fromFloat width
        , String.fromFloat height
        ]


length : Float -> Float -> Float
length total amount =
    amount * tau * unitCirc.r / total


type alias Stroke =
    { length : Float
    , offset : Float
    }


mapToStroke : Model -> List ( Section, Stroke )
mapToStroke model =
    let
        sectionList =
            Dict.values model.accounts |> List.sortBy (.account >> .balance)

        totalAmount =
            toFloat <| sum sectionList

        f section ( list, offset ) =
            let
                currLength =
                    length totalAmount (toFloat section.account.balance)
            in
            ( ( section, Stroke currLength offset ) :: list, offset + currLength )
    in
    List.foldr f ( [], -pi / 2 ) sectionList
        |> Tuple.first


view : Model -> Svg Msg
view model =
    let
        strokeColor section =
            if section.hovered then
                "gold"

            else
                section.color

        f : ( Section, Stroke ) -> Svg Msg
        f ( section, sectionStroke ) =
            Svg.path
                [ circSectionPath <| CircSection sectionStroke.offset sectionStroke.length 1 0.8
                , fill <| strokeColor section
                , onMouseOver <| OnMouseOver section.account.name
                , onMouseOut <| OnMouseOut section.account.name
                ]
                []

        text =
            Svg.text_
                [ x "0"
                , y "0"
                , fontSize "0.1"
                , dominantBaseline "middle"
                , textAnchor "middle"
                ]
                [ Svg.text <| Balance.format model.total
                ]
    in
    Svg.svg
        [ width "100%"
        , height "100%"
        , viewBox viewBoxStr
        ]
        (text :: List.map f (mapToStroke model))


update : Msg -> Model -> Model
update msg model =
    let
        toggle : Maybe Section -> Maybe Section
        toggle section =
            Maybe.map (\s -> { s | hovered = not s.hovered }) section
    in
    case msg of
        OnMouseOver accountName ->
            { model | accounts = Dict.update accountName toggle model.accounts }

        OnMouseOut accountName ->
            { model | accounts = Dict.update accountName toggle model.accounts }
