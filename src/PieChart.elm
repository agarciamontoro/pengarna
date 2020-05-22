module PieChart exposing (Model, Msg, getModel, pie, update)

import Account exposing (SimpleAccount)
import Dict exposing (Dict)
import Svg exposing (..)
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



-- In Polar coordinates


type alias CircSection =
    { start : Float
    , angleLength : Float
    , outerRadius : Float
    , innerRadius : Float
    }


circSectionPath : CircSection -> String
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


type alias Model =
    { accounts : Dict String Section
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
    List.filterMap toDict accounts |> Dict.fromList |> Model


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


mapToStroke : Circle -> Model -> List ( Section, Stroke )
mapToStroke circle model =
    let
        sectionList =
            Dict.values model.accounts |> List.sortBy (.account >> .balance)

        totalAmount =
            toFloat <| sum sectionList

        f section ( list, offset ) =
            let
                currLength =
                    length circle totalAmount (toFloat section.account.balance)
            in
            ( ( section, Stroke currLength offset ) :: list, offset + currLength )
    in
    List.foldr f ( [], -pi / 2 ) sectionList
        |> Tuple.first


genericPie : Circle -> Model -> Svg Msg
genericPie circData model =
    let
        strokeColor section =
            if section.hovered then
                "gold"

            else
                section.color

        f : ( Section, Stroke ) -> Svg Msg
        f ( section, sectionStroke ) =
            Svg.path
                [ d <| circSectionPath <| CircSection sectionStroke.offset sectionStroke.length 1 0.8
                , fill <| strokeColor section -- "none"
                , stroke "red"
                , strokeWidth "0.005"
                , strokeLinecap "square"
                , onMouseOver <| OnMouseOver section.account.name
                , onMouseOut <| OnMouseOut section.account.name
                ]
                []
    in
    svg
        [ width "100%"
        , height "100%"
        , viewBox <| viewBoxStr circData
        ]
    <|
        List.map f <|
            mapToStroke circData model


pie : Model -> Svg Msg
pie =
    genericPie unitCirc


update : Msg -> Model -> Model
update msg model =
    let
        toggle : Maybe Section -> Maybe Section
        toggle section =
            Maybe.map (\s -> { s | hovered = not s.hovered }) section
    in
    case msg of
        OnMouseOver accountName ->
            Model <| Dict.update accountName toggle model.accounts

        OnMouseOut accountName ->
            Model <| Dict.update accountName toggle model.accounts
