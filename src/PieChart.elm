module PieChart exposing (Model, Msg, getModel, pie, update)

import Account exposing (SimpleAccount)
import Array exposing (Array)
import Dict exposing (Dict)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOut, onMouseOver)


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


dashArray : Circle -> Stroke -> String
dashArray circle stroke =
    String.join " "
        [ String.fromFloat stroke.length
        , String.fromFloat <| tau * circle.r - stroke.length
        ]


dashOffset : Circle -> Stroke -> String
dashOffset _ stroke =
    String.fromFloat (tau - stroke.offset)


mapToStroke : Circle -> Model -> List ( Section, Stroke )
mapToStroke circle model =
    let
        sectionList =
            Dict.values model.accounts |> List.sortBy (.account >> .balance)

        totalAmount =
            sum sectionList

        f section ( list, offset ) =
            let
                currLength =
                    length circle (toFloat totalAmount) (toFloat section.account.balance)
            in
            ( ( section, Stroke currLength offset ) :: list, offset + currLength )
    in
    List.foldr f ( [], tau - tau / 4 ) sectionList
        |> Tuple.first


genericPie : Circle -> Model -> Svg Msg
genericPie circData model =
    let
        f : ( Section, Stroke ) -> Svg Msg
        f ( section, sectionStroke ) =
            circle
                [ cx <| String.fromFloat circData.x
                , cy <| String.fromFloat circData.y
                , r <| String.fromFloat circData.r
                , fill "none"
                , stroke <| section.color
                , strokeWidth <| String.fromFloat circData.width
                , strokeDasharray <| dashArray circData sectionStroke
                , strokeDashoffset <| dashOffset circData sectionStroke
                , pointerEvents "visibleStroke"
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
