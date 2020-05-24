module PieChart exposing (Model, Msg, getModel, update, view)

import Account exposing (SimpleAccount)
import Balance
import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOut, onMouseOver)
import SvgUtils


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
                [ SvgUtils.circSectionPath <| SvgUtils.CircSection sectionStroke.offset sectionStroke.length 1 0.8
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
