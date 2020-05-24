module PieChart exposing (Model, Msg, getModel, update, view)

import Account exposing (SimpleAccount)
import Balance
import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOut, onMouseOver)
import Svg.Keyed
import SvgUtils


{-| PieChart model

  - total is the sum of the balances in all the accounts.
  - circWidth is the width of the circle, as a percentage (over 1) of the circle radius.
  - accounts is a dictionary mapping every account name to its AccountSection, the type
    containing all data to render a circle section.

-}
type alias Model =
    { total : Int
    , circWidth : Float
    , hoveredAccount : Maybe AccountSection
    , accounts : Dict String AccountSection
    }


mapToStroke : Int -> List SimpleAccount -> List ( String, AccountSection )
mapToStroke total accounts =
    let
        sectionList =
            List.sortBy .balance accounts

        f section ( list, offset ) =
            let
                currLength =
                    length (toFloat total) (toFloat section.balance)

                account =
                    AccountSection
                        section
                        (colorFromName section.name)
                        False
                        (Stroke currLength offset)
            in
            ( ( section.name, account ) :: list, offset + currLength )
    in
    List.foldr f ( [], -pi / 2 ) sectionList |> Tuple.first


getModel : Float -> List SimpleAccount -> Model
getModel circWidth accounts =
    let
        total =
            List.sum <| List.map .balance cleanList

        prefixLength =
            String.length "expenses:"

        cleanList =
            List.filterMap
                (\acc ->
                    case acc.name of
                        "expenses" ->
                            Nothing

                        name ->
                            Just { acc | name = String.dropLeft prefixLength name }
                )
                accounts
    in
    mapToStroke total cleanList |> Dict.fromList |> Model total circWidth Nothing


{-| Data needed to render a circle section
-}
type alias AccountSection =
    { account : SimpleAccount
    , color : String
    , selected : Bool
    , stroke : Stroke
    }


{-| Maps every top level account under expenses: to a color
-}
nameToColor : Dict String String
nameToColor =
    Dict.fromList
        [ ( "impuestos", "darkslategrey" )
        , ( "casa", "crimson" )
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


type Msg
    = OnMouseOver String
    | OnMouseOut String


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
    amount * 2 * pi * unitCirc.r / total


type alias Stroke =
    { length : Float
    , offset : Float
    }


view : Model -> Svg Msg
view model =
    let
        f : String -> AccountSection -> Svg Msg
        f name account =
            Svg.path
                [ SvgUtils.circSectionPath <| SvgUtils.CircSection account.stroke.offset account.stroke.length 1 (1 - model.circWidth)
                , fill account.color
                , onMouseOver <| OnMouseOver account.account.name
                , onMouseOut <| OnMouseOut name
                ]
                []

        allPaths =
            case model.hoveredAccount of
                Nothing ->
                    Dict.toList <| Dict.map f model.accounts

                Just acc ->
                    ( acc.account.name
                    , Svg.path
                        [ SvgUtils.circSectionPath <| SvgUtils.CircSection acc.stroke.offset acc.stroke.length 1.01 (0.99 - model.circWidth)
                        , fill "gold"
                        , onMouseOver <| OnMouseOver acc.account.name
                        , onMouseOut <| OnMouseOut acc.account.name
                        ]
                        []
                    )
                        :: (Dict.toList <| Dict.map f model.accounts)

        formatInnerText accountName amount =
            Svg.g [ textAnchor "middle" ]
                [ Svg.text_
                    [ dy "-0.07"
                    , opacity "0.5"
                    , fontSize "0.08"
                    , fontWeight "350"
                    ]
                    [ Svg.text <| String.toUpper accountName
                    ]
                , Svg.text_
                    [ x "0"
                    , y "0"
                    , fontSize "0.17"
                    , fontWeight "200"
                    , dominantBaseline "middle"
                    ]
                    [ Svg.text <| Balance.format amount
                    ]
                ]

        text =
            case model.hoveredAccount of
                Nothing ->
                    formatInnerText "Total" model.total

                Just { account } ->
                    formatInnerText account.name account.balance
    in
    Svg.Keyed.node "svg"
        [ width "100%"
        , height "100%"
        , viewBox viewBoxStr
        ]
        (( "innerTitle", text ) :: allPaths)


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnMouseOver accountName ->
            case model.hoveredAccount of
                Nothing ->
                    { model
                        | accounts = Dict.remove accountName model.accounts
                        , hoveredAccount = Dict.get accountName model.accounts
                    }

                Just _ ->
                    model

        OnMouseOut accountName ->
            { model
                | accounts = Dict.update accountName (\_ -> model.hoveredAccount) model.accounts
                , hoveredAccount = Nothing
            }
