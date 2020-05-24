module Balance exposing
    ( Balance
    , balanceDecoder
    , getFirstEuroBalance
    , format
    )

{-|

@docs Balance


# JSON

@docs balanceDecoder


# Utils

@docs getFirstEuroBalance

-}

import Commodity exposing (Commodity)
import Json.Decode as Decode exposing (Decoder)


{-| A Balance is nothing more than a quantity of money in a specific
[`Commodity`](Commodity#Commodity). To model the quantity 54.17€, one would use

    Balance Commodity.Euro 54.17

-}
type alias Balance =
    { commodity : Commodity
    , quantity : Float
    }


{-| JSON decoder that expects, at least:

  - An `acommodity` field that contains a string that should comply with
    [`commodityDecoder`](Commodity#commodityDecoder).
  - An `aquantity` object that contains two integer fields: `decimalPlaces` and
    `decimalMantissa`, which define the quantity as
    `decimalMantissa / (10 ^ decimalPlaces)`.

-}
balanceDecoder : Decoder Balance
balanceDecoder =
    Decode.map2 Balance
        (Decode.field "acommodity" Decode.string |> Decode.andThen Commodity.commodityDecoder)
        (Decode.field "aquantity" quantityDecoder)


quantityDecoder : Decoder Float
quantityDecoder =
    let
        quantityFromJSON decimalPlaces decimalMantissa =
            toFloat decimalMantissa / toFloat (10 ^ decimalPlaces)
    in
    Decode.map2 quantityFromJSON
        (Decode.field "decimalPlaces" Decode.int)
        (Decode.field "decimalMantissa" Decode.int)


isOfCommodity : Commodity -> Balance -> Bool
isOfCommodity commodity balance =
    balance.commodity == commodity


getFirstCommodityBalance : Commodity -> List Balance -> Float
getFirstCommodityBalance commodity balances =
    case List.head (List.filter (isOfCommodity commodity) balances) of
        Nothing ->
            0

        Just balance ->
            balance.quantity


{-| Given a list of balances, retrieve the quantity of the first one whose
`commodity` field equals [`Commodity.Euro`](Commodity#Commodity); if not found,
the Float returned equals 0.
-}
getFirstEuroBalance : List Balance -> Float
getFirstEuroBalance =
    getFirstCommodityBalance Commodity.Euro


format : Int -> String
format balance =
    let
        str =
            String.fromInt balance

        maybeWholePart =
            String.slice 0 -2 str

        wholePart =
            if String.isEmpty maybeWholePart then
                "0"

            else
                maybeWholePart
    in
    String.concat
        [ wholePart
        , ","
        , String.right 2 str
        , "€"
        ]
