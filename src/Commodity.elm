module Commodity exposing
    ( Commodity(..)
    , commodityDecoder
    )

import Json.Decode as Decode exposing (Decoder)


type Commodity
    = Euro
    | Unknown


commodityDecoder : String -> Decoder Commodity
commodityDecoder string =
    Decode.succeed <|
        case string of
            "€" ->
                Euro

            other ->
                Unknown
