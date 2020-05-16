module Commodity exposing
    ( Commodity(..)
    , commodityDecoder
    )

{-|

@docs Commodity


# JSON

@docs commodityDecoder

-}

import Json.Decode as Decode exposing (Decoder)


{-| All supported commodities, with a special value for unsupported ones:
`Unknown`.
-}
type Commodity
    = Euro
    | Unknown


{-| Generate an always-successfull Decoder from a string, defaulting to `Unknown` when the string is not supported.

    commodityDecoder "€" -> Decode.succeed Euro
    commodityDecoder "¥" -> Decode.succeed Unknown

-}
commodityDecoder : String -> Decoder Commodity
commodityDecoder string =
    Decode.succeed <|
        case string of
            "€" ->
                Euro

            _ ->
                Unknown
