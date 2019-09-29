module Posting exposing
    ( EuroPosting
    , Posting
    , isOfAccount
    , postingDecoder
    , toEuroPosting
    , toTuple
    , viewPosting
    )

import Account exposing (Account)
import Balance exposing (Balance)
import Bulma.Classes as Bulma
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)


type alias Posting =
    { amounts : List Balance
    , account : String
    }


type alias EuroPosting =
    { amount : Float
    , account : String
    }


toEuroPosting : Posting -> EuroPosting
toEuroPosting posting =
    EuroPosting (Balance.getFirstEuroBalance posting.amounts) posting.account


toTuple : EuroPosting -> ( String, Float )
toTuple posting =
    ( posting.account, posting.amount )


isOfAccount : Account -> Posting -> Bool
isOfAccount account posting =
    posting.account == account.name


postingDecoder : Decoder Posting
postingDecoder =
    Decode.map2 Posting
        (Decode.field "pamount" (Decode.list Balance.balanceDecoder))
        (Decode.field "paccount" Decode.string)


viewPosting : Posting -> List (Html msg)
viewPosting posting =
    [ span
        [ class Bulma.isLeft ]
        [ text posting.account ]
    , span [ class Bulma.isRight ]
        [ text <|
            String.fromFloat <|
                Balance.getFirstEuroBalance posting.amounts
        ]
    ]
