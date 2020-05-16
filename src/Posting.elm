module Posting exposing
    ( Posting, EuroPosting
    , postingDecoder, viewPosting
    , isOfAccount, toEuroPosting
    )

{-|

@docs Posting, EuroPosting


# JSON and HTML

@docs postingDecoder, viewPosting


# Utils

@docs isOfAccount, toEuroPosting

-}

import Account exposing (Account)
import Balance exposing (Balance)
import Bulma.Classes as Bulma
import Html exposing (Html, li, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)


{-| A Posting is a group of [`Balance`s](Balance#Balance) (with possibly
different commodities) for a specific account.

    Posting [ Balance Commodity.Euro 10.5 ] "assets:cash"

    Posting [ Balance Commodity.Euro -50 ] "assets:bank"

-}
type alias Posting =
    { amounts : List Balance
    , account : String
    }


{-| A EuroPosting is a particular type of Posting, in which all the balances are assumed to be Euros. As there are no different commodities, the amount of the posting is a single number.s

    EuroPosting 0.79 "expenses:food:milk"

-}
type alias EuroPosting =
    { amount : Float
    , account : String
    }


{-| Convert a generic [Posting](#Posting) into a [EuroPosting](#EuroPosting),
retrieving the first balance whose commodity field is Commodity.Euro. If it
does not exist, the built EuroPosting's amount equals 0.

    toEuroPosting (Posting [ Balance Commodity.Euro 10.5 ] "assets:cash") == EuroPosting 10.5 "assets:cash"

-}
toEuroPosting : Posting -> EuroPosting
toEuroPosting posting =
    EuroPosting (Balance.getFirstEuroBalance posting.amounts) posting.account


{-| Check whether a [`Posting`](#Posting) refers to a specific [`Account`](Account#Account).
-}
isOfAccount : Account -> Posting -> Bool
isOfAccount account posting =
    posting.account == account.name


{-| JSON decoder that expects, at least:

  - A `pamount` field that contains a list of well-formed Balances (see [`balanceDecoder`](Balance#balanceDecoder)).
  - A `paccount` field that contains a string with the name of the account.

-}
postingDecoder : Decoder Posting
postingDecoder =
    Decode.map2 Posting
        (Decode.field "pamount" (Decode.list Balance.balanceDecoder))
        (Decode.field "paccount" Decode.string)


{-| Render a posting using the first Euro balance found.
-}
viewPosting : Posting -> Html msg
viewPosting posting =
    li [ class Bulma.panelBlock ]
        [ span
            [ class Bulma.isLeft ]
            [ Account.formatAccountName posting.account ]
        , span [ class Bulma.isRight ]
            [ text <|
                String.fromFloat <|
                    Balance.getFirstEuroBalance posting.amounts
            ]
        ]
