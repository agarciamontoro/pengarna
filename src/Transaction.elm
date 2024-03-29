module Transaction exposing
    ( Transaction
    , transactionDecoder, viewTransactionList
    , balancesFromMaybeMonth
    )

{-|

@docs Transaction


# JSON and HTML

@docs transactionDecoder, viewTransactionList


# Utils

@docs balancesFromMaybeMonth

-}

import Account exposing (Account, formatAccountName)
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Dict exposing (Dict)
import Html exposing (Html, div, h3, li, p, text, ul)
import Html.Attributes exposing (class)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Posting exposing (EuroPosting, Posting)
import Time
import TimeUtils


{-| The Transaction type models an hledger transaction: a group of Postings
done in a particular point of time with an attached description. The sum of all
the postings is expected to yield a zero balance.

    Transaction
      [ Posting [Balance Commodity.Euro 50] assets:cash
      , Posting [Balance Commodity.Euro -50] assets:bank
      ]
      (Time.millisToPosix 1574121600000)
      "Withdraw 50 euros"

-}
type alias Transaction =
    { postings : List Posting
    , date : Time.Posix
    , description : String
    }


{-| JSON decoder that expects, at least:

  - A `tpostings` field, containing a list of well-formed Postings (see
    [`postingDecoder`](Posting#postingDecoder)).
  - A `tdate` field, containing an [`ISO-8601`-compliant date](https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest/).
  - A `tdescription`, containing a string describing the transaction.

-}
transactionDecoder : Decoder Transaction
transactionDecoder =
    Decode.map3 Transaction
        (Decode.field "tpostings" (Decode.list Posting.postingDecoder))
        (Decode.field "tdate" Iso8601.decoder)
        (Decode.field "tdescription" Decode.string)


viewTransaction : Transaction -> Html msg
viewTransaction transaction =
    let
        title =
            div [ BulmaHelpers.classList [ Bulma.level, Bulma.isMobile ] ]
                [ div [ class Bulma.levelLeft ]
                    [ div [ class Bulma.levelItem ]
                        [ div []
                            [ p [ class Bulma.isSize3 ]
                                [ text transaction.description ]
                            , p [] [ text <| TimeUtils.dateToString transaction.date ]
                            ]
                        ]
                    ]
                , div [ class Bulma.levelRight ]
                    [ div [ class Bulma.levelItem ]
                        [ h3 [ class Bulma.isSize3 ]
                            [ text <|
                                String.fromFloat <|
                                    euroBalanceFromAccount "assets:" transaction
                            ]
                        ]
                    ]
                ]
    in
    li []
        [ title
        , ul [ class Bulma.panel ] <|
            List.map
                Posting.viewPosting
                transaction.postings
        ]


isFromMonth : Time.Month -> Transaction -> Bool
isFromMonth month transaction =
    TimeUtils.toMonthUTC transaction.date == month


filterByMonth : List Transaction -> Time.Month -> List Transaction
filterByMonth accounts month =
    List.filter (isFromMonth month) accounts


allPostings : List Transaction -> List Posting
allPostings =
    List.concatMap .postings


allEuroPostings : List Transaction -> List EuroPosting
allEuroPostings transactions =
    List.map Posting.toEuroPosting (allPostings transactions)


getAllBalances : List Transaction -> Dict String Float
getAllBalances transactions =
    let
        sumPosting : EuroPosting -> Dict String Float -> Dict String Float
        sumPosting posting dict =
            Dict.update
                posting.account
                (\sum -> Just <| posting.amount + Maybe.withDefault 0 sum)
                dict
    in
    List.foldl
        sumPosting
        Dict.empty
        (allEuroPostings transactions)


fromMaybeMonth : Maybe Time.Month -> List Transaction -> List Transaction
fromMaybeMonth month transactions =
    Maybe.withDefault [] <|
        Maybe.map (filterByMonth transactions) month


{-| Given a Maybe Month and a list of transactions, build an HTML list with all
the balances in that month, defaulting to an empty list if the Maybe is a
Nothing.
-}
balancesFromMaybeMonth : Maybe Time.Month -> List Transaction -> Html msg
balancesFromMaybeMonth month transactions =
    let
        list =
            Dict.toList <| getAllBalances <| fromMaybeMonth month transactions
    in
    ul
        []
        (List.map
            (\elem ->
                li []
                    [ p [] [ formatAccountName <| Tuple.first elem ]
                    , p [] [ text <| String.fromFloat (Tuple.second elem) ]
                    ]
            )
            list
        )


{-| Renders a list of transactions to an ul element.
-}
viewTransactionList : List Transaction -> Html msg
viewTransactionList transactions =
    ul [] <|
        List.reverse <|
            List.map viewTransaction transactions


euroBalanceFromAccount : String -> Transaction -> Float
euroBalanceFromAccount prefix transaction =
    let
        foo : Posting -> Maybe Float
        foo posting =
            if String.startsWith prefix posting.account then
                Just (Posting.toEuroPosting posting).amount

            else
                Nothing
    in
    List.foldl (+) 0 <| List.filterMap foo transaction.postings
