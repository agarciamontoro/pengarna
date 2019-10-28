module Transaction exposing
    ( Transaction
    , balancesFromMonth
    , concatPostingsFromAccount
    , filterByMonth
    , fromMonth
    , getAllBalances
    , isFromMonth
    , postingsFromAccount
    , transactionDecoder
    , viewTransaction
    , viewTransactionList
    )

import Account exposing (Account, formatAccountName)
import Balance exposing (Balance)
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Dict exposing (Dict)
import Html exposing (Html, div, h3, li, p, span, text, ul)
import Html.Attributes exposing (class)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Posting exposing (EuroPosting, Posting)
import Time
import TimeUtils


type alias Transaction =
    { postings : List Posting
    , date : Time.Posix
    , description : String
    }


transactionDecoder : Decoder Transaction
transactionDecoder =
    Decode.map3 Transaction
        (Decode.field "tpostings" (Decode.list Posting.postingDecoder))
        (Decode.field "tdate" Iso8601.decoder)
        (Decode.field "tdescription" Decode.string)


viewTransaction : Transaction -> List (Html msg)
viewTransaction transaction =
    let
        title =
            div [ BulmaHelpers.classList [ Bulma.level, Bulma.isMobile ] ]
                [ div [ class Bulma.levelLeft ]
                    [ div [ class Bulma.levelItem ]
                        [ h3 [ class Bulma.isSize3 ]
                            [ text transaction.description ]
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
    [ title
    , span [] [ text <| TimeUtils.dateToString transaction.date ]
    , ul [ class Bulma.panel ] <|
        List.map (\posting -> li [ class Bulma.panelBlock ] (Posting.viewPosting posting)) transaction.postings
    ]


isFromMonth : Time.Month -> Transaction -> Bool
isFromMonth month transaction =
    TimeUtils.toMonthUTC transaction.date == month


filterByMonth : List Transaction -> Time.Month -> List Transaction
filterByMonth accounts month =
    List.filter (isFromMonth month) accounts


postingsFromAccount : Account -> Transaction -> List Posting
postingsFromAccount account transaction =
    List.filter (Posting.isOfAccount account) transaction.postings


concatPostingsFromAccount : Account -> List Transaction -> List Posting
concatPostingsFromAccount account transactions =
    List.concatMap (postingsFromAccount account) transactions


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


fromMonth : Maybe Time.Month -> List Transaction -> List Transaction
fromMonth month transactions =
    Maybe.withDefault [] <|
        Maybe.map (filterByMonth transactions) month


balancesFromMonth : Maybe Time.Month -> List Transaction -> List (Html msg)
balancesFromMonth month transactions =
    let
        list =
            Dict.toList <| getAllBalances <| fromMonth month transactions
    in
    [ ul
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
    ]


viewTransactionList : List Transaction -> List (Html msg)
viewTransactionList transactions =
    [ ul [] <|
        List.reverse <|
            List.map
                (\trans -> li [] (viewTransaction trans))
                transactions
    ]


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
