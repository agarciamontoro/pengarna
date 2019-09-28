module Transaction exposing
    ( EuroPosting
    , Posting
    , Transaction
    , allEuroPostings
    , allPostings
    , concatPostingsFromAccount
    , currentDay
    , currentMonth
    , currentYear
    , dateToString
    , filterByMonth
    , getAllBalances
    , isFromMonth
    , isOfAccount
    , monthToSpanish
    , postingDecoder
    , postingsFromAccount
    , toDayUTC
    , toEuroPosting
    , toMonthUTC
    , toTuple
    , toYearUTC
    , transactionDecoder
    , viewPosting
    , viewTransaction
    , viewTransactionList
    )

import Account exposing (Account)
import Array
import Balance exposing (Balance)
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Commodity exposing (Commodity)
import Dict exposing (Dict)
import Html exposing (Html, div, h3, li, nav, span, text, ul)
import Html.Attributes exposing (class)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Task exposing (Task)
import Time


toYearUTC : Time.Posix -> Int
toYearUTC =
    Time.toYear Time.utc


toMonthUTC : Time.Posix -> Time.Month
toMonthUTC =
    Time.toMonth Time.utc


toDayUTC : Time.Posix -> Int
toDayUTC =
    Time.toDay Time.utc


currentDay : Task x Int
currentDay =
    Task.map2 Time.toDay Time.here Time.now


currentMonth : Task x Time.Month
currentMonth =
    Task.map2 Time.toMonth Time.here Time.now


currentYear : Task x Int
currentYear =
    Task.map2 Time.toDay Time.here Time.now


type alias Transaction =
    { postings : List Posting
    , date : Time.Posix
    , description : String
    }


transactionDecoder : Decoder Transaction
transactionDecoder =
    Decode.map3 Transaction
        (Decode.field "tpostings" (Decode.list postingDecoder))
        (Decode.field "tdate" Iso8601.decoder)
        (Decode.field "tdescription" Decode.string)


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



-- type alias NewPosting =
--     { amounts : Dict Commodity Float
--     , account : String
--     }
--
--
-- toTuple : Balance -> ( Commodity, Float )
-- toTuple balance =
--     ( balance.commodity, balance.quantity )
--
--
-- oldToNew : Posting -> NewPosting
-- oldToNew posting =
--     NewPosting
--         (Dict.fromList (List.map toTuple posting.amounts))
--         posting.account


postingDecoder : Decoder Posting
postingDecoder =
    Decode.map2 Posting
        (Decode.field "pamount" (Decode.list Balance.balanceDecoder))
        (Decode.field "paccount" Decode.string)


monthToSpanish : Time.Month -> String
monthToSpanish month =
    case month of
        Time.Jan ->
            "enero"

        Time.Feb ->
            "febrero"

        Time.Mar ->
            "marzo"

        Time.Apr ->
            "abril"

        Time.May ->
            "mayo"

        Time.Jun ->
            "junio"

        Time.Jul ->
            "julio"

        Time.Aug ->
            "agosto"

        Time.Sep ->
            "septiembre"

        Time.Oct ->
            "octubre"

        Time.Nov ->
            "noviembre"

        Time.Dec ->
            "diciembre"


dateToString : Time.Posix -> String
dateToString time =
    String.concat
        [ String.fromInt <| toDayUTC time
        , " de "
        , monthToSpanish <| toMonthUTC time
        , " de "
        , String.fromInt <| toYearUTC time
        ]


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


viewTransaction : Transaction -> List (Html msg)
viewTransaction transaction =
    [ h3 [ class Bulma.isSize3 ] [ text transaction.description ]
    , span [] [ text <| dateToString transaction.date ]
    , ul [ class Bulma.panel ] <|
        List.map (\posting -> li [ class Bulma.panelBlock ] (viewPosting posting)) transaction.postings
    ]


isFromMonth : Time.Month -> Transaction -> Bool
isFromMonth month transaction =
    toMonthUTC transaction.date == month


filterByMonth : List Transaction -> Time.Month -> List Transaction
filterByMonth accounts month =
    List.filter (isFromMonth month) accounts


isOfAccount : Account -> Posting -> Bool
isOfAccount account posting =
    posting.account == account.name


postingsFromAccount : Account -> Transaction -> List Posting
postingsFromAccount account transaction =
    List.filter (isOfAccount account) transaction.postings


concatPostingsFromAccount : Account -> List Transaction -> List Posting
concatPostingsFromAccount account transactions =
    List.concatMap (postingsFromAccount account) transactions



-- rawBalance : List Posting -> Dict Commodity Float
-- rawBalance postings =
--     List.partition (\posting.)
-- List Transaction -> Dict String Float
--


allPostings : List Transaction -> List Posting
allPostings =
    List.concatMap .postings


allEuroPostings : List Transaction -> List EuroPosting
allEuroPostings transactions =
    List.map toEuroPosting (allPostings transactions)


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


viewTransactionList : List Transaction -> List (Html msg)
viewTransactionList transactions =
    [ ul [] <|
        List.reverse <|
            List.map
                (\trans -> li [] (viewTransaction trans))
                transactions
    ]
