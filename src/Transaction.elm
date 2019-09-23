module Transaction exposing
    ( Posting
    , Transaction
    , dateToString
    , monthToSpanish
    , postingDecoder
    , transactionDecoder
    , viewTransaction
    )

import Array
import Balance exposing (Balance)
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Html exposing (Html, div, h3, li, nav, span, text, ul)
import Html.Attributes exposing (class)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
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
    [ nav
        [ BulmaHelpers.classList
            [ Bulma.breadcrumb
            , Bulma.hasBulletSeparator
            ]
        ]
        [ ul [] <|
            List.map
                (\acc -> li [] [ text acc ])
                (String.split ":" posting.account)
        ]
    , span []
        [ text <|
            String.fromFloat <|
                Maybe.withDefault 0 (Balance.getEuroBalance posting.amounts)
        ]
    ]


viewTransaction : Transaction -> List (Html msg)
viewTransaction transaction =
    [ h3 [] [ text transaction.description ]
    , span [] [ text <| dateToString transaction.date ]
    , div [] <| List.map (\posting -> div [] (viewPosting posting)) transaction.postings
    ]
