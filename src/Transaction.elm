module Transaction exposing
    ( Posting
    , SimpleDate
    , Transaction
    , monthFromInt
    , monthToSpanish
    , parseSimpleDate
    , postingDecoder
    , simpleDateDecoder
    , simpleDateToString
    , transactionDecoder
    , viewTransaction
    )

import Array
import Balance exposing (Balance)
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Html exposing (Html, div, h3, li, nav, span, text, ul)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Time


type alias Transaction =
    { postings : List Posting
    , date : SimpleDate
    , description : String
    }


transactionDecoder : Decoder Transaction
transactionDecoder =
    Decode.map3 Transaction
        (Decode.field "tpostings" (Decode.list postingDecoder))
        (Decode.field "tdate" simpleDateDecoder)
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


type alias SimpleDate =
    { year : Int
    , month : Time.Month
    , day : Int
    }


simpleDateDecoder : Decoder SimpleDate
simpleDateDecoder =
    Decode.andThen
        (\string ->
            case parseSimpleDate string of
                Just date ->
                    Decode.succeed date

                Nothing ->
                    Decode.fail <|
                        String.concat
                            [ "The string "
                            , string
                            , " could not be parsed into a date"
                            ]
        )
        Decode.string


monthFromInt : Int -> Maybe Time.Month
monthFromInt num =
    case num of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing


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


{-| Parses a simple date like 2019-09-28
-}
parseSimpleDate : String -> Maybe SimpleDate
parseSimpleDate string =
    let
        elements =
            Array.fromList <| String.split "-" string
    in
    Maybe.map3 SimpleDate
        (Array.get 0 elements |> Maybe.andThen String.toInt)
        (Array.get 1 elements
            |> Maybe.andThen String.toInt
            |> Maybe.andThen monthFromInt
        )
        (Array.get 2 elements |> Maybe.andThen String.toInt)


simpleDateToString : SimpleDate -> String
simpleDateToString { year, month, day } =
    String.concat
        [ String.fromInt day
        , " de "
        , monthToSpanish month
        , " de "
        , String.fromInt year
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
    , span [] [ text <| simpleDateToString transaction.date ]
    , div [] <| List.map (\posting -> div [] (viewPosting posting)) transaction.postings
    ]
