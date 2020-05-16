module TimeUtils exposing
    ( toMonthUTC
    , dateToString
    , monthToSpanish
    )

{-| Util functions to work with Time.Posix data, UTC- and Spanish-opinionated.


# Getters

@docs toMonthUTC


# Human readable dates

@docs dateToString

-}

import Time


toYearUTC : Time.Posix -> Int
toYearUTC =
    Time.toYear Time.utc


{-| Retrieve the Time.Month part of a Time.Posix date, assuming it is UTC.
-}
toMonthUTC : Time.Posix -> Time.Month
toMonthUTC =
    Time.toMonth Time.utc


toDayUTC : Time.Posix -> Int
toDayUTC =
    Time.toDay Time.utc


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


{-| Human readable string from a Time.Posix date, assuming it is UTC.
-}
dateToString : Time.Posix -> String
dateToString time =
    String.concat
        [ String.fromInt <| toDayUTC time
        , " de "
        , monthToSpanish <| toMonthUTC time
        , " de "
        , String.fromInt <| toYearUTC time
        ]
