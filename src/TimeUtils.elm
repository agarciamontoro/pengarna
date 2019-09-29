module TimeUtils exposing
    ( currentDay
    , currentMonth
    , currentYear
    , dateToString
    , monthToSpanish
    , toDayUTC
    , toMonthUTC
    , toYearUTC
    )

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
