module Utilities exposing (adjustAllowedTimesOfDayToClientZone, isDateBeforeToday, posixToDateString, posixToTimeString)

import Time exposing (Month(..), Posix, Zone)
import Time.Extra as TimeExtra exposing (Interval(..))


addLeadingZero : Int -> String
addLeadingZero value =
    let
        string =
            String.fromInt value
    in
    if String.length string == 1 then
        "0" ++ string

    else
        string


monthToNmbString : Month -> String
monthToNmbString month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


isDateBeforeToday : Posix -> Posix -> Bool
isDateBeforeToday today datetime =
    Time.posixToMillis today > Time.posixToMillis datetime


posixToDateString : Zone -> Posix -> String
posixToDateString zone date =
    addLeadingZero (Time.toDay zone date)
        ++ "."
        ++ monthToNmbString (Time.toMonth zone date)
        ++ "."
        ++ addLeadingZero (Time.toYear zone date)


posixToTimeString : Zone -> Posix -> String
posixToTimeString zone datetime =
    addLeadingZero (Time.toHour zone datetime)
        ++ ":"
        ++ addLeadingZero (Time.toMinute zone datetime)
        ++ ":"
        ++ addLeadingZero (Time.toSecond zone datetime)


{-| The goal of this naive function is to adjust
the allowed time boundaries within the baseZone
to the time zone in which the picker is running
(clientZone) for the current day being processed
(datetime).

For example, the allowed times of day could be
9am - 5pm EST. However, if someone is using the
picker in MST (2 hours behind EST), the allowed
times of day displayed in the picker should be
7am - 3pm.

There is likely a better way to do this, but it
is suitable as an example.

-}
adjustAllowedTimesOfDayToClientZone : Zone -> Zone -> Posix -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }
adjustAllowedTimesOfDayToClientZone baseZone clientZone today datetimeBeingProcessed =
    let
        processingPartsInClientZone =
            TimeExtra.posixToParts clientZone datetimeBeingProcessed

        todayPartsInClientZone =
            TimeExtra.posixToParts clientZone today

        startPartsAdjustedForBaseZone =
            TimeExtra.posixToParts baseZone datetimeBeingProcessed
                |> (\parts -> TimeExtra.partsToPosix baseZone { parts | hour = 8, minute = 0 })
                |> TimeExtra.posixToParts clientZone

        endPartsAdjustedForBaseZone =
            TimeExtra.posixToParts baseZone datetimeBeingProcessed
                |> (\parts -> TimeExtra.partsToPosix baseZone { parts | hour = 17, minute = 30 })
                |> TimeExtra.posixToParts clientZone

        bounds =
            { startHour = startPartsAdjustedForBaseZone.hour
            , startMinute = startPartsAdjustedForBaseZone.minute
            , endHour = endPartsAdjustedForBaseZone.hour
            , endMinute = endPartsAdjustedForBaseZone.minute
            }
    in
    if processingPartsInClientZone.day == todayPartsInClientZone.day && processingPartsInClientZone.month == todayPartsInClientZone.month && processingPartsInClientZone.year == todayPartsInClientZone.year then
        if todayPartsInClientZone.hour > bounds.startHour || (todayPartsInClientZone.hour == bounds.startHour && todayPartsInClientZone.minute > bounds.startMinute) then
            { startHour = todayPartsInClientZone.hour, startMinute = todayPartsInClientZone.minute, endHour = bounds.endHour, endMinute = bounds.endMinute }

        else
            bounds

    else
        bounds
