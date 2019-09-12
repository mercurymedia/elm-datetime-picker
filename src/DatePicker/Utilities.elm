module DatePicker.Utilities exposing (addLeadingZero, dayToNameString, doDaysMatch, durationDayPickedOrBetween, monthData, monthToNameString, splitIntoWeeks, switchTimes, toUtcDateTimeString)

import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..))
import Time.Extra as Time exposing (Interval(..), Parts)


monthData : Posix -> List Parts
monthData time =
    let
        monthStart =
            Time.floor Month Time.utc time

        monthStartDay =
            Time.toWeekday Time.utc monthStart

        nextMonthStart =
            -- we add a day to monthStart to guarantee next month, Time.ceiling
            -- with monthStart was producing the same month, not next month
            Time.ceiling Month Time.utc (Time.add Day 1 Time.utc monthStart)

        nextMonthStartDay =
            Time.toWeekday Time.utc nextMonthStart

        frontPad =
            case monthStartDay of
                Mon ->
                    Time.range Day 1 Time.utc (Time.add Day -1 Time.utc monthStart) monthStart

                Tue ->
                    Time.range Day 1 Time.utc (Time.add Day -2 Time.utc monthStart) monthStart

                Wed ->
                    Time.range Day 1 Time.utc (Time.add Day -3 Time.utc monthStart) monthStart

                Thu ->
                    Time.range Day 1 Time.utc (Time.add Day -4 Time.utc monthStart) monthStart

                Fri ->
                    Time.range Day 1 Time.utc (Time.add Day -5 Time.utc monthStart) monthStart

                Sat ->
                    Time.range Day 1 Time.utc (Time.add Day -6 Time.utc monthStart) monthStart

                Sun ->
                    []

        endPad =
            case nextMonthStartDay of
                Mon ->
                    Time.range Day 1 Time.utc nextMonthStart (Time.add Day 6 Time.utc nextMonthStart)

                Tue ->
                    Time.range Day 1 Time.utc nextMonthStart (Time.add Day 5 Time.utc nextMonthStart)

                Wed ->
                    Time.range Day 1 Time.utc nextMonthStart (Time.add Day 4 Time.utc nextMonthStart)

                Thu ->
                    Time.range Day 1 Time.utc nextMonthStart (Time.add Day 3 Time.utc nextMonthStart)

                Fri ->
                    Time.range Day 1 Time.utc nextMonthStart (Time.add Day 2 Time.utc nextMonthStart)

                Sat ->
                    Time.range Day 1 Time.utc nextMonthStart (Time.add Day 1 Time.utc nextMonthStart)

                Sun ->
                    []
    in
    (frontPad |> List.map (Time.posixToParts Time.utc))
        ++ (Time.range Day 1 Time.utc monthStart nextMonthStart
                |> List.map (Time.posixToParts Time.utc)
           )
        ++ (endPad |> List.map (Time.posixToParts Time.utc))


splitIntoWeeks : List Parts -> List (List Parts) -> List (List Parts)
splitIntoWeeks days weeks =
    if List.length days <= 7 then
        days :: weeks

    else
        let
            ( week, restOfDays ) =
                List.splitAt 7 days

            newWeeks =
                week :: weeks
        in
        splitIntoWeeks restOfDays newWeeks


monthToNameString : Month -> String
monthToNameString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


dayToNameString : Weekday -> String
dayToNameString day =
    case day of
        Mon ->
            "Mo"

        Tue ->
            "Tu"

        Wed ->
            "We"

        Thu ->
            "Th"

        Fri ->
            "Fr"

        Sat ->
            "Sa"

        Sun ->
            "Su"


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


durationDayPickedOrBetween : Parts -> Maybe Posix -> ( Maybe Posix, Maybe Posix ) -> ( Bool, Bool )
durationDayPickedOrBetween day hovered ( pickedStart, pickedEnd ) =
    case ( pickedStart, pickedEnd ) of
        ( Nothing, Nothing ) ->
            ( False, False )

        ( Just start, Nothing ) ->
            let
                picked =
                    doDaysMatch day (Time.posixToParts Time.utc start)

                between =
                    case hovered of
                        Just hoveredTime ->
                            isDayBetweenPickedAndHovered (Time.partsToPosix Time.utc day) start hoveredTime

                        Nothing ->
                            False
            in
            ( picked, between )

        ( Nothing, Just end ) ->
            let
                picked =
                    doDaysMatch day (Time.posixToParts Time.utc end)

                between =
                    case hovered of
                        Just hoveredTime ->
                            isDayBetweenPickedAndHovered (Time.partsToPosix Time.utc day) end hoveredTime

                        Nothing ->
                            False
            in
            ( picked, between )

        ( Just start, Just end ) ->
            let
                picked =
                    doDaysMatch day (Time.posixToParts Time.utc end) || doDaysMatch day (Time.posixToParts Time.utc start)

                between =
                    isDayBetweenPicks (Time.partsToPosix Time.utc day) start end
            in
            ( picked, between )


isDayBetweenPickedAndHovered : Posix -> Posix -> Posix -> Bool
isDayBetweenPickedAndHovered day picked hovered =
    (Time.posixToMillis picked
        > Time.posixToMillis day
        && Time.posixToMillis day
        > Time.posixToMillis hovered
    )
        || (Time.posixToMillis picked
                < Time.posixToMillis day
                && Time.posixToMillis day
                < Time.posixToMillis hovered
           )


isDayBetweenPicks : Posix -> Posix -> Posix -> Bool
isDayBetweenPicks day start end =
    Time.posixToMillis start
        < Time.posixToMillis day
        && Time.posixToMillis day
        < Time.posixToMillis end


doDaysMatch : Parts -> Parts -> Bool
doDaysMatch refDay targetDay =
    refDay.day == targetDay.day && refDay.month == targetDay.month && refDay.year == targetDay.year


switchTimes : Parts -> Parts -> ( Posix, Posix )
switchTimes dateTimeOne dateTimeTwo =
    let
        newOne =
            { dateTimeOne | hour = dateTimeTwo.hour, minute = dateTimeTwo.minute } |> Time.partsToPosix Time.utc

        newTwo =
            { dateTimeTwo | hour = dateTimeOne.hour, minute = dateTimeOne.minute } |> Time.partsToPosix Time.utc
    in
    ( newOne, newTwo )


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


toUtcDateString : Posix -> String
toUtcDateString date =
    addLeadingZero (Time.toDay Time.utc date)
        ++ "."
        ++ monthToNmbString (Time.toMonth Time.utc date)
        ++ "."
        ++ addLeadingZero (Time.toYear Time.utc date)


toUtcDateTimeString : Posix -> String
toUtcDateTimeString datetime =
    toUtcDateString datetime
        ++ " "
        ++ addLeadingZero (Time.toHour Time.utc datetime)
        ++ ":"
        ++ addLeadingZero (Time.toMinute Time.utc datetime)
        ++ ":"
        ++ addLeadingZero (Time.toSecond Time.utc datetime)
        ++ " (UTC)"
