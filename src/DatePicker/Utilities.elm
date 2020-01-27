module DatePicker.Utilities exposing (addLeadingZero, dayToNameString, doDaysMatch, durationDayPickedOrBetween, enforceTimeBoundaries, eventIsOutsideComponent, generateHourOptions, generateMinuteOptions, monthData, monthToNameString, selectedAndSelectableTimeParts, setDayNotTime, setHourNotDay, setMinuteNotDay, splitIntoWeeks)

import Html exposing (Html, option, text)
import Html.Attributes exposing (selected, value)
import Json.Decode as Decode
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra as Time exposing (Interval(..))


monthData : Zone -> Posix -> List Posix
monthData zone time =
    let
        monthStart =
            Time.floor Month zone time

        monthStartDay =
            Time.toWeekday zone monthStart

        nextMonthStart =
            -- we add a day to monthStart to guarantee next month, Time.ceiling
            -- with monthStart was producing the same month, not next month
            Time.ceiling Month zone (Time.add Day 1 zone monthStart)

        nextMonthStartDay =
            Time.toWeekday zone nextMonthStart

        frontPad =
            case monthStartDay of
                Mon ->
                    Time.range Day 1 zone (Time.add Day -1 zone monthStart) monthStart

                Tue ->
                    Time.range Day 1 zone (Time.add Day -2 zone monthStart) monthStart

                Wed ->
                    Time.range Day 1 zone (Time.add Day -3 zone monthStart) monthStart

                Thu ->
                    Time.range Day 1 zone (Time.add Day -4 zone monthStart) monthStart

                Fri ->
                    Time.range Day 1 zone (Time.add Day -5 zone monthStart) monthStart

                Sat ->
                    Time.range Day 1 zone (Time.add Day -6 zone monthStart) monthStart

                Sun ->
                    []

        endPad =
            case nextMonthStartDay of
                Mon ->
                    Time.range Day 1 zone nextMonthStart (Time.add Day 6 zone nextMonthStart)

                Tue ->
                    Time.range Day 1 zone nextMonthStart (Time.add Day 5 zone nextMonthStart)

                Wed ->
                    Time.range Day 1 zone nextMonthStart (Time.add Day 4 zone nextMonthStart)

                Thu ->
                    Time.range Day 1 zone nextMonthStart (Time.add Day 3 zone nextMonthStart)

                Fri ->
                    Time.range Day 1 zone nextMonthStart (Time.add Day 2 zone nextMonthStart)

                Sat ->
                    Time.range Day 1 zone nextMonthStart (Time.add Day 1 zone nextMonthStart)

                Sun ->
                    []
    in
    frontPad ++ Time.range Day 1 zone monthStart nextMonthStart ++ endPad


splitIntoWeeks : List Posix -> List (List Posix) -> List (List Posix)
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


durationDayPickedOrBetween : Zone -> Posix -> Maybe Posix -> ( Maybe Posix, Maybe Posix ) -> ( Bool, Bool )
durationDayPickedOrBetween zone day hovered ( pickedStart, pickedEnd ) =
    case ( pickedStart, pickedEnd ) of
        ( Nothing, Nothing ) ->
            ( False, False )

        ( Just start, Nothing ) ->
            let
                picked =
                    doDaysMatch zone day start

                between =
                    case hovered of
                        Just hoveredTime ->
                            isDayBetweenDates day start hoveredTime

                        Nothing ->
                            False
            in
            ( picked, between )

        ( Nothing, Just end ) ->
            let
                picked =
                    doDaysMatch zone day end

                between =
                    case hovered of
                        Just hoveredTime ->
                            isDayBetweenDates day end hoveredTime

                        Nothing ->
                            False
            in
            ( picked, between )

        ( Just start, Just end ) ->
            let
                picked =
                    doDaysMatch zone day end || doDaysMatch zone day start

                between =
                    isDayBetweenDates day start end
            in
            ( picked, between )


isDayBetweenDates : Posix -> Posix -> Posix -> Bool
isDayBetweenDates day dateOne dateTwo =
    (Time.posixToMillis dateOne
        > Time.posixToMillis day
        && Time.posixToMillis day
        > Time.posixToMillis dateTwo
    )
        || (Time.posixToMillis dateOne
                < Time.posixToMillis day
                && Time.posixToMillis day
                < Time.posixToMillis dateTwo
           )


doDaysMatch : Zone -> Posix -> Posix -> Bool
doDaysMatch zone dateTimeOne dateTimeTwo =
    let
        oneParts =
            Time.posixToParts zone dateTimeOne

        twoParts =
            Time.posixToParts zone dateTimeTwo
    in
    oneParts.day == twoParts.day && oneParts.month == twoParts.month && oneParts.year == twoParts.year


{-| Set the day (month and year) of the previously selected dateTime to match that of the newly selected dateTime
-}
setDayNotTime : Zone -> Posix -> Posix -> Posix
setDayNotTime zone newPickedDT prevPickedDT =
    let
        newPickedParts =
            Time.posixToParts zone newPickedDT
    in
    Time.posixToParts zone prevPickedDT |> (\parts -> { parts | day = newPickedParts.day, month = newPickedParts.month, year = newPickedParts.year }) |> Time.partsToPosix zone


{-| Set only the hour of the provided dateTime
-}
setHourNotDay : Zone -> Int -> Posix -> Posix
setHourNotDay zone hour timeToUpdate =
    let
        parts =
            Time.posixToParts zone timeToUpdate

        newParts =
            { parts | hour = hour }
    in
    Time.partsToPosix zone newParts


{-| Set only the minute of the provided dateTime
-}
setMinuteNotDay : Zone -> Int -> Posix -> Posix
setMinuteNotDay zone minute timeToUpdate =
    let
        parts =
            Time.posixToParts zone timeToUpdate

        newParts =
            { parts | minute = minute }
    in
    Time.partsToPosix zone newParts


generateHourOptions : List Int -> Int -> List (Html msg)
generateHourOptions selectableHours selectedHour =
    selectableHours
        |> List.map (\hour -> option [ value (String.fromInt hour), selected (selectedHour == hour) ] [ text (addLeadingZero hour) ])


generateMinuteOptions : List Int -> Int -> List (Html msg)
generateMinuteOptions selectableMinutes selectedMinute =
    selectableMinutes
        |> List.map (\minute -> option [ value (String.fromInt minute), selected (selectedMinute == minute) ] [ text (addLeadingZero minute) ])


eventIsOutsideComponent : String -> Decode.Decoder Bool
eventIsOutsideComponent componentId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if componentId == id then
                        -- found match by id
                        Decode.succeed False

                    else
                        -- try next decoder
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> eventIsOutsideComponent componentId |> Decode.field "parentNode")

        -- fallback if all previous decoders failed
        , Decode.succeed True
        ]


filterSelectableHours : Zone -> Posix -> (Zone -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }) -> List Int
filterSelectableHours zone dateTimeBeingProcessed allowableTimesFn =
    let
        { startHour, endHour } =
            allowableTimesFn zone dateTimeBeingProcessed
    in
    List.range startHour endHour


filterSelectableMinutes : Zone -> Posix -> (Zone -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }) -> List Int
filterSelectableMinutes zone dateTimeBeingProcessed allowableTimesFn =
    let
        { startHour, startMinute, endHour, endMinute } =
            allowableTimesFn zone dateTimeBeingProcessed

        hour =
            Time.toHour zone dateTimeBeingProcessed

        filterBefore =
            if startHour == hour then
                startMinute

            else
                0

        filterAfter =
            if endHour == hour then
                endMinute

            else
                59
    in
    List.range filterBefore filterAfter


selectedAndSelectableTimeParts : Zone -> Maybe Posix -> (Zone -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }) -> { selectedHour : Int, selectableHours : List Int, selectedMinute : Int, selectableMinutes : List Int }
selectedAndSelectableTimeParts zone pickedDateTime allowableTimesFn =
    Maybe.map
        (\time ->
            let
                timeParts =
                    Time.posixToParts zone time
            in
            { selectedHour = timeParts.hour
            , selectableHours = filterSelectableHours zone time allowableTimesFn
            , selectedMinute = timeParts.minute
            , selectableMinutes = filterSelectableMinutes zone time allowableTimesFn
            }
        )
        pickedDateTime
        |> Maybe.withDefault { selectedHour = 0, selectableHours = List.range 0 23, selectedMinute = 0, selectableMinutes = List.range 0 59 }


enforceTimeBoundaries : Zone -> Posix -> (Zone -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }) -> Posix
enforceTimeBoundaries zone dateTimeBeingProcessed allowableTimesFn =
    let
        { startHour, startMinute } =
            allowableTimesFn zone dateTimeBeingProcessed
    in
    Time.posixToParts zone dateTimeBeingProcessed |> (\parts -> { parts | hour = startHour, minute = startMinute } |> Time.partsToPosix zone)
