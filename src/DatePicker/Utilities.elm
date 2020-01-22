module DatePicker.Utilities exposing (addLeadingZero, dayToNameString, doDaysMatch, durationDayPickedOrBetween, enforceTimeBoundaries, eventIsOutsideComponent, generateHourOptions, generateMinuteOptions, monthData, monthToNameString, selectedAndSelectableTimeParts, setDayNotTime, setHourNotDay, setMinuteNotDay, splitIntoWeeks)

import Html exposing (Html, option, text)
import Html.Attributes exposing (selected, value)
import Json.Decode as Decode
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..))
import Time.Extra as Time exposing (Interval(..))


monthData : Posix -> List Posix
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
    frontPad ++ Time.range Day 1 Time.utc monthStart nextMonthStart ++ endPad


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


durationDayPickedOrBetween : Posix -> Maybe Posix -> ( Maybe Posix, Maybe Posix ) -> ( Bool, Bool )
durationDayPickedOrBetween day hovered ( pickedStart, pickedEnd ) =
    case ( pickedStart, pickedEnd ) of
        ( Nothing, Nothing ) ->
            ( False, False )

        ( Just start, Nothing ) ->
            let
                picked =
                    doDaysMatch day start

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
                    doDaysMatch day end

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
                    doDaysMatch day end || doDaysMatch day start

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


doDaysMatch : Posix -> Posix -> Bool
doDaysMatch dateTimeOne dateTimeTwo =
    let
        oneParts =
            Time.posixToParts Time.utc dateTimeOne

        twoParts =
            Time.posixToParts Time.utc dateTimeTwo
    in
    oneParts.day == twoParts.day && oneParts.month == twoParts.month && oneParts.year == twoParts.year


{-| Set the day (month and year) of the previously selected dateTime to match that of the newly selected dateTime
-}
setDayNotTime : Posix -> Posix -> Posix
setDayNotTime newPickedDT prevPickedDT =
    let
        newPickedParts =
            Time.posixToParts Time.utc newPickedDT
    in
    Time.posixToParts Time.utc prevPickedDT |> (\parts -> { parts | day = newPickedParts.day, month = newPickedParts.month, year = newPickedParts.year }) |> Time.partsToPosix Time.utc


{-| Set only the hour of the provided dateTime
-}
setHourNotDay : Int -> Posix -> Posix
setHourNotDay hour timeToUpdate =
    let
        parts =
            Time.posixToParts Time.utc timeToUpdate

        newParts =
            { parts | hour = hour }
    in
    Time.partsToPosix Time.utc newParts


{-| Set only the minute of the provided dateTime
-}
setMinuteNotDay : Int -> Posix -> Posix
setMinuteNotDay minute timeToUpdate =
    let
        parts =
            Time.posixToParts Time.utc timeToUpdate

        newParts =
            { parts | minute = minute }
    in
    Time.partsToPosix Time.utc newParts


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


filterSelectableHours : Posix -> (Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }) -> List Int
filterSelectableHours dateTimeBeingProcessed allowableTimesFn =
    let
        { startHour, endHour } =
            allowableTimesFn dateTimeBeingProcessed
    in
    List.range startHour endHour


filterSelectableMinutes : Posix -> (Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }) -> List Int
filterSelectableMinutes dateTimeBeingProcessed allowableTimesFn =
    let
        { startHour, startMinute, endHour, endMinute } =
            allowableTimesFn dateTimeBeingProcessed

        hour =
            Time.toHour Time.utc dateTimeBeingProcessed

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


selectedAndSelectableTimeParts : Maybe Posix -> (Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }) -> { selectedHour : Int, selectableHours : List Int, selectedMinute : Int, selectableMinutes : List Int }
selectedAndSelectableTimeParts pickedDateTime allowableTimesFn =
    Maybe.map
        (\time ->
            let
                timeParts =
                    Time.posixToParts Time.utc time
            in
            { selectedHour = timeParts.hour
            , selectableHours = filterSelectableHours time allowableTimesFn
            , selectedMinute = timeParts.minute
            , selectableMinutes = filterSelectableMinutes time allowableTimesFn
            }
        )
        pickedDateTime
        |> Maybe.withDefault { selectedHour = 0, selectableHours = List.range 0 23, selectedMinute = 0, selectableMinutes = List.range 0 59 }


enforceTimeBoundaries : Posix -> (Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }) -> Posix
enforceTimeBoundaries dateTimeBeingProcessed allowableTimesFn =
    let
        { startHour, startMinute } =
            allowableTimesFn dateTimeBeingProcessed
    in
    Time.posixToParts Time.utc dateTimeBeingProcessed |> (\parts -> { parts | hour = startHour, minute = startMinute } |> Time.partsToPosix Time.utc)
