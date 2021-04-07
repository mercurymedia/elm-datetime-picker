module DatePicker.Utilities exposing
    ( PickerDay, monthData, generateHourOptions, generateMinuteOptions, generateListOfWeekDay
    , pickerDayFromPosix, timeOfDayFromPosix, monthToNameString, dayToNameString
    , setTimeOfDay, setHourNotDay, setMinuteNotDay
    , calculateViewOffset, eventIsOutsideComponent, hourBoundsForSelectedMinute, minuteBoundsForSelectedHour, posixWithinPickerDayBoundaries, validSelectionOrDefault
    )

{-| Utility functions for both Pickers.


# View Types & Functions

@docs PickerDay, monthData, generateHourOptions, generateMinuteOptions, generateListOfWeekDay


# Conversions

@docs pickerDayFromPosix, timeOfDayFromPosix, monthToNameString, dayToNameString


# Making a selection

@docs setTimeOfDay, setHourNotDay, setMinuteNotDay


# Queries

@docs calculateViewOffset, eventIsOutsideComponent, hourBoundsForSelectedMinute, minuteBoundsForSelectedHour, posixWithinPickerDayBoundaries, validSelectionOrDefault

-}

import Html exposing (Html, option, text, th, time)
import Html.Attributes exposing (disabled, selected, value)
import Json.Decode as Decode
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra as Time exposing (Interval(..))



-- VIEW TYPES & FUNCTIONS


{-| The type representing a day within the picker.

Includes selectable time boundaries & whether or
not the day is disabled (not selectable).

-}
type alias PickerDay =
    { start : Posix
    , end : Posix
    , disabled : Bool
    }


{-| Generate a month to be rendered by the picker
based on the provided `Posix` time.

Returns a list of weeks which are themselves a
list of `PickerDay`s

-}
monthData :
    Zone
    -> (Zone -> Posix -> Bool)
    -> Weekday
    -> Maybe (Zone -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int })
    -> Posix
    -> List (List PickerDay)
monthData zone isDisabledFn firstWeekDay allowableTimesFn time =
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
            Time.range Day 1 zone (Time.add Day (calculatePad firstWeekDay monthStartDay True) zone monthStart) monthStart

        endPad =
            Time.range Day 1 zone nextMonthStart (Time.add Day (calculatePad firstWeekDay nextMonthStartDay False) zone nextMonthStart)
    in
    (frontPad
        ++ Time.range Day 1 zone monthStart nextMonthStart
        ++ endPad
    )
        |> monthDataToPickerDays zone isDisabledFn allowableTimesFn
        |> splitIntoWeeks []
        |> List.reverse


monthDataToPickerDays :
    Zone
    -> (Zone -> Posix -> Bool)
    -> Maybe (Zone -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int })
    -> List Posix
    -> List PickerDay
monthDataToPickerDays zone isDisabledFn allowableTimesFn posixList =
    List.map
        (\posix ->
            pickerDayFromPosix zone isDisabledFn allowableTimesFn posix
        )
        posixList


splitIntoWeeks : List (List PickerDay) -> List PickerDay -> List (List PickerDay)
splitIntoWeeks weeks days =
    if List.length days <= 7 then
        days :: weeks

    else
        let
            ( week, restOfDays ) =
                List.splitAt 7 days

            newWeeks =
                week :: weeks
        in
        splitIntoWeeks newWeeks restOfDays


{-| Generate a list of Weekday based on the first week day
set in the settings.
-}
generateListOfWeekDay : Weekday -> List Weekday
generateListOfWeekDay firstWeekDay =
    case firstWeekDay of
        Mon ->
            [ Mon, Tue, Wed, Thu, Fri, Sat, Sun ]

        Tue ->
            [ Tue, Wed, Thu, Fri, Sat, Sun, Mon ]

        Wed ->
            [ Wed, Thu, Fri, Sat, Sun, Mon, Tue ]

        Thu ->
            [ Thu, Fri, Sat, Sun, Mon, Tue, Wed ]

        Fri ->
            [ Fri, Sat, Sun, Mon, Tue, Wed, Thu ]

        Sat ->
            [ Sat, Sun, Mon, Tue, Wed, Thu, Fri ]

        Sun ->
            [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]


{-| Generate a list of Html `option`s representing
selectable hours based on the provided selectable hours
list.
-}
generateHourOptions : Zone -> Maybe ( PickerDay, Posix ) -> List Int -> List (Html msg)
generateHourOptions zone selectionTuple selectableHours =
    let
        isSelected =
            \h -> Maybe.map (\( _, selection ) -> Time.toHour zone selection == h) selectionTuple |> Maybe.withDefault False
    in
    selectableHours
        |> List.map (\hour -> option [ value (String.fromInt hour), selected (isSelected hour) ] [ text (addLeadingZero hour) ])


{-| Generate a list of Html `option`s representing
selectable minutes based on the provided selectable minutes
list.
-}
generateMinuteOptions : Zone -> Maybe ( PickerDay, Posix ) -> List Int -> List (Html msg)
generateMinuteOptions zone selectionTuple selectableMinutes =
    let
        isSelected =
            \m -> Maybe.map (\( _, selection ) -> Time.toMinute zone selection == m) selectionTuple |> Maybe.withDefault False
    in
    selectableMinutes
        |> List.map (\minute -> option [ value (String.fromInt minute), selected (isSelected minute) ] [ text (addLeadingZero minute) ])


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



-- CONVERSIONS


{-| Convert the provided `Posix` into a `PickerDay`.
Uses the provided functions to determine whether or
not the day should be disabled in the picker as well
as to determine the allowable times of day.

If no allowable times function is provided the boundary
times default to the start and end of the day (i.e.
00:00 & 23:59)

-}
pickerDayFromPosix :
    Zone
    -> (Zone -> Posix -> Bool)
    -> Maybe (Zone -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int })
    -> Posix
    -> PickerDay
pickerDayFromPosix zone isDisabledFn allowableTimesFn posix =
    let
        flooredPosix =
            Time.floor Day zone posix

        allowableTimes =
            Maybe.map (\fn -> fn zone flooredPosix) allowableTimesFn
                |> Maybe.withDefault { startHour = 0, startMinute = 0, endHour = 23, endMinute = 59 }
    in
    { start = setTimeOfDay zone allowableTimes.startHour allowableTimes.startMinute flooredPosix
    , end = setTimeOfDay zone allowableTimes.endHour allowableTimes.endMinute flooredPosix
    , disabled = isDisabledFn zone (Time.floor Day zone flooredPosix)
    }


{-| Convert the provided `Posix` into a tuple of
integers representing the selected hour of day and
minute of the hour, in that order.
-}
timeOfDayFromPosix : Zone -> Posix -> ( Int, Int )
timeOfDayFromPosix zone posix =
    ( Time.toHour zone posix, Time.toMinute zone posix )


{-| Convert the provided `Month` type into a string
representing the `Month`'s name.
-}
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


{-| Convert the provided `Weekday` type into a string
representing the `Weekday`'s name.
-}
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



-- MAKING A SELECTION


{-| Set the hour and minute of the provided dateTime.
-}
setTimeOfDay : Zone -> Int -> Int -> Posix -> Posix
setTimeOfDay zone hour minute timeToUpdate =
    let
        parts =
            Time.posixToParts zone timeToUpdate

        newParts =
            { parts | hour = hour, minute = minute }
    in
    Time.partsToPosix zone newParts


{-| Set only the hour of the provided dateTime.
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


{-| Set only the minute of the provided dateTime.
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



-- QUERIES


{-| Determine the offset in months between the base/reference time
and the selected time, if any.
-}
calculateViewOffset : Zone -> Posix -> Maybe Posix -> Int
calculateViewOffset zone referenceTime subjectTime =
    let
        flooredReference =
            Time.floor Month zone referenceTime
    in
    case subjectTime of
        Nothing ->
            0

        Just time ->
            let
                flooredSubject =
                    Time.floor Month zone time
            in
            if Time.posixToMillis flooredReference <= Time.posixToMillis flooredSubject then
                Time.diff Month zone flooredReference flooredSubject

            else
                0 - Time.diff Month zone flooredSubject flooredReference


{-| Determine if the user has clicked outside of the datepicker component.
-}
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


{-| Determine the start and end hour boundaries for the selected minute of hour of day.
-}
hourBoundsForSelectedMinute : Zone -> ( PickerDay, Posix ) -> ( Int, Int )
hourBoundsForSelectedMinute zone ( pickerDay, selection ) =
    let
        ( startBoundaryHour, startBoundaryMinute ) =
            timeOfDayFromPosix zone pickerDay.start

        ( endBoundaryHour, endBoundaryMinute ) =
            timeOfDayFromPosix zone pickerDay.end

        ( _, selectedMinute ) =
            timeOfDayFromPosix zone selection

        earliestSelectableHour =
            if selectedMinute < startBoundaryMinute then
                -- it start and end hour bounds are same hour
                -- it is impossible to select an invalid minute
                -- for an hour option, so we can safely assume
                -- that is not the case here and bump the earliest
                -- selectable hour back by one
                startBoundaryHour + 1

            else
                startBoundaryHour

        latestSelectableHour =
            if selectedMinute > endBoundaryMinute then
                -- it start and end hour bounds are same hour
                -- it is impossible to select an invalid minute
                -- for an hour option, so we can safely assume
                -- that is not the case here and bump the latest
                -- selectable hour forward by one
                endBoundaryHour - 1

            else
                endBoundaryHour
    in
    ( earliestSelectableHour, latestSelectableHour )


{-| Determine the start and end minute boundaries for the selected hour of day.
-}
minuteBoundsForSelectedHour : Zone -> ( PickerDay, Posix ) -> ( Int, Int )
minuteBoundsForSelectedHour zone ( pickerDay, selection ) =
    let
        ( startBoundaryHour, startBoundaryMinute ) =
            timeOfDayFromPosix zone pickerDay.start

        ( endBoundaryHour, endBoundaryMinute ) =
            timeOfDayFromPosix zone pickerDay.end

        ( selectedHour, selectedMinute ) =
            timeOfDayFromPosix zone selection

        earliestSelectableMinute =
            if startBoundaryHour == selectedHour then
                startBoundaryMinute

            else
                0

        latestSelectableMinute =
            if endBoundaryHour == selectedHour then
                endBoundaryMinute

            else
                59
    in
    ( earliestSelectableMinute, latestSelectableMinute )


{-| Determine if the provided `Posix` falls within the provided `PickerDay` time boundaries.

Will return True even if the `Posix` is from a different calendar day. All this function
cares about is that the provided `Posix` time of day falls within the \`PickerDay\`\`
allowable times.

-}
posixWithinPickerDayBoundaries : Zone -> PickerDay -> Posix -> Bool
posixWithinPickerDayBoundaries zone pickerDay selection =
    let
        ( startHour, startMinute ) =
            timeOfDayFromPosix zone pickerDay.start

        ( endHour, endMinute ) =
            timeOfDayFromPosix zone pickerDay.end

        ( selectionHour, selectionMinute ) =
            timeOfDayFromPosix zone selection
    in
    (startHour == selectionHour && startMinute <= selectionMinute)
        || (startHour < selectionHour && selectionHour < endHour)
        || (selectionHour == endHour && selectionMinute <= endMinute)


{-| Determine if the provided `Posix` falls within the provided `PickerDay`.
-}
validSelectionOrDefault : Zone -> Maybe ( PickerDay, Posix ) -> ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix )
validSelectionOrDefault zone default ( selectionPickerDay, selection ) =
    let
        selectionDayEqualsPickerDay =
            doDaysMatch zone selection selectionPickerDay.start
    in
    if posixWithinPickerDayBoundaries zone selectionPickerDay selection && not selectionPickerDay.disabled && selectionDayEqualsPickerDay then
        Just ( selectionPickerDay, selection )

    else
        default


doDaysMatch : Zone -> Posix -> Posix -> Bool
doDaysMatch zone dateTimeOne dateTimeTwo =
    let
        oneParts =
            Time.posixToParts zone dateTimeOne

        twoParts =
            Time.posixToParts zone dateTimeTwo
    in
    oneParts.day == twoParts.day && oneParts.month == twoParts.month && oneParts.year == twoParts.year


calculatePad : Weekday -> Weekday -> Bool -> Int
calculatePad firstWeekDay monthStartDay isFrontPad =
    let
        listOfWeekday =
            generateListOfWeekDay firstWeekDay

        calculatedPadInt =
            case List.elemIndex monthStartDay listOfWeekday of
                Just val ->
                    if isFrontPad then
                        -val

                    else
                        7 - val

                Nothing ->
                    0
    in
    calculatedPadInt
