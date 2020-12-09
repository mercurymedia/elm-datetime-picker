module DatePicker.Utilities exposing (PickerDay, calculateViewOffset, dayToNameString, eventIsOutsideComponent, generateHourOptions, generateMinuteOptions, hourBoundsForSelectedDay, minuteBoundsForSelectedHour, monthData, monthToNameString, newOrPreviousSelection, pickerDayFromPosix, selectionWithinPickerDayBoundaries, setHourNotDay, setMinuteNotDay, setTimeOfDay, timeOfDayFromPosix)

import Html exposing (Html, option, text, th, time)
import Html.Attributes exposing (disabled, selected, value)
import Json.Decode as Decode
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra as Time exposing (Interval(..))


monthData :
    Zone
    -> (Zone -> Posix -> Bool)
    -> Maybe (Zone -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int })
    -> Posix
    -> List (List PickerDay)
monthData zone isDisabledFn allowableTimesFn time =
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
    (frontPad
        ++ Time.range Day 1 zone monthStart nextMonthStart
        ++ endPad
    )
        |> monthDataToPickerDays zone isDisabledFn allowableTimesFn
        |> splitIntoWeeks []
        |> List.reverse


type alias PickerDay =
    { start : Posix
    , end : Posix
    , disabled : Bool
    }


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


newOrPreviousSelection : Zone -> Maybe ( PickerDay, Posix ) -> ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix )
newOrPreviousSelection zone previousSelection ( newSelectionPickerDay, newSelection ) =
    if selectionWithinPickerDayBoundaries zone newSelectionPickerDay newSelection then
        Just ( newSelectionPickerDay, newSelection )

    else
        previousSelection


selectionWithinPickerDayBoundaries : Zone -> PickerDay -> Posix -> Bool
selectionWithinPickerDayBoundaries zone pickerDay selection =
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


timeOfDayFromPosix : Zone -> Posix -> ( Int, Int )
timeOfDayFromPosix zone posix =
    ( Time.toHour zone posix, Time.toMinute zone posix )


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


setTimeOfDay : Zone -> Int -> Int -> Posix -> Posix
setTimeOfDay zone hour minute timeToUpdate =
    let
        parts =
            Time.posixToParts zone timeToUpdate

        newParts =
            { parts | hour = hour, minute = minute }
    in
    Time.partsToPosix zone newParts


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


generateHourOptions : Zone -> List Int -> Maybe ( PickerDay, Posix ) -> List (Html msg)
generateHourOptions zone selectableHours selectionTuple =
    let
        isSelected =
            \h -> Maybe.map (\( _, selection ) -> Time.toHour zone selection == h) selectionTuple |> Maybe.withDefault False
    in
    selectableHours
        |> List.map (\hour -> option [ value (String.fromInt hour), selected (isSelected hour) ] [ text (addLeadingZero hour) ])


generateMinuteOptions : Zone -> List Int -> Maybe ( PickerDay, Posix ) -> List (Html msg)
generateMinuteOptions zone selectableMinutes selectionTuple =
    let
        isSelected =
            \m -> Maybe.map (\( _, selection ) -> Time.toMinute zone selection == m) selectionTuple |> Maybe.withDefault False
    in
    selectableMinutes
        |> List.map (\minute -> option [ value (String.fromInt minute), selected (isSelected minute) ] [ text (addLeadingZero minute) ])


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


hourBoundsForSelectedDay : Zone -> PickerDay -> ( Int, Int )
hourBoundsForSelectedDay zone pickerDay =
    ( Time.toHour zone pickerDay.start, Time.toHour zone pickerDay.end )


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
