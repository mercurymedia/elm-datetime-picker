module DatePicker.DurationUtilities exposing
    ( previewSelection, selectDay, selectStartHour, selectEndHour, selectStartMinute, selectEndMinute
    , filterSelectableTimes, dayPickedOrBetween
    , isDayBetween, isDayPicked, isPickedDaySelectionTuple
    )

{-| Utility functions specific to the DurationDatePicker.


# Making a selection

@docs previewSelection, selectDay, selectStartHour, selectEndHour, selectStartMinute, selectEndMinute


# Queries

@docs filterSelectableTimes, dayPickedOrBetween

-}

import DatePicker.Utilities as Utilities exposing (PickerDay)
import Time exposing (Month(..), Posix, Weekday(..), Zone)


{-| Essentially the same as the `selectDay` function. However, if there
is a prior, valid duration selected, it simply returns the selection. Otherwise,
it calls the `selectDay` function.
-}
previewSelection : Zone -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> PickerDay -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
previewSelection zone previousStartSelectionTuple previousEndSelectionTuple hoveredPickerDay =
    case ( previousStartSelectionTuple, previousEndSelectionTuple ) of
        ( Just startSelectionTuple, Just endSelectionTuple ) ->
            ( Just startSelectionTuple, Just endSelectionTuple )

        _ ->
            selectDay zone previousStartSelectionTuple previousEndSelectionTuple hoveredPickerDay


{-| Select a day.

With prior selected start and end:

    if provided day == selected start day: deselect start day
    else if provided day == selected end day: deselect end day
    else: provided day becomes new start and end day is deselected

With only prior selected start:

    if selected day < selected start: selected day becomes new start
    and selected start becomes end

        if selected start time is selectable in new start day: time is maintained
        else: new selected start time is set to earliest selectable time in new start day

        end time is set to latest selectable time in prior start/new end day

    else if selected day >= selected start: selected day becomes selected end

        end time is set to latest selectable time in selected day

With only prior selected end:

    if selected day > selected end: selected day becomes new end
    and selected end becomes start

        if selected end time is selectable in new end day: time is maintained
        else: new selected end time is set to latest selectable time in new end day

        start time is set to earliest selectable time in prior end/new start day

    else if selected day <= selected end: selected day becomes selected start

        start time is set to earliest selectable time in selected day

With no prior selection:

    selected day becomes selected start

    start time is set to earliest selectable time in selected day

Returns a tuple representing the selected start and end selection tuples
A selection tuple contains the selected `PickerDay` and a `Posix` representing
the full selection (day + time of day).

-}
selectDay : Zone -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> PickerDay -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
selectDay zone previousStartSelectionTuple previousEndSelectionTuple selectedPickerDay =
    if selectedPickerDay.disabled then
        ( previousStartSelectionTuple, previousEndSelectionTuple )

    else
        case ( previousStartSelectionTuple, previousEndSelectionTuple ) of
            ( Just ( startPickerDay, previousSelectionStart ), Just ( endPickerDay, previousSelectionEnd ) ) ->
                if startPickerDay == selectedPickerDay && selectedPickerDay == endPickerDay then
                    ( Just ( startPickerDay, previousSelectionStart ), Nothing )

                else if startPickerDay == selectedPickerDay then
                    ( Nothing, Just ( endPickerDay, previousSelectionEnd ) )

                else if endPickerDay == selectedPickerDay then
                    ( Just ( startPickerDay, previousSelectionStart ), Nothing )

                else
                    ( Just ( selectedPickerDay, selectedPickerDay.start ), Nothing )

            ( Just ( startPickerDay, selectionStart ), Nothing ) ->
                if startPickerDay == selectedPickerDay || Time.posixToMillis startPickerDay.start < Time.posixToMillis selectedPickerDay.start then
                    ( Just ( startPickerDay, selectionStart ), Just ( selectedPickerDay, selectedPickerDay.end ) )

                else if Utilities.posixWithinPickerDayBoundaries zone selectedPickerDay selectionStart then
                    -- keep previously picked time of day
                    ( Just ( selectedPickerDay, Utilities.setTimeOfDay zone (Time.toHour zone selectionStart) (Time.toMinute zone selectionStart) 0 selectedPickerDay.start )
                    , Just ( startPickerDay, startPickerDay.end )
                    )

                else
                    -- use start of picked day
                    ( Just ( selectedPickerDay, selectedPickerDay.start )
                    , Just ( startPickerDay, startPickerDay.end )
                    )

            ( Nothing, Just ( endPickerDay, selectionEnd ) ) ->
                if endPickerDay == selectedPickerDay || Time.posixToMillis endPickerDay.start > Time.posixToMillis selectedPickerDay.end then
                    ( Just ( selectedPickerDay, selectedPickerDay.start ), Just ( endPickerDay, selectionEnd ) )

                else if Utilities.posixWithinPickerDayBoundaries zone selectedPickerDay selectionEnd then
                    -- keep previously picked time of day
                    ( Just ( endPickerDay, endPickerDay.start )
                    , Just ( selectedPickerDay, Utilities.setTimeOfDay zone (Time.toHour zone selectionEnd) (Time.toMinute zone selectionEnd) 59 selectedPickerDay.end )
                    )

                else
                    -- use end of picked day
                    ( Just ( endPickerDay, endPickerDay.start )
                    , Just ( selectedPickerDay, selectedPickerDay.end )
                    )

            ( Nothing, Nothing ) ->
                ( Just ( selectedPickerDay, selectedPickerDay.start ), Nothing )


{-| Select a start hour.

With a prior selected start:

    set: start hour -> provided hour

    if prior selected start minute is selectable in new hour: maintain selected minute
    else: select earliest selectable minute for new hour

With only a prior selected end:

    set: start day -> selected end day, start hour -> provided hour, start minute -> earliest
    selectable minute for provided hour

With no prior selection:

    set: start day -> base day, start hour -> provided hour, start minute -> earliest
    selectable minute for provided hour

If the resulting selected time is not valid, the prior selection is returned instead.

If the resulting selected duration is not valid, the prior selected duration is returned instead.

Returns a tuple representing the selected start and end selection tuples
A selection tuple contains the selected `PickerDay` and a `Posix` representing
the full selection (day + time of day).

-}
selectStartHour : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> Int -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
selectStartHour zone basePickerDay startSelectionTuple endSelectionTuple newStartHour =
    let
        ( selectedPickerDay, selection ) =
            case ( startSelectionTuple, endSelectionTuple ) of
                ( Just ( startPickerDay, startSelection ), _ ) ->
                    let
                        updatedHourSelection =
                            Utilities.setHourNotDay zone newStartHour startSelection

                        ( earliestSelectableMinute, _ ) =
                            Utilities.minuteBoundsForSelectedHour zone ( startPickerDay, updatedHourSelection )
                    in
                    if Time.toMinute zone startSelection < earliestSelectableMinute then
                        -- only select earliest selectable minute for hour if prior
                        -- selected minute is less than the earliest selectable minute
                        ( startPickerDay, Utilities.setMinuteNotDay zone earliestSelectableMinute updatedHourSelection )

                    else
                        -- otherwise, keep prior selected minute
                        ( startPickerDay, updatedHourSelection )

                ( Nothing, Just ( endPickerDay, endSelection ) ) ->
                    let
                        updatedHourSelection =
                            Utilities.setHourNotDay zone newStartHour endSelection

                        ( earliestSelectableMinute, _ ) =
                            Utilities.minuteBoundsForSelectedHour zone ( endPickerDay, updatedHourSelection )
                    in
                    ( endPickerDay, Utilities.setMinuteNotDay zone earliestSelectableMinute updatedHourSelection )

                ( Nothing, Nothing ) ->
                    let
                        updatedHourSelection =
                            Utilities.setHourNotDay zone newStartHour basePickerDay.start

                        ( earliestSelectableMinute, _ ) =
                            Utilities.minuteBoundsForSelectedHour zone ( basePickerDay, updatedHourSelection )
                    in
                    ( basePickerDay, Utilities.setMinuteNotDay zone earliestSelectableMinute updatedHourSelection )
    in
    Utilities.validSelectionOrDefault zone startSelectionTuple ( selectedPickerDay, selection )
        |> (\subjectSelection -> validDurationSelectionOrDefault ( startSelectionTuple, endSelectionTuple ) ( subjectSelection, endSelectionTuple ))


{-| Select an end hour.

With a prior selected end:

    set: end hour -> provided hour

    if prior selected end minute is selectable in new hour: maintain selected minute
    else: select latest selectable minute for new hour

With only a prior selected start:

    set: end day -> selected start day, end hour -> provided hour, end minute -> latest
    selectable minute for provided hour

With no prior selection:

    set: end day -> base day, end hour -> provided hour, end minute -> latest
    selectable minute for provided hour

If the resulting selected time is not valid, the prior selection is returned instead.

If the resulting selected duration is not valid, the prior selected duration is returned instead.

Returns a tuple representing the selected start and end selection tuples
A selection tuple contains the selected `PickerDay` and a `Posix` representing
the full selection (day + time of day).

-}
selectEndHour : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> Int -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
selectEndHour zone basePickerDay startSelectionTuple endSelectionTuple newEndHour =
    let
        ( selectedPickerDay, selection ) =
            case ( startSelectionTuple, endSelectionTuple ) of
                ( _, Just ( endPickerDay, endSelection ) ) ->
                    let
                        updatedHourSelection =
                            Utilities.setHourNotDay zone newEndHour endSelection

                        ( _, latestSelectableMinute ) =
                            Utilities.minuteBoundsForSelectedHour zone ( endPickerDay, updatedHourSelection )
                    in
                    if Time.toMinute zone endSelection > latestSelectableMinute then
                        -- only select latest selectable minute for hour if prior
                        -- selected minute is greater than the latest selectable minute
                        ( endPickerDay, Utilities.setMinuteNotDay zone latestSelectableMinute updatedHourSelection )

                    else
                        -- otherwise, keep prior selected minute
                        ( endPickerDay, updatedHourSelection )

                ( Just ( startPickerDay, startSelection ), Nothing ) ->
                    let
                        updatedHourSelection =
                            Utilities.setHourNotDay zone newEndHour startPickerDay.end

                        ( _, latestSelectableMinute ) =
                            Utilities.minuteBoundsForSelectedHour zone ( startPickerDay, updatedHourSelection )
                    in
                    ( startPickerDay, Utilities.setMinuteNotDay zone latestSelectableMinute updatedHourSelection )

                ( Nothing, Nothing ) ->
                    let
                        updatedHourSelection =
                            Utilities.setHourNotDay zone newEndHour basePickerDay.end

                        ( _, latestSelectableMinute ) =
                            Utilities.minuteBoundsForSelectedHour zone ( basePickerDay, updatedHourSelection )
                    in
                    ( basePickerDay, Utilities.setMinuteNotDay zone latestSelectableMinute updatedHourSelection )
    in
    Utilities.validSelectionOrDefault zone endSelectionTuple ( selectedPickerDay, selection )
        |> (\subjectSelection -> validDurationSelectionOrDefault ( startSelectionTuple, endSelectionTuple ) ( startSelectionTuple, subjectSelection ))


{-| Select a start minute.

With a prior selected start:

    set: start minute -> provided minute

With only a prior selected end:

    set: start day -> selected end day, start hour -> selected end day start hour, start minute -> provided minute

With no prior selection:

    set: start day -> base day, start hour -> base day start hour, start minute -> provided minute

If the resulting selected time is not valid, the prior selection is returned instead.

If the resulting selected duration is not valid, the prior selected duration is returned instead.

Returns a tuple representing the selected start and end selection tuples
A selection tuple contains the selected `PickerDay` and a `Posix` representing
the full selection (day + time of day).

-}
selectStartMinute : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> Int -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
selectStartMinute zone basePickerDay startSelectionTuple endSelectionTuple newStartMinute =
    let
        ( selectedPickerDay, selection ) =
            case ( startSelectionTuple, endSelectionTuple ) of
                ( Just ( startPickerDay, startSelection ), _ ) ->
                    ( startPickerDay, Utilities.setMinuteNotDay zone newStartMinute startSelection )

                ( Nothing, Just ( endPickerDay, endSelection ) ) ->
                    ( endPickerDay, Utilities.setMinuteNotDay zone newStartMinute endPickerDay.start )

                ( Nothing, Nothing ) ->
                    ( basePickerDay, Utilities.setMinuteNotDay zone newStartMinute basePickerDay.start )
    in
    Utilities.validSelectionOrDefault zone startSelectionTuple ( selectedPickerDay, selection )
        |> (\subjectSelection -> validDurationSelectionOrDefault ( startSelectionTuple, endSelectionTuple ) ( subjectSelection, endSelectionTuple ))


{-| Select an end minute.

With a prior selected end:

    set: end minute -> provided minute

With only a prior selected start:

    set: end day -> selected start day, end hour -> selected start day end hour, end minute -> provided minute

With no prior selection:

    set: end day -> base day, end hour -> base day end hour, end minute -> provided minute

If the resulting selected time is not valid, the prior selection is returned instead.

If the resulting selected duration is not valid, the prior selected duration is returned instead.

Returns a tuple representing the selected start and end selection tuples
A selection tuple contains the selected `PickerDay` and a `Posix` representing
the full selection (day + time of day).

-}
selectEndMinute : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> Int -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
selectEndMinute zone basePickerDay startSelectionTuple endSelectionTuple newEndMinute =
    let
        ( selectedPickerDay, selection ) =
            case ( startSelectionTuple, endSelectionTuple ) of
                ( _, Just ( endPickerDay, endSelection ) ) ->
                    ( endPickerDay, Utilities.setMinuteNotDay zone newEndMinute endSelection )

                ( Just ( startPickerDay, startSelection ), Nothing ) ->
                    ( startPickerDay, Utilities.setMinuteNotDay zone newEndMinute startPickerDay.end )

                ( Nothing, Nothing ) ->
                    ( basePickerDay, Utilities.setMinuteNotDay zone newEndMinute basePickerDay.end )
    in
    Utilities.validSelectionOrDefault zone endSelectionTuple ( selectedPickerDay, selection )
        |> (\subjectSelection -> validDurationSelectionOrDefault ( startSelectionTuple, endSelectionTuple ) ( startSelectionTuple, subjectSelection ))


validDurationSelectionOrDefault : ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) ) -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) ) -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
validDurationSelectionOrDefault ( previousStart, previousEnd ) ( newStart, newEnd ) =
    case ( newStart, newEnd ) of
        ( Just ( _, startSelection ), Just ( _, endSelection ) ) ->
            if Time.posixToMillis startSelection < Time.posixToMillis endSelection then
                ( newStart, newEnd )

            else
                ( previousStart, previousEnd )

        _ ->
            -- when there aren't two selections there can't be a duration
            -- so just return the new stuff, whatever it may be
            ( newStart, newEnd )


{-| Determine if a calendar day is selected or between selections.

Returns a tuple with two `Bool`s.

The first indicates if the day is selected; the second indicates if
the day is between selections.

-}
dayPickedOrBetween : Zone -> PickerDay -> Maybe PickerDay -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) ) -> ( Bool, Bool )
dayPickedOrBetween zone day hovered ( startSelectionTuple, endSelectionTuple ) =
    case ( startSelectionTuple, endSelectionTuple ) of
        ( Nothing, Nothing ) ->
            ( False, False )

        ( Just ( startPickerDay, _ ), Nothing ) ->
            let
                between =
                    Maybe.map (\h -> isDayBetweenDates day startPickerDay h) hovered
                        |> Maybe.withDefault False
            in
            ( day == startPickerDay, between )

        ( Nothing, Just ( endPickerDay, _ ) ) ->
            let
                between =
                    Maybe.map (\h -> isDayBetweenDates day endPickerDay h) hovered
                        |> Maybe.withDefault False
            in
            ( day == endPickerDay, between )

        ( Just ( startPickerDay, _ ), Just ( endPickerDay, _ ) ) ->
            ( day == startPickerDay || day == endPickerDay, isDayBetweenDates day startPickerDay endPickerDay )


isDayPicked : Zone -> PickerDay -> Maybe PickerDay -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) ) -> Bool
isDayPicked zone day hovered ( startSelectionTuple, endSelectionTuple ) =
    let
        ( isPicked, _ ) =
            dayPickedOrBetween zone day hovered ( startSelectionTuple, endSelectionTuple )
    in
    isPicked


isDayBetween : Zone -> PickerDay -> Maybe PickerDay -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) ) -> Bool
isDayBetween zone day hovered ( startSelectionTuple, endSelectionTuple ) =
    let
        ( _, isBetween ) =
            dayPickedOrBetween zone day hovered ( startSelectionTuple, endSelectionTuple )
    in
    isBetween


isPickedDaySelectionTuple : PickerDay -> Maybe ( PickerDay, Posix ) -> Bool
isPickedDaySelectionTuple day selectionTuple =
    case selectionTuple of
        Just ( pickerDay, _ ) ->
            pickerDay == day

        _ ->
            False


isDayBetweenDates : PickerDay -> PickerDay -> PickerDay -> Bool
isDayBetweenDates day dateOne dateTwo =
    (Time.posixToMillis dateOne.start
        > Time.posixToMillis day.end
        && Time.posixToMillis day.start
        > Time.posixToMillis dateTwo.end
    )
        || (Time.posixToMillis dateOne.end
                < Time.posixToMillis day.start
                && Time.posixToMillis day.end
                < Time.posixToMillis dateTwo.start
           )


{-| Determine the selectable start and end hours as well as
the selectable start and end minutes for the selected start and
end datetimes.
-}
filterSelectableTimes : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> { selectableStartHours : List Int, selectableStartMinutes : List Int, selectableEndHours : List Int, selectableEndMinutes : List Int }
filterSelectableTimes zone baseDay startSelectionTuple endSelectionTuple =
    case ( startSelectionTuple, endSelectionTuple ) of
        ( Just ( startPickerDay, startSelection ), Just ( endPickerDay, endSelection ) ) ->
            let
                ( selectableStartHours, selectableEndHours ) =
                    selectableHours zone ( startPickerDay, startSelection ) ( endPickerDay, endSelection )

                ( selectableStartMinutes, selectableEndMinutes ) =
                    selectableMinutes zone ( startPickerDay, startSelection ) ( endPickerDay, endSelection )
            in
            { selectableStartHours = selectableStartHours
            , selectableStartMinutes = selectableStartMinutes
            , selectableEndHours = selectableEndHours
            , selectableEndMinutes = selectableEndMinutes
            }

        ( Just ( startPickerDay, startSelection ), Nothing ) ->
            let
                ( earliestSelectableStartHour, latestSelectableStartHour ) =
                    Utilities.hourBoundsForSelectedMinute zone ( startPickerDay, startSelection )

                ( earliestSelectableStartMinute, latestSelectableStartMinute ) =
                    Utilities.minuteBoundsForSelectedHour zone ( startPickerDay, startSelection )

                ( selectedStartHour, selectedStartMinute ) =
                    Utilities.timeOfDayFromPosix zone startSelection
            in
            { selectableStartHours = List.range earliestSelectableStartHour latestSelectableStartHour
            , selectableStartMinutes = List.range earliestSelectableStartMinute latestSelectableStartMinute
            , selectableEndHours = List.range selectedStartHour latestSelectableStartHour
            , selectableEndMinutes = List.range selectedStartMinute latestSelectableStartMinute
            }

        ( Nothing, Just ( endPickerDay, endSelection ) ) ->
            let
                ( earliestSelectableEndHour, latestSelectableEndHour ) =
                    Utilities.hourBoundsForSelectedMinute zone ( endPickerDay, endSelection )

                ( earliestSelectableEndMinute, latestSelectableEndMinute ) =
                    Utilities.minuteBoundsForSelectedHour zone ( endPickerDay, endSelection )

                ( selectedEndHour, selectedEndMinute ) =
                    Utilities.timeOfDayFromPosix zone endSelection
            in
            { selectableStartHours = List.range earliestSelectableEndHour selectedEndHour
            , selectableStartMinutes = List.range earliestSelectableEndMinute selectedEndMinute
            , selectableEndHours = List.range earliestSelectableEndHour latestSelectableEndHour
            , selectableEndMinutes = List.range earliestSelectableEndMinute latestSelectableEndMinute
            }

        ( Nothing, Nothing ) ->
            let
                ( earliestSelectableHour, latestSelectableHour ) =
                    Utilities.hourBoundsForSelectedMinute zone ( baseDay, baseDay.start )

                ( earliestSelectableMinute, latestSelectableMinute ) =
                    Utilities.minuteBoundsForSelectedHour zone ( baseDay, baseDay.start )
            in
            { selectableStartHours = List.range earliestSelectableHour latestSelectableHour
            , selectableStartMinutes = List.range earliestSelectableMinute latestSelectableMinute
            , selectableEndHours = List.range earliestSelectableHour latestSelectableHour
            , selectableEndMinutes = List.range earliestSelectableMinute latestSelectableMinute
            }


selectableHours : Zone -> ( PickerDay, Posix ) -> ( PickerDay, Posix ) -> ( List Int, List Int )
selectableHours zone ( startPickerDay, startSelection ) ( endPickerDay, endSelection ) =
    let
        ( earliestSelectableStartHour, latestSelectableStartHour ) =
            Utilities.hourBoundsForSelectedMinute zone ( startPickerDay, startSelection )

        ( earliestSelectableEndHour, latestSelectableEndHour ) =
            Utilities.hourBoundsForSelectedMinute zone ( endPickerDay, endSelection )

        selectedStartHour =
            Time.toHour zone startSelection

        selectedEndHour =
            Time.toHour zone endSelection
    in
    if startPickerDay == endPickerDay then
        ( List.range earliestSelectableStartHour selectedEndHour, List.range selectedStartHour latestSelectableEndHour )

    else
        ( List.range earliestSelectableStartHour latestSelectableStartHour, List.range earliestSelectableEndHour latestSelectableEndHour )


selectableMinutes : Zone -> ( PickerDay, Posix ) -> ( PickerDay, Posix ) -> ( List Int, List Int )
selectableMinutes zone ( startPickerDay, startSelection ) ( endPickerDay, endSelection ) =
    let
        ( earliestSelectableStartMinute, latestSelectableStartMinute ) =
            Utilities.minuteBoundsForSelectedHour zone ( startPickerDay, startSelection )

        ( earliestSelectableEndMinute, latestSelectableEndMinute ) =
            Utilities.minuteBoundsForSelectedHour zone ( endPickerDay, endSelection )

        ( selectedStartHour, selectedStartMinute ) =
            Utilities.timeOfDayFromPosix zone startSelection

        ( selectedEndHour, selectedEndMinute ) =
            Utilities.timeOfDayFromPosix zone endSelection
    in
    if startPickerDay == endPickerDay && selectedStartHour == selectedEndHour then
        -- if duration starts and ends on same hour of same day:
        -- selectable start minutes: FROM earliest selectable minute of selected start hour in selected start day TO selected minute of selected end hour in selected end day
        -- selectable end minutes: FROM selected minute of selected start hour in selected start day TO latest selectable minute of selected end hour in selected end day
        ( List.range earliestSelectableStartMinute selectedEndMinute, List.range selectedStartMinute latestSelectableEndMinute )

    else
        -- otherwise
        -- selectable start minutes: FROM earliest selectable minute of selected start hour in selected start day TO latest selectable minute of selected start hour in selected start day
        -- selectable end minutes: FROM earliest selectable minute of selected end hour in selected end day TO latest selectable minute of selected end hour in selected end day
        ( List.range earliestSelectableStartMinute latestSelectableStartMinute, List.range earliestSelectableEndMinute latestSelectableEndMinute )
