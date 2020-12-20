module DatePicker.DurationUtilities exposing (dayPickedOrBetween, filterSelectableTimes, selectDay, selectEndHour, selectEndMinute, selectStartHour, selectStartMinute)

import DatePicker.Utilities as Utilities exposing (PickerDay)
import Time exposing (Month(..), Posix, Weekday(..), Zone)


selectDay : Zone -> Bool -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> PickerDay -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
selectDay zone forDisplay previousStartSelectionTuple previousEndSelectionTuple selectedPickerDay =
    -- this function's logic already ensures valid selections, no need for additional validation
    case ( previousStartSelectionTuple, previousEndSelectionTuple ) of
        ( Just ( startPickerDay, previousSelectionStart ), Just ( endPickerDay, previousSelectionEnd ) ) ->
            if forDisplay then
                ( previousStartSelectionTuple, previousEndSelectionTuple )

            else if startPickerDay == selectedPickerDay && selectedPickerDay == endPickerDay then
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

            else if Utilities.selectionWithinPickerDayBoundaries zone selectedPickerDay selectionStart then
                -- keep previously picked time of day
                ( Just ( selectedPickerDay, Utilities.setTimeOfDay zone (Time.toHour zone selectionStart) (Time.toMinute zone selectionStart) selectedPickerDay.start )
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

            else if Utilities.selectionWithinPickerDayBoundaries zone selectedPickerDay selectionEnd then
                -- keep previously picked time of day
                ( Just ( endPickerDay, endPickerDay.start )
                , Just ( selectedPickerDay, Utilities.setTimeOfDay zone (Time.toHour zone selectionEnd) (Time.toMinute zone selectionEnd) selectedPickerDay.end )
                )

            else
                -- use end of picked day
                ( Just ( endPickerDay, endPickerDay.start )
                , Just ( selectedPickerDay, selectedPickerDay.end )
                )

        ( Nothing, Nothing ) ->
            ( Just ( selectedPickerDay, selectedPickerDay.start ), Nothing )


selectStartHour : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> Int -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
selectStartHour zone basePickerDay startSelectionTuple endSelectionTuple newStartHour =
    let
        ( selectedPickerDay, selection ) =
            case ( startSelectionTuple, endSelectionTuple ) of
                ( Just ( startPickerDay, startSelection ), _ ) ->
                    ( startPickerDay, Utilities.setHourNotDay zone newStartHour startSelection )

                ( Nothing, Just ( endPickerDay, endSelection ) ) ->
                    ( endPickerDay, Utilities.setHourNotDay zone newStartHour endSelection )

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
        |> (\subjectSelection -> newOrPreviousDuration ( startSelectionTuple, endSelectionTuple ) ( subjectSelection, endSelectionTuple ))


selectEndHour : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> Int -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
selectEndHour zone basePickerDay startSelectionTuple endSelectionTuple newEndHour =
    let
        ( selectedPickerDay, selection ) =
            case ( startSelectionTuple, endSelectionTuple ) of
                ( _, Just ( endPickerDay, endSelection ) ) ->
                    ( endPickerDay, Utilities.setHourNotDay zone newEndHour endSelection )

                ( Just ( startPickerDay, startSelection ), Nothing ) ->
                    ( startPickerDay, Utilities.setHourNotDay zone newEndHour startSelection )

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
        |> (\subjectSelection -> newOrPreviousDuration ( startSelectionTuple, endSelectionTuple ) ( startSelectionTuple, subjectSelection ))


selectStartMinute : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> Int -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
selectStartMinute zone basePickerDay startSelectionTuple endSelectionTuple newStartMinute =
    let
        ( selectedPickerDay, selection ) =
            case ( startSelectionTuple, endSelectionTuple ) of
                ( Just ( startPickerDay, startSelection ), _ ) ->
                    ( startPickerDay, Utilities.setMinuteNotDay zone newStartMinute startSelection )

                ( Nothing, Just ( endPickerDay, endSelection ) ) ->
                    ( endPickerDay, Utilities.setMinuteNotDay zone newStartMinute endSelection )

                ( Nothing, Nothing ) ->
                    ( basePickerDay, Utilities.setMinuteNotDay zone newStartMinute basePickerDay.start )
    in
    Utilities.validSelectionOrDefault zone startSelectionTuple ( selectedPickerDay, selection )
        |> (\subjectSelection -> newOrPreviousDuration ( startSelectionTuple, endSelectionTuple ) ( subjectSelection, endSelectionTuple ))


selectEndMinute : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> Int -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
selectEndMinute zone basePickerDay startSelectionTuple endSelectionTuple newEndMinute =
    let
        ( selectedPickerDay, selection ) =
            case ( startSelectionTuple, endSelectionTuple ) of
                ( _, Just ( endPickerDay, endSelection ) ) ->
                    ( endPickerDay, Utilities.setMinuteNotDay zone newEndMinute endSelection )

                ( Just ( startPickerDay, startSelection ), Nothing ) ->
                    ( startPickerDay, Utilities.setMinuteNotDay zone newEndMinute startSelection )

                ( Nothing, Nothing ) ->
                    ( basePickerDay, Utilities.setMinuteNotDay zone newEndMinute basePickerDay.end )
    in
    Utilities.validSelectionOrDefault zone endSelectionTuple ( selectedPickerDay, selection )
        |> (\subjectSelection -> newOrPreviousDuration ( startSelectionTuple, endSelectionTuple ) ( startSelectionTuple, subjectSelection ))


newOrPreviousDuration : ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) ) -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) ) -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
newOrPreviousDuration ( previousStart, previousEnd ) ( newStart, newEnd ) =
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
                    Utilities.hourBoundsForSelectedDay zone startPickerDay

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
                    Utilities.hourBoundsForSelectedDay zone endPickerDay

                ( earliestSelectableEndMinute, latestSelectableEndMinute ) =
                    Utilities.minuteBoundsForSelectedHour zone ( endPickerDay, endSelection )

                ( selectedEndHour, selectedEndMinute ) =
                    Utilities.timeOfDayFromPosix zone endSelection
            in
            { selectableStartHours = List.range earliestSelectableEndHour selectedEndHour
            , selectableStartMinutes = List.range earliestSelectableEndMinute selectedEndMinute
            , selectableEndHours = List.range earliestSelectableEndMinute latestSelectableEndHour
            , selectableEndMinutes = List.range earliestSelectableEndMinute latestSelectableEndMinute
            }

        ( Nothing, Nothing ) ->
            let
                ( earliestSelectableHour, latestSelectableHour ) =
                    Utilities.hourBoundsForSelectedDay zone baseDay

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
            Utilities.hourBoundsForSelectedDay zone startPickerDay

        ( earliestSelectableEndHour, latestSelectableEndHour ) =
            Utilities.hourBoundsForSelectedDay zone endPickerDay

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
