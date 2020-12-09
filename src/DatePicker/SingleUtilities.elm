module DatePicker.SingleUtilities exposing (filterSelectableTimes, selectDay, selectHour, selectMinute)

import DatePicker.Utilities as Utilities exposing (PickerDay)
import Time exposing (Month(..), Posix, Weekday(..), Zone)


selectDay : Zone -> Maybe ( PickerDay, Posix ) -> PickerDay -> ( PickerDay, Posix )
selectDay zone previousSelectionTuple selectedPickerDay =
    -- this function's logic already ensures valid selections, no need for additional validation
    Maybe.map
        (\( _, previousSelection ) ->
            if Utilities.selectionWithinPickerDayBoundaries zone selectedPickerDay previousSelection then
                -- keep previously picked time of day
                ( selectedPickerDay, Utilities.setTimeOfDay zone (Time.toHour zone previousSelection) (Time.toMinute zone previousSelection) selectedPickerDay.start )

            else
                -- use start of picked day
                ( selectedPickerDay, selectedPickerDay.start )
        )
        previousSelectionTuple
        |> Maybe.withDefault ( selectedPickerDay, selectedPickerDay.start )


selectHour : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Int -> Maybe ( PickerDay, Posix )
selectHour zone basePickerDay selectionTuple newHour =
    let
        ( selectedPickerDay, updatedSelection ) =
            case selectionTuple of
                Just ( pickerDay, selection ) ->
                    ( pickerDay, Utilities.setHourNotDay zone newHour selection )

                Nothing ->
                    let
                        updatedHourSelection =
                            Utilities.setHourNotDay zone newHour basePickerDay.start

                        ( earliestSelectableMinute, _ ) =
                            Utilities.minuteBoundsForSelectedHour zone ( basePickerDay, updatedHourSelection )
                    in
                    ( basePickerDay, Utilities.setMinuteNotDay zone earliestSelectableMinute updatedHourSelection )
    in
    Utilities.newOrPreviousSelection zone selectionTuple ( selectedPickerDay, updatedSelection )


selectMinute : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Int -> Maybe ( PickerDay, Posix )
selectMinute zone basePickerDay selectionTuple newMinute =
    let
        ( selectedPickerDay, updatedSelection ) =
            case selectionTuple of
                Just ( pickerDay, selection ) ->
                    ( pickerDay, Utilities.setMinuteNotDay zone newMinute selection )

                Nothing ->
                    ( basePickerDay, Utilities.setMinuteNotDay zone newMinute basePickerDay.start )
    in
    Utilities.newOrPreviousSelection zone selectionTuple ( selectedPickerDay, updatedSelection )


filterSelectableTimes : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> { selectableHours : List Int, selectableMinutes : List Int }
filterSelectableTimes zone baseDay selectionTuple =
    case selectionTuple of
        Just ( pickerDay, selection ) ->
            let
                ( earliestSelectableHour, latestSelectableHour ) =
                    Utilities.hourBoundsForSelectedDay zone pickerDay

                ( earliestSelectableMinute, latestSelectableMinute ) =
                    Utilities.minuteBoundsForSelectedHour zone ( pickerDay, selection )
            in
            { selectableHours = List.range earliestSelectableHour latestSelectableHour
            , selectableMinutes = List.range earliestSelectableMinute latestSelectableMinute
            }

        Nothing ->
            let
                ( earliestSelectableHour, latestSelectableHour ) =
                    Utilities.hourBoundsForSelectedDay zone baseDay

                ( earliestSelectableMinute, latestSelectableMinute ) =
                    Utilities.minuteBoundsForSelectedHour zone ( baseDay, baseDay.start )
            in
            { selectableHours = List.range earliestSelectableHour latestSelectableHour
            , selectableMinutes = List.range earliestSelectableMinute latestSelectableMinute
            }
