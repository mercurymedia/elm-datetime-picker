module DatePicker.SingleUtilities exposing
    ( selectDay, selectHour, selectMinute
    , filterSelectableTimes
    )

{-| Utility functions specific to the SingleDatePicker.


# Making a selection

@docs selectDay, selectHour, selectMinute


# Queries

@docs filterSelectableTimes

-}

import DatePicker.DurationUtilities exposing (previewSelection)
import DatePicker.Utilities as Utilities exposing (PickerDay)
import Time exposing (Month(..), Posix, Weekday(..), Zone)


{-| Select a day.

If there is a prior selection and the selection's
time of day falls within the bounds of the newly selected day, the time
is transferred to the new selection. Otherwise, the start bound of the
newly selected day is used as the selection time of day.

Returns a `Maybe` tuple containing the selected `PickerDay` and a `Posix` representing
the full selection (day + time of day).

-}
selectDay : Zone -> Maybe ( PickerDay, Posix ) -> PickerDay -> Maybe ( PickerDay, Posix )
selectDay zone previousSelectionTuple selectedPickerDay =
    if selectedPickerDay.disabled then
        previousSelectionTuple

    else
        case previousSelectionTuple of
            Just ( _, previousSelection ) ->
                if Utilities.posixWithinPickerDayBoundaries zone selectedPickerDay previousSelection then
                    -- keep previously picked time of day
                    Just ( selectedPickerDay, Utilities.setTimeOfDay zone (Time.toHour zone previousSelection) (Time.toMinute zone previousSelection) selectedPickerDay.start )

                else
                    -- use start of picked day
                    Just ( selectedPickerDay, selectedPickerDay.start )

            Nothing ->
                Just ( selectedPickerDay, selectedPickerDay.start )


{-| Select an hour.

With a prior selection:

    set: hour -> provided hour

    if prior selected minute is selectable in new hour: maintain selected minute
    else: select earliest selectable minute for new hour

With no prior selection:

    set: day -> base day, hour -> provided hour, minute -> earliest
    selectable minute for provided hour

If the resulting selected time is not valid, the prior selection is returned instead.

Returns `Just` a tuple containing the selected `PickerDay` and a `Posix` representing
the full selection (day + time of day) or `Nothing`.

-}
selectHour : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Int -> Maybe ( PickerDay, Posix )
selectHour zone basePickerDay selectionTuple newHour =
    Maybe.withDefault ( basePickerDay, basePickerDay.start ) selectionTuple
        |> (\( pickerDay, selection ) -> ( pickerDay, Utilities.setHourNotDay zone newHour selection ))
        |> (\( pickerDay, selection ) ->
                let
                    ( earliestSelectableMinute, _ ) =
                        Utilities.minuteBoundsForSelectedHour zone ( pickerDay, selection )
                in
                -- if no prior selection, always select earliest selectable minute for hour
                if selectionTuple == Nothing then
                    ( pickerDay, Utilities.setMinuteNotDay zone earliestSelectableMinute selection )
                    -- if prior selection, only select earliest selectable minute for hour if prior
                    -- selected minute is less than the earliest selectable minute

                else if Time.toMinute zone selection < earliestSelectableMinute then
                    ( pickerDay, Utilities.setMinuteNotDay zone earliestSelectableMinute selection )
                    -- otherwise, keep prior selected minute

                else
                    ( pickerDay, selection )
           )
        |> Utilities.validSelectionOrDefault zone selectionTuple


{-| Select a minute.

With a prior selection:

    set: minute -> provided minute

With no prior selection:

    set: day -> base day, hour -> base day start hour, minute -> provided minute

If the resulting selected time is not valid, the prior selection is returned instead.

Returns `Just` a tuple containing the selected `PickerDay` and a `Posix` representing
the full selection (day + time of day) or `Nothing`.

-}
selectMinute : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> Int -> Maybe ( PickerDay, Posix )
selectMinute zone basePickerDay selectionTuple newMinute =
    Maybe.withDefault ( basePickerDay, basePickerDay.start ) selectionTuple
        |> (\( pickerDay, selection ) -> ( pickerDay, Utilities.setMinuteNotDay zone newMinute selection ))
        |> Utilities.validSelectionOrDefault zone selectionTuple


{-| Determine the selectable hours and minutes for either
the currently selected time of day or the base day start time.
-}
filterSelectableTimes : Zone -> PickerDay -> Maybe ( PickerDay, Posix ) -> { selectableHours : List Int, selectableMinutes : List Int }
filterSelectableTimes zone baseDay selectionTuple =
    case selectionTuple of
        Just ( pickerDay, selection ) ->
            let
                ( earliestSelectableHour, latestSelectableHour ) =
                    Utilities.hourBoundsForSelectedMinute zone ( pickerDay, selection )

                ( earliestSelectableMinute, latestSelectableMinute ) =
                    Utilities.minuteBoundsForSelectedHour zone ( pickerDay, selection )
            in
            { selectableHours = List.range earliestSelectableHour latestSelectableHour
            , selectableMinutes = List.range earliestSelectableMinute latestSelectableMinute
            }

        Nothing ->
            let
                ( earliestSelectableHour, latestSelectableHour ) =
                    Utilities.hourBoundsForSelectedMinute zone ( baseDay, baseDay.start )

                ( earliestSelectableMinute, latestSelectableMinute ) =
                    Utilities.minuteBoundsForSelectedHour zone ( baseDay, baseDay.start )
            in
            { selectableHours = List.range earliestSelectableHour latestSelectableHour
            , selectableMinutes = List.range earliestSelectableMinute latestSelectableMinute
            }
