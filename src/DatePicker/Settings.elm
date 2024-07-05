module DatePicker.Settings exposing (..)

import DatePicker.Utilities as Utilities exposing (DomLocation(..), PickerDay)
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra as Time exposing (Interval(..))


{-| The type facilitating the configuration of the datepicker settings.

`id` - provides a custom id to the picker element
`zone` - the `Zone` in which the date picker is being used (client zone)
`formattedDay` - a function that returns a string representation for the provided day of the week
`formattedMonth` - a function that returns a string representation for the provided month
`isDayDisabled` - a function that determines if the combined `Posix` and `Zone` represent a day that should be disabled in the picker
`focusedDate` - a `Posix` that represents a day that should be highlighted on the picker (i.e. the current day)
`dateStringFn` - a function that returns a string representation of the selected day
`timePickerVisibility` - see below
`showCalendarWeekNumbers` - wheather to display or not display caldendar week numbers
`presetRanges` - a list of `PresetDate`, for selectable, preconfigured dates

More information can be found in the [examples](https://github.com/mercurymedia/elm-datetime-picker/tree/master/examples).

-}
type alias Settings =
    { zone : Zone
    , id : String
    , firstWeekDay : Weekday
    , formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , isDayDisabled : Zone -> Posix -> Bool
    , focusedDate : Maybe Posix
    , dateStringFn : Zone -> Posix -> String
    , timePickerVisibility : TimePickerVisibility
    , showCalendarWeekNumbers : Bool
    , presets : List Preset
    }


{-| Set the visibility of the timepicker in the `DateTimePicker`

`NeverVisible` - The time picker is never visible. Please note that
while ostensibly picking a day, a selection still returns a Posix
representing the beginning of that day (00:00). It is up to you to
process the selection accordingly if you wish to treat it as a whole day.

`Toggleable` - The time picker visibility can be toggled but
is by default closed when the datetime picker is opened. Additional
configuration can be achieved via `TimePickerSettings`.

`AlwaysVisible` - The time picker is always visible. This is the default setting
as it most explicitly shows that the datetime picker is indeed picking both
a date and time, not simply a date. Additional configuration can be achieved
via `TimePickerSettings`.

-}
type TimePickerVisibility
    = NeverVisible
    | Toggleable TimePickerSettings
    | AlwaysVisible TimePickerSettings


{-| The type facilitating the configuration of the timepicker settings.

`timeStringFn` - a function that returns a string representation of the selected time of day

Because it could be the case that a picker is being used in a different
timezone than the home timezone of the implementor, the `allowedTimesofDay`
function ingests a `Zone` in addition to a `Posix`. The
`Zone` represents the time zone in which the picker is being used. An
implementor can leverage this to compare against a base time zone when
enforcing allowable times of day, etc. You SHOULD assume that the `Posix`
passed into these functions is floored to the start of its respective `Day`.

More information can be found in the [examples](https://github.com/mercurymedia/elm-datetime-picker/tree/master/examples).

-}
type alias TimePickerSettings =
    { timeStringFn : Zone -> Posix -> String
    , allowedTimesOfDay :
        Zone
        -> Posix
        ->
            { startHour : Int
            , startMinute : Int
            , endHour : Int
            , endMinute : Int
            }
    }


type Preset
    = PresetDate PresetDateConfig
    | PresetRange PresetRangeConfig


{-| Set type facilitating the preset dates
`title` - the displayed name of the preset
`date` - the `Posix` the preset will select in the date picker.
-}
type alias PresetDateConfig =
    { title : String
    , date : Posix
    }


{-| Set type facilitating the preset date ranges
`title` - the displayed name of the preset
`range` - the time range the date range picker will select, consisting of
a start-`Posix` and an end-`Posix`.
-}
type alias PresetRangeConfig =
    { title : String
    , range : { start : Posix, end : Posix }
    }


{-| A record of default settings for the date picker. Extend this if
you want to further customize the date picker.

Requires a `Zone` to inform the picker in which time zone it should
display the selected duration as well as a `msg` that expects a tuple containing
a datepicker instance and a `Maybe` tuple representing a selected duration.

    ( DatePicker, Maybe ( Posix, Posix ) ) -> msg

-}
defaultSettings : Zone -> Settings
defaultSettings zone =
    { zone = zone
    , id = "date-picker-component"
    , firstWeekDay = Mon
    , formattedDay = Utilities.dayToNameString
    , formattedMonth = Utilities.monthToNameString
    , isDayDisabled = \_ _ -> False
    , focusedDate = Nothing
    , dateStringFn = \_ _ -> ""
    , timePickerVisibility = AlwaysVisible defaultTimePickerSettings
    , showCalendarWeekNumbers = False
    , presets = []
    }


{-| A record of default settings for the time picker. Extend this if
you want to further customize the time picker.
-}
defaultTimePickerSettings : TimePickerSettings
defaultTimePickerSettings =
    { timeStringFn = \_ _ -> "", allowedTimesOfDay = \_ _ -> { startHour = 0, startMinute = 0, endHour = 23, endMinute = 59 } }


generatePickerDay : Settings -> Posix -> PickerDay
generatePickerDay settings time =
    Maybe.map
        (\timePickerSettings ->
            Utilities.pickerDayFromPosix settings.zone settings.isDayDisabled (Just timePickerSettings.allowedTimesOfDay) time
        )
        (getTimePickerSettings settings)
        |> Maybe.withDefault (Utilities.pickerDayFromPosix settings.zone settings.isDayDisabled Nothing time)


getTimePickerSettings : Settings -> Maybe TimePickerSettings
getTimePickerSettings settings =
    case settings.timePickerVisibility of
        NeverVisible ->
            Nothing

        Toggleable timePickerSettings ->
            Just timePickerSettings

        AlwaysVisible timePickerSettings ->
            Just timePickerSettings


isPresetDateActive : Settings -> Maybe ( PickerDay, Posix ) -> PresetDateConfig -> Bool
isPresetDateActive settings selectionTuple { date } =
    case selectionTuple of
        Just ( pickerDay, _ ) ->
            let
                presetPickerDay =
                    generatePickerDay settings date
            in
            if presetPickerDay == pickerDay then
                True

            else
                False

        _ ->
            False


isPresetRangeActive : Settings -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> PresetRangeConfig -> Bool
isPresetRangeActive settings startSelectionTuple endSelectionTuple { range } =
    case ( startSelectionTuple, endSelectionTuple ) of
        ( Just ( startPickerDay, _ ), Just ( endPickerDay, _ ) ) ->
            let
                presetStartPickerDay =
                    generatePickerDay settings range.start

                presetEndPickerDay =
                    generatePickerDay settings range.end
            in
            if presetStartPickerDay == startPickerDay && presetEndPickerDay == endPickerDay then
                True

            else
                False

        _ ->
            False
