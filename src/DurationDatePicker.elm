module DurationDatePicker exposing
    ( DatePicker, Msg, init, view, update, subscriptions
    , Settings, defaultSettings, TimePickerVisibility(..)
    , TimePickerSettings, defaultTimePickerSettings
    , openPicker, closePicker
    , isOpen
    )

{-| A date picker component for picking a datetime range.


# Architecture

@docs DatePicker, Msg, init, view, update, subscriptions


# Settings

@docs Settings, defaultSettings, TimePickerVisibility
@docs TimePickerSettings, defaultTimePickerSettings


# Externally Triggered Actions

@docs openPicker, closePicker


# Query

@docs isOpen

-}

import Browser.Events
import Date
import DatePicker.DurationUtilities as DurationUtilities
import DatePicker.Icons as Icons
import DatePicker.Styles
import DatePicker.Utilities as Utilities exposing (PickerDay)
import Html exposing (Html, button, div, select, span, text)
import Html.Attributes exposing (class, disabled, id, type_)
import Html.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Decode
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra as Time exposing (Interval(..))


{-| The opaque type representing a particular date picker instance.
-}
type DatePicker msg
    = DatePicker (Model msg)


type alias Model msg =
    { status : Status
    , hovered : Maybe PickerDay
    , internalMsg : Msg -> msg
    , viewOffset : Int
    , startSelectionTuple : Maybe ( PickerDay, Posix )
    , endSelectionTuple : Maybe ( PickerDay, Posix )
    }


type Status
    = Closed
    | Open Bool PickerDay


{-| The type facilitating the configuration of the datepicker settings.

`internalMsg` - a function that returns a `msg` for the calling module to handle picker updates
`zone` - the `Zone` in which the date picker is being used (client zone)
`formattedDay` - a function that returns a string representation for the provided day of the week
`formattedMonth` - a function that returns a string representation for the provided month
`isDayDisabled` - a function that determines if the combined `Posix` and `Zone` represent a day that should be disabled in the picker
`focusedDate` - a `Posix` that represents a day that should be highlighted on the picker (i.e. the current day)
`dateStringFn` - a function that returns a string representation of the selected day
`timePickerVisibility` - see below

More information can be found in the [examples](https://github.com/mercurymedia/elm-datetime-picker/tree/master/examples).

-}
type alias Settings =
    { zone : Zone
    , firstWeekDay : Weekday
    , formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , isDayDisabled : Zone -> Posix -> Bool
    , focusedDate : Maybe Posix
    , dateStringFn : Zone -> Posix -> String
    , timePickerVisibility : TimePickerVisibility
    , showCalendarWeekNumbers : Bool
    }


{-| Set the visibility of the timepicker in the `DateTimePicker`

`NeverVisible` - The time picker is never visible. Please note that
while ostensibly picking a day, a selection still returns a Posix
representing the beginning of the day (00:00) when selecting the start
of the date range and the end of the day (23:59) when selecting the end
of the date range.

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


{-| The type facilitating the timepicker's select field configuration
-}
type alias TimePickerSelectConfig =
    { selectableHours : List Int
    , selectableMinutes : List Int
    , selection : Maybe ( PickerDay, Posix )
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
    , firstWeekDay = Mon
    , formattedDay = Utilities.dayToNameString
    , formattedMonth = Utilities.monthToNameString
    , isDayDisabled = \_ _ -> False
    , focusedDate = Nothing
    , dateStringFn = \_ _ -> ""
    , timePickerVisibility = AlwaysVisible defaultTimePickerSettings
    , showCalendarWeekNumbers = False
    }


{-| A record of default settings for the time picker. Extend this if
you want to further customize the time picker.
-}
defaultTimePickerSettings : TimePickerSettings
defaultTimePickerSettings =
    { timeStringFn = \_ _ -> "", allowedTimesOfDay = \_ _ -> { startHour = 0, startMinute = 0, endHour = 23, endMinute = 59 } }


{-| Instantiates and returns a date picker.
-}
init : (Msg -> msg) -> DatePicker msg
init internalMsg =
    DatePicker
        { status = Closed
        , hovered = Nothing
        , internalMsg = internalMsg
        , viewOffset = 0
        , startSelectionTuple = Nothing
        , endSelectionTuple = Nothing
        }


datePickerId : String
datePickerId =
    "date-picker-component"


{-| Events external to the picker to which it is subscribed.
-}
subscriptions : DatePicker msg -> Sub msg
subscriptions (DatePicker model) =
    case model.status of
        Open _ _ ->
            Browser.Events.onMouseDown (clickedOutsidePicker datePickerId model.internalMsg)

        Closed ->
            Sub.none


clickedOutsidePicker : String -> (Msg -> msg) -> Decode.Decoder msg
clickedOutsidePicker componentId internalMsg =
    Decode.field "target" (Utilities.eventIsOutsideComponent componentId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| internalMsg Close

                else
                    Decode.fail "inside component"
            )


{-| Open the provided date picker and receive the updated picker instance. Also
takes a default time the picker should center on (in the event a time has not yet
been picked) as well as the picked start and end times. A common example of a default time
would be the datetime for the current day.
-}
openPicker : Settings -> Posix -> Maybe Posix -> Maybe Posix -> DatePicker msg -> DatePicker msg
openPicker settings baseTime start end (DatePicker model) =
    let
        viewOffset =
            Utilities.calculateViewOffset settings.zone baseTime start

        startSelectionTuple =
            Maybe.map (\s -> ( generatePickerDay settings s, s )) start

        endSelectionTuple =
            Maybe.map (\e -> ( generatePickerDay settings e, e )) end

        timePickerVisible =
            case settings.timePickerVisibility of
                NeverVisible ->
                    False

                Toggleable _ ->
                    False

                AlwaysVisible _ ->
                    True
    in
    DatePicker
        { model
            | status = Open timePickerVisible (generatePickerDay settings baseTime)
            , startSelectionTuple = startSelectionTuple
            , endSelectionTuple = endSelectionTuple
            , viewOffset = viewOffset
        }


{-| Close the provided date picker and receive the updated picker instance.
-}
closePicker : DatePicker msg -> DatePicker msg
closePicker (DatePicker model) =
    DatePicker { model | status = Closed }


{-| Indicates whether the DatePicker is open
-}
isOpen : DatePicker msg -> Bool
isOpen (DatePicker { status }) =
    case status of
        Open _ _ ->
            True

        Closed ->
            False


type StartOrEnd
    = Start
    | End


{-| Internal Msg's to update the picker.
-}
type Msg
    = NextMonth
    | PrevMonth
    | NextYear
    | PrevYear
    | SetHoveredDay PickerDay
    | ClearHoveredDay
    | SetRange PickerDay
    | ToggleTimePickerVisibility
    | SetHour StartOrEnd Int
    | SetMinute StartOrEnd Int
    | Close


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


processSelection : Model msg -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) ) -> ( DatePicker msg, Maybe ( Posix, Posix ) )
processSelection model ( startSelectionTuple, endSelectionTuple ) =
    let
        newDuration =
            Maybe.map2 (\( _, startSelection ) ( _, endSelection ) -> ( startSelection, endSelection )) startSelectionTuple endSelectionTuple
    in
    ( DatePicker { model | startSelectionTuple = startSelectionTuple, endSelectionTuple = endSelectionTuple }, newDuration )


{-| Update the DurationDatePicker according to the given internal msg.

Returns the updated picker and the currently selected start and end datetime, if available.

-}
update : Settings -> Msg -> DatePicker msg -> ( DatePicker msg, Maybe ( Posix, Posix ) )
update settings msg (DatePicker model) =
    case model.status of
        Open timePickerVisible baseDay ->
            case msg of
                NextMonth ->
                    ( DatePicker { model | viewOffset = model.viewOffset + 1 }, Nothing )

                PrevMonth ->
                    ( DatePicker { model | viewOffset = model.viewOffset - 1 }, Nothing )

                NextYear ->
                    ( DatePicker { model | viewOffset = model.viewOffset + 12 }, Nothing )

                PrevYear ->
                    ( DatePicker { model | viewOffset = model.viewOffset - 12 }, Nothing )

                SetHoveredDay pickerDay ->
                    ( DatePicker { model | hovered = Just pickerDay }, Nothing )

                ClearHoveredDay ->
                    ( DatePicker { model | hovered = Nothing }, Nothing )

                SetRange pickerDay ->
                    DurationUtilities.selectDay settings.zone model.startSelectionTuple model.endSelectionTuple pickerDay
                        |> processSelection model

                ToggleTimePickerVisibility ->
                    case settings.timePickerVisibility of
                        Toggleable _ ->
                            ( DatePicker { model | status = Open (not timePickerVisible) baseDay }, Nothing )

                        _ ->
                            ( DatePicker model, Nothing )

                SetHour startOrEnd hour ->
                    case startOrEnd of
                        Start ->
                            DurationUtilities.selectStartHour settings.zone baseDay model.startSelectionTuple model.endSelectionTuple hour
                                |> processSelection model

                        End ->
                            DurationUtilities.selectEndHour settings.zone baseDay model.startSelectionTuple model.endSelectionTuple hour
                                |> processSelection model

                SetMinute startOrEnd minute ->
                    case startOrEnd of
                        Start ->
                            DurationUtilities.selectStartMinute settings.zone baseDay model.startSelectionTuple model.endSelectionTuple minute
                                |> processSelection model

                        End ->
                            DurationUtilities.selectEndMinute settings.zone baseDay model.startSelectionTuple model.endSelectionTuple minute
                                |> processSelection model

                Close ->
                    ( DatePicker { model | status = Closed }, Nothing )

        Closed ->
            ( DatePicker model, Nothing )


classPrefix : String
classPrefix =
    "elm-datetimepicker--"


determineDateTimeRange : Zone -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> Maybe PickerDay -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) )
determineDateTimeRange zone startSelectionTuple endSelectionTuple hoveredDay =
    let
        hovered =
            Maybe.andThen showHoveredIfEnabled hoveredDay
    in
    case hovered of
        Just h ->
            DurationUtilities.previewSelection zone startSelectionTuple endSelectionTuple h

        Nothing ->
            ( startSelectionTuple, endSelectionTuple )


showHoveredIfEnabled : PickerDay -> Maybe PickerDay
showHoveredIfEnabled hovered =
    if hovered.disabled then
        Nothing

    else
        Just hovered


{-| The date picker view. Simply pass it the configured settings
and the date picker instance you wish to view.
-}
view : Settings -> DatePicker msg -> Html msg
view settings (DatePicker model) =
    case model.status of
        Open timePickerVisible baseDay ->
            let
                leftViewTime =
                    Time.add Month model.viewOffset settings.zone baseDay.start

                rightViewTime =
                    Time.add Month (model.viewOffset + 1) settings.zone baseDay.start
            in
            div
                [ id datePickerId, class (classPrefix ++ "picker-container"), class (classPrefix ++ "duration") ]
                [ div
                    [ class (classPrefix ++ "calendars-container") ]
                    [ div
                        [ id "left-container", class (classPrefix ++ "calendar-container") ]
                        [ viewCalendar settings model leftViewTime
                        ]
                    , div
                        [ id "right-container", class (classPrefix ++ "calendar-container") ]
                        [ viewCalendar settings model rightViewTime
                        ]
                    ]
                , viewFooter settings timePickerVisible baseDay model
                ]

        Closed ->
            text ""


viewCalendar : Settings -> Model msg -> Posix -> Html msg
viewCalendar settings model viewTime =
    div
        []
        [ viewCalendarHeader settings model viewTime
        , viewMonth settings model viewTime
        ]


viewCalendarPreviousNavigation : Model msg -> Html msg
viewCalendarPreviousNavigation model =
    div [ class (classPrefix ++ "calendar-header-navigation"), class (classPrefix ++ "calendar-header-navigation--previous") ]
        [ div
            [ id "previous-year"
            , class (classPrefix ++ "calendar-header-chevron")
            , onClick <| model.internalMsg <| PrevYear
            ]
            [ Icons.chevronsLeft
                |> Icons.withSize 15
                |> Icons.toHtml []
            ]
        , div
            [ id "previous-month"
            , class (classPrefix ++ "calendar-header-chevron")
            , onClick <| model.internalMsg <| PrevMonth
            ]
            [ Icons.chevronLeft
                |> Icons.withSize 15
                |> Icons.toHtml []
            ]
        ]


viewCalendarNextNavigation : Model msg -> Html msg
viewCalendarNextNavigation model =
    div [ class (classPrefix ++ "calendar-header-navigation"), class (classPrefix ++ "calendar-header-navigation--next") ]
        [ div
            [ id "next-month"
            , class (classPrefix ++ "calendar-header-chevron")
            , onClick <| model.internalMsg <| NextMonth
            ]
            [ Icons.chevronRight
                |> Icons.withSize 15
                |> Icons.toHtml []
            ]
        , div
            [ id "next-year"
            , class (classPrefix ++ "calendar-header-chevron")
            , onClick <| model.internalMsg <| NextYear
            ]
            [ Icons.chevronsRight
                |> Icons.withSize 15
                |> Icons.toHtml []
            ]
        ]


viewCalendarHeader : Settings -> Model msg -> Posix -> Html msg
viewCalendarHeader settings model viewTime =
    div
        [ class (classPrefix ++ "calendar-header") ]
        [ div [ class (classPrefix ++ "calendar-header-row") ]
            [ viewCalendarPreviousNavigation model
            , viewCalenderHeaderText settings viewTime
            , viewCalendarNextNavigation model
            ]
        , viewWeekHeader settings
        ]


viewCalenderHeaderText : Settings -> Posix -> Html msg
viewCalenderHeaderText { zone, formattedMonth } viewTime =
    let
        monthName =
            Time.toMonth zone viewTime |> formattedMonth

        year =
            Time.toYear zone viewTime |> String.fromInt
    in
    div
        [ class (classPrefix ++ "calendar-header-text")
        ]
        [ div []
            [ span [ id "month" ] [ text monthName ]
            , span [] [ text " " ]
            , span [ id "year" ] [ text year ]
            ]
        ]


viewWeekHeader : Settings -> Html msg
viewWeekHeader settings =
    div
        [ class (classPrefix ++ "calendar-header-week") ]
        ((if settings.showCalendarWeekNumbers then
            div [ class (classPrefix ++ "calendar-header-week-number") ] [ text "W" ]

          else
            text ""
         )
            :: List.map (viewHeaderDay settings.formattedDay) (Utilities.generateListOfWeekDay settings.firstWeekDay)
        )


viewHeaderDay : (Weekday -> String) -> Weekday -> Html msg
viewHeaderDay formatDay day =
    div
        [ class (classPrefix ++ "calendar-header-day") ]
        [ text (formatDay day) ]


viewMonth : Settings -> Model msg -> Posix -> Html msg
viewMonth settings model viewTime =
    let
        allowedTimesOfDayFn =
            Maybe.map .allowedTimesOfDay (getTimePickerSettings settings)

        weeks =
            Utilities.monthData settings.zone settings.isDayDisabled settings.firstWeekDay allowedTimesOfDayFn viewTime

        currentMonth =
            Time.posixToParts settings.zone viewTime |> .month
    in
    div
        [ class (classPrefix ++ "calendar-month"), onMouseOut <| model.internalMsg ClearHoveredDay ]
        [ div [] (List.map (viewWeek settings model currentMonth) weeks)
        ]


viewWeek : Settings -> Model msg -> Month -> List PickerDay -> Html msg
viewWeek settings model currentMonth week =
    let
        firstDateOfWeek =
            Maybe.map
                (\day ->
                    Date.fromPosix settings.zone day.start
                )
                (List.head week)

        dateWeekNumber =
            case firstDateOfWeek of
                Just date ->
                    String.fromInt (Date.weekNumber date)

                Nothing ->
                    ""
    in
    div [ class (classPrefix ++ "calendar-week") ]
        ((if settings.showCalendarWeekNumbers then
            div [ class (classPrefix ++ "calendar-week-number") ] [ text dateWeekNumber ]

          else
            text ""
         )
            :: List.map (viewDay settings model currentMonth) week
        )


viewDay : Settings -> Model msg -> Month -> PickerDay -> Html msg
viewDay settings model currentMonth day =
    let
        dayParts =
            Time.posixToParts settings.zone day.start

        isFocused =
            Maybe.map (\fday -> generatePickerDay settings fday == day) settings.focusedDate
                |> Maybe.withDefault False

        ( isPicked, isBetween ) =
            DurationUtilities.dayPickedOrBetween settings.zone day model.hovered ( model.startSelectionTuple, model.endSelectionTuple )

        dayClasses =
            DatePicker.Styles.durationDayClasses classPrefix (dayParts.month /= currentMonth) day.disabled isPicked isFocused isBetween

        startOrEndClasses =
            DatePicker.Styles.durationStartOrEndClasses classPrefix
                (DurationUtilities.isPickedDaySelectionTuple day model.startSelectionTuple)
                (DurationUtilities.isPickedDaySelectionTuple day model.endSelectionTuple)
    in
    button
        [ type_ "button"
        , disabled day.disabled
        , class dayClasses
        , class startOrEndClasses
        , onClick <| model.internalMsg (SetRange day)
        , onMouseOver <| model.internalMsg (SetHoveredDay day)
        ]
        [ text (String.fromInt dayParts.day) ]


viewFooter : Settings -> Bool -> PickerDay -> Model msg -> Html msg
viewFooter settings timePickerVisible baseDay model =
    let
        ( startSelectionTuple, endSelectionTuple ) =
            determineDateTimeRange settings.zone model.startSelectionTuple model.endSelectionTuple model.hovered
    in
    div [ class (classPrefix ++ "footer") ]
        [ determineDateTimeView settings model timePickerVisible baseDay startSelectionTuple endSelectionTuple
        ]


viewEmpty : Html msg
viewEmpty =
    span [ class (classPrefix ++ "footer-empty") ] [ text "––.––.––––" ]


viewDateTimesSeparator : Html msg
viewDateTimesSeparator =
    div [ class (classPrefix ++ "footer-datetimes-separator") ]
        [ Icons.arrowRight
            |> Icons.withSize 16
            |> Icons.toHtml []
        ]


determineDateTimeView : Settings -> Model msg -> Bool -> PickerDay -> Maybe ( PickerDay, Posix ) -> Maybe ( PickerDay, Posix ) -> Html msg
determineDateTimeView settings model timePickerVisible baseDay startSelectionTuple endSelectionTuple =
    let
        { selectableStartHours, selectableStartMinutes, selectableEndHours, selectableEndMinutes } =
            DurationUtilities.filterSelectableTimes settings.zone baseDay startSelectionTuple endSelectionTuple

        startTimePickerSelectConfig =
            { selectableHours = selectableStartHours
            , selectableMinutes = selectableStartMinutes
            , selection = startSelectionTuple
            }

        endTimePickerSelectConfig =
            { selectableHours = selectableEndHours
            , selectableMinutes = selectableEndMinutes
            , selection = endSelectionTuple
            }
    in
    case ( startSelectionTuple, endSelectionTuple ) of
        ( Nothing, Nothing ) ->
            div [ class (classPrefix ++ "footer-datetimes-container") ]
                [ viewEmpty
                , viewDateTimesSeparator
                , viewEmpty
                ]

        ( Just ( _, startSelection ), Nothing ) ->
            div [ class (classPrefix ++ "footer-datetimes-container") ]
                [ viewDateOrDateTime settings model timePickerVisible Start startSelection startTimePickerSelectConfig
                , viewDateTimesSeparator
                , viewEmpty
                ]

        ( Nothing, Just ( _, endSelection ) ) ->
            div [ class (classPrefix ++ "footer-datetimes-container") ]
                [ viewEmpty
                , viewDateTimesSeparator
                , viewDateOrDateTime settings model timePickerVisible End endSelection endTimePickerSelectConfig
                ]

        ( Just ( startPickerDay, startSelection ), Just ( endPickerDay, endSelection ) ) ->
            if startPickerDay == endPickerDay && timeIsStartOfDay settings startSelection && timeIsEndOfDay settings endSelection then
                viewDate settings startSelection

            else
                div [ class (classPrefix ++ "footer-datetimes-container") ]
                    [ viewDateOrDateTime settings model timePickerVisible Start startSelection startTimePickerSelectConfig
                    , viewDateTimesSeparator
                    , viewDateOrDateTime settings model timePickerVisible End endSelection endTimePickerSelectConfig
                    ]


viewDateOrDateTime : Settings -> Model msg -> Bool -> StartOrEnd -> Posix -> TimePickerSelectConfig -> Html msg
viewDateOrDateTime settings model timePickerVisible startOrEnd dateTime timePickerConfig =
    Maybe.map
        (\timePickerSettings ->
            case startOrEnd of
                Start ->
                    if timeIsStartOfDay settings dateTime then
                        viewDate settings dateTime

                    else
                        viewDateTime settings model timePickerVisible Start timePickerSettings.timeStringFn dateTime timePickerConfig

                End ->
                    if timeIsEndOfDay settings dateTime then
                        viewDate settings dateTime

                    else
                        viewDateTime settings model timePickerVisible End timePickerSettings.timeStringFn dateTime timePickerConfig
        )
        (getTimePickerSettings settings)
        |> Maybe.withDefault (viewDate settings dateTime)


timeIsStartOfDay : Settings -> Posix -> Bool
timeIsStartOfDay settings time =
    let
        { hour, minute } =
            Time.posixToParts settings.zone time
    in
    hour == 0 && minute == 0


timeIsEndOfDay : Settings -> Posix -> Bool
timeIsEndOfDay settings time =
    let
        { hour, minute } =
            Time.posixToParts settings.zone time
    in
    hour == 23 && minute == 59


viewDate : Settings -> Posix -> Html msg
viewDate settings dateTime =
    div [ class (classPrefix ++ "footer-datetime-container") ]
        [ span [ class (classPrefix ++ "selection-date") ]
            [ Icons.calendar
                |> Icons.withSize 16
                |> Icons.toHtml []
            , text (settings.dateStringFn settings.zone dateTime)
            ]
        ]


viewDateTime : Settings -> Model msg -> Bool -> StartOrEnd -> (Zone -> Posix -> String) -> Posix -> TimePickerSelectConfig -> Html msg
viewDateTime settings model timePickerVisible startOrEnd timeStringFn dateTime timePickerSelectConfig =
    div [ class (classPrefix ++ "footer-datetime-container") ]
        [ span [ class (classPrefix ++ "selection-date") ]
            [ Icons.calendar
                |> Icons.withSize 16
                |> Icons.toHtml []
            , text (settings.dateStringFn settings.zone dateTime)
            ]
        , viewTimeOrTimePicker settings model timePickerVisible startOrEnd timeStringFn dateTime timePickerSelectConfig
        ]


viewTimeOrTimePicker : Settings -> Model msg -> Bool -> StartOrEnd -> (Zone -> Posix -> String) -> Posix -> TimePickerSelectConfig -> Html msg
viewTimeOrTimePicker settings model timePickerVisible startOrEnd timeStringFn dateTime timePickerSelectConfig =
    case settings.timePickerVisibility of
        NeverVisible ->
            text ""

        Toggleable _ ->
            let
                ( viewToggleView, toggleIcon ) =
                    if timePickerVisible then
                        ( viewTimePicker settings model startOrEnd timePickerSelectConfig, Icons.check )

                    else
                        ( text (timeStringFn settings.zone dateTime), Icons.edit )
            in
            span [ class (classPrefix ++ "selection-time") ]
                [ Icons.clock
                    |> Icons.withSize 16
                    |> Icons.toHtml []
                , viewToggleView
                , div [ class (classPrefix ++ "time-picker-toggle"), onClick <| model.internalMsg ToggleTimePickerVisibility ]
                    [ toggleIcon
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    ]
                ]

        AlwaysVisible _ ->
            span [ class (classPrefix ++ "selection-time") ]
                [ Icons.clock
                    |> Icons.withSize 16
                    |> Icons.toHtml []
                , viewTimePicker settings model startOrEnd timePickerSelectConfig
                ]


viewTimePicker : Settings -> Model msg -> StartOrEnd -> TimePickerSelectConfig -> Html msg
viewTimePicker settings model startOrEnd { selectableHours, selectableMinutes, selection } =
    div
        [ class (classPrefix ++ "time-picker") ]
        [ div [ class (classPrefix ++ "select-container") ]
            -- Eventually we would like to use onInput instead of a custom on "change".
            --
            -- It will be easier to reason through. However, at the moment, a few browsers are not compatible
            -- with that behaviour. See: https://caniuse.com/#search=oninput
            [ viewSelect
                [ class "hour-select", on "change" (Decode.map model.internalMsg (Decode.map (SetHour startOrEnd) targetValueIntParse)) ]
                (Utilities.generateHourOptions settings.zone selection selectableHours)
            , div [ class (classPrefix ++ "select-spacer") ] [ text ":" ]
            , viewSelect
                [ class "minute-select", on "change" (Decode.map model.internalMsg (Decode.map (SetMinute startOrEnd) targetValueIntParse)) ]
                (Utilities.generateMinuteOptions settings.zone selection selectableMinutes)
            ]
        ]


viewSelect : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewSelect attributes content =
    div [ class (classPrefix ++ "select") ]
        [ select attributes content
        , Icons.chevronDown
            |> Icons.withSize 16
            |> Icons.toHtml []
        ]
