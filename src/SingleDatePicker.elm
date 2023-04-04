module SingleDatePicker exposing
    ( DatePicker, init, view, subscriptions
    , Settings, defaultSettings, TimePickerVisibility(..)
    , TimePickerSettings, defaultTimePickerSettings
    , openPicker, closePicker
    , isOpen
    )

{-| A date picker component for a single datetime.


# Architecture

@docs DatePicker, init, view, subscriptions


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
import DatePicker.Icons as Icons
import DatePicker.SingleUtilities as SingleUtilities
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
type DatePicker
    = DatePicker Model


type alias Model =
    { status : Status
    , hovered : Maybe PickerDay
    , viewOffset : Int
    , selectionTuple : Maybe ( PickerDay, Posix )
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
type alias Settings msg =
    { internalMsg : ( DatePicker, Maybe Posix ) -> msg
    , zone : Zone
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


{-| A record of default settings for the date picker. Extend this if
you want to further customize the date picker.

Requires a `Zone` to inform the picker in which time zone it should
display the picked time as well as a `msg` that expects a tuple containing a
datepicker instance and a `Maybe Posix` representing a selected datetime.

    ( DatePicker, Maybe Posix ) -> msg

-}
defaultSettings : Zone -> (( DatePicker, Maybe Posix ) -> msg) -> Settings msg
defaultSettings zone internalMsg =
    { internalMsg = internalMsg
    , zone = zone
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
init : DatePicker
init =
    DatePicker
        { status = Closed
        , hovered = Nothing
        , viewOffset = 0
        , selectionTuple = Nothing
        }


datePickerId : String
datePickerId =
    "date-picker-component"


{-| Events external to the picker to which it is subscribed.
-}
subscriptions : Settings msg -> (( DatePicker, Maybe Posix ) -> msg) -> DatePicker -> Sub msg
subscriptions settings internalMsg (DatePicker model) =
    case model.status of
        Open _ _ ->
            Browser.Events.onMouseDown (clickedOutsidePicker settings datePickerId internalMsg (DatePicker model))

        Closed ->
            Sub.none


clickedOutsidePicker : Settings msg -> String -> (( DatePicker, Maybe Posix ) -> msg) -> DatePicker -> Decode.Decoder msg
clickedOutsidePicker settings componentId internalMsg datePicker =
    Decode.field "target" (Utilities.eventIsOutsideComponent componentId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| internalMsg (update settings Close datePicker)

                else
                    Decode.fail "inside component"
            )


{-| Open the provided date picker and receive the updated picker instance. Also
takes a default time the picker should center on (in the event a time has not yet
been picked) as well as the picked time. A common example of a default time
would be the datetime for the current day.
-}
openPicker : Settings msg -> Posix -> Maybe Posix -> DatePicker -> DatePicker
openPicker settings baseTime pickedTime (DatePicker model) =
    let
        viewOffset =
            Utilities.calculateViewOffset settings.zone baseTime pickedTime

        selectionTuple =
            Maybe.map (\pt -> ( generatePickerDay settings pt, pt )) pickedTime

        timePickerVisible =
            case settings.timePickerVisibility of
                NeverVisible ->
                    False

                Toggleable _ ->
                    False

                AlwaysVisible _ ->
                    True
    in
    DatePicker { model | status = Open timePickerVisible (generatePickerDay settings baseTime), selectionTuple = selectionTuple, viewOffset = viewOffset }


{-| Close the provided date picker and receive the updated picker instance.
-}
closePicker : DatePicker -> DatePicker
closePicker (DatePicker model) =
    DatePicker { model | status = Closed }


{-| Indicates whether the DatePicker is open
-}
isOpen : DatePicker -> Bool
isOpen (DatePicker { status }) =
    case status of
        Open _ _ ->
            True

        Closed ->
            False


type Msg
    = NextMonth
    | PrevMonth
    | NextYear
    | PrevYear
    | SetHoveredDay PickerDay
    | ClearHoveredDay
    | SetDay PickerDay
    | ToggleTimePickerVisibility
    | SetHour Int
    | SetMinute Int
    | Close


generatePickerDay : Settings msg -> Posix -> PickerDay
generatePickerDay settings time =
    Maybe.map
        (\timePickerSettings ->
            Utilities.pickerDayFromPosix settings.zone settings.isDayDisabled (Just timePickerSettings.allowedTimesOfDay) time
        )
        (getTimePickerSettings settings)
        |> Maybe.withDefault (Utilities.pickerDayFromPosix settings.zone settings.isDayDisabled Nothing time)


getTimePickerSettings : Settings msg -> Maybe TimePickerSettings
getTimePickerSettings settings =
    case settings.timePickerVisibility of
        NeverVisible ->
            Nothing

        Toggleable timePickerSettings ->
            Just timePickerSettings

        AlwaysVisible timePickerSettings ->
            Just timePickerSettings


update : Settings msg -> Msg -> DatePicker -> ( DatePicker, Maybe Posix )
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

                SetDay pickerDay ->
                    case SingleUtilities.selectDay settings.zone model.selectionTuple pickerDay of
                        Just ( newPickerDay, newSelection ) ->
                            ( DatePicker { model | selectionTuple = Just ( newPickerDay, newSelection ) }, Just newSelection )

                        Nothing ->
                            ( DatePicker { model | selectionTuple = Nothing }, Nothing )

                ToggleTimePickerVisibility ->
                    case settings.timePickerVisibility of
                        Toggleable _ ->
                            ( DatePicker { model | status = Open (not timePickerVisible) baseDay }, Nothing )

                        _ ->
                            ( DatePicker model, Nothing )

                SetHour hour ->
                    Maybe.map
                        (\( pickerDay, selection ) ->
                            ( DatePicker { model | selectionTuple = Just ( pickerDay, selection ) }, Just selection )
                        )
                        (SingleUtilities.selectHour settings.zone baseDay model.selectionTuple hour)
                        |> Maybe.withDefault ( DatePicker model, Nothing )

                SetMinute minute ->
                    Maybe.map
                        (\( pickerDay, selection ) ->
                            ( DatePicker { model | selectionTuple = Just ( pickerDay, selection ) }, Just selection )
                        )
                        (SingleUtilities.selectMinute settings.zone baseDay model.selectionTuple minute)
                        |> Maybe.withDefault ( DatePicker model, Nothing )

                Close ->
                    ( DatePicker { model | status = Closed }, Nothing )

        Closed ->
            ( DatePicker model, Nothing )


classPrefix : String
classPrefix =
    "elm-datetimepicker--"


determineDateTime : Zone -> Maybe ( PickerDay, Posix ) -> Maybe PickerDay -> Maybe ( PickerDay, Posix )
determineDateTime zone selectionTuple hoveredDay =
    let
        hovered =
            Maybe.andThen showHoveredIfEnabled hoveredDay
    in
    case hovered of
        Just h ->
            SingleUtilities.selectDay zone selectionTuple h

        Nothing ->
            selectionTuple


showHoveredIfEnabled : PickerDay -> Maybe PickerDay
showHoveredIfEnabled hovered =
    if hovered.disabled then
        Nothing

    else
        Just hovered


showSelectedIfHovered : Maybe ( PickerDay, Posix ) -> PickerDay -> Maybe ( PickerDay, Posix )
showSelectedIfHovered selectionTuple hovered =
    case selectionTuple of
        Just ( pickerDay, selection ) ->
            if pickerDay == hovered then
                Just ( pickerDay, selection )

            else
                Just ( hovered, hovered.start )

        Nothing ->
            Just ( hovered, hovered.start )


{-| The date picker view. Simply pass it the configured settings
and the date picker instance you wish to view.
-}
view : Settings msg -> DatePicker -> Html msg
view settings (DatePicker model) =
    case model.status of
        Open timePickerVisible baseDay ->
            let
                offsetTime =
                    Time.add Month model.viewOffset settings.zone baseDay.start
            in
            div
                [ id datePickerId, class (classPrefix ++ "picker-container"), class (classPrefix ++ "single") ]
                [ div [ class (classPrefix ++ "calendar-container") ]
                    [ viewCalendarHeader settings model offsetTime
                    , viewMonth settings model offsetTime
                    ]
                , viewFooter settings timePickerVisible baseDay model
                ]

        Closed ->
            text ""


viewCalendarHeader : Settings msg -> Model -> Posix -> Html msg
viewCalendarHeader settings model time =
    let
        monthName =
            Time.toMonth settings.zone time |> settings.formattedMonth

        year =
            Time.toYear settings.zone time |> String.fromInt
    in
    div
        [ class (classPrefix ++ "calendar-header") ]
        [ div [ class (classPrefix ++ "calendar-header-row") ]
            [ div [ class (classPrefix ++ "calendar-header-navigation") ]
                [ div
                    [ id "previous-year"
                    , class (classPrefix ++ "calendar-header-chevron")
                    , onClick <| settings.internalMsg <| update settings PrevYear (DatePicker model)
                    ]
                    [ Icons.chevronsLeft
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    ]
                , div
                    [ id "previous-month"
                    , class (classPrefix ++ "calendar-header-chevron")
                    , onClick <| settings.internalMsg <| update settings PrevMonth (DatePicker model)
                    ]
                    [ Icons.chevronLeft
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    ]
                ]
            , div
                [ class (classPrefix ++ "calendar-header-text") ]
                [ div [ id "month" ]
                    [ span [ id "month" ] [ text monthName ]
                    , span [] [ text " " ]
                    , span [ id "year" ] [ text year ]
                    ]
                ]
            , div [ class (classPrefix ++ "calendar-header-navigation") ]
                [ div
                    [ id "next-month"
                    , class (classPrefix ++ "calendar-header-chevron")
                    , onClick <| settings.internalMsg <| update settings NextMonth (DatePicker model)
                    ]
                    [ Icons.chevronRight
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    ]
                , div
                    [ id "next-year"
                    , class (classPrefix ++ "calendar-header-chevron")
                    , onClick <| settings.internalMsg <| update settings NextYear (DatePicker model)
                    ]
                    [ Icons.chevronsRight
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    ]
                ]
            ]
        , viewWeekHeader settings
        ]


viewWeekHeader : Settings msg -> Html msg
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


viewMonth : Settings msg -> Model -> Posix -> Html msg
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
        [ class (classPrefix ++ "calendar-month"), onMouseOut <| settings.internalMsg (update settings ClearHoveredDay (DatePicker model)) ]
        [ div [] (List.map (viewWeek settings currentMonth model) weeks)
        ]


viewWeek : Settings msg -> Month -> Model -> List PickerDay -> Html msg
viewWeek settings currentMonth model week =
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


viewDay : Settings msg -> Model -> Month -> PickerDay -> Html msg
viewDay settings model currentMonth day =
    let
        dayParts =
            Time.posixToParts settings.zone day.start

        isFocused =
            Maybe.map (\fday -> generatePickerDay settings fday == day) settings.focusedDate
                |> Maybe.withDefault False

        isPicked =
            Maybe.map (\( pickerDay, _ ) -> pickerDay == day) model.selectionTuple
                |> Maybe.withDefault False

        dayClasses =
            DatePicker.Styles.singleDayClasses classPrefix (dayParts.month /= currentMonth) day.disabled isPicked isFocused
    in
    button
        [ type_ "button"
        , disabled day.disabled
        , class dayClasses
        , onClick <| settings.internalMsg (update settings (SetDay day) (DatePicker model))
        , onMouseOver <| settings.internalMsg (update settings (SetHoveredDay day) (DatePicker model))
        ]
        [ text (String.fromInt dayParts.day) ]


viewFooter : Settings msg -> Bool -> PickerDay -> Model -> Html msg
viewFooter settings timePickerVisible baseDay model =
    let
        displayTime =
            determineDateTime settings.zone model.selectionTuple model.hovered
    in
    div [ class (classPrefix ++ "footer") ]
        [ case displayTime of
            Nothing ->
                viewEmpty

            Just ( _, selection ) ->
                viewDateOrDateTime settings timePickerVisible baseDay model selection
        ]


viewDateOrDateTime : Settings msg -> Bool -> PickerDay -> Model -> Posix -> Html msg
viewDateOrDateTime settings timePickerVisible baseDay model selection =
    Maybe.map
        (\timePickerSettings ->
            if timeIsStartOfDay settings selection then
                viewDate settings selection

            else
                viewDateTime settings timePickerVisible baseDay model selection timePickerSettings
        )
        (getTimePickerSettings settings)
        |> Maybe.withDefault (viewDate settings selection)


viewEmpty : Html msg
viewEmpty =
    div [ class (classPrefix ++ "footer-empty") ]
        [ text "––.––.––––" ]


viewDateTime : Settings msg -> Bool -> PickerDay -> Model -> Posix -> TimePickerSettings -> Html msg
viewDateTime settings timePickerVisible baseDay model selection timePickerSettings =
    div [ class (classPrefix ++ "footer-datetime-container") ]
        [ viewDate settings selection
        , viewTimeOrTimePicker settings timePickerVisible baseDay model selection timePickerSettings
        ]


viewTimeOrTimePicker : Settings msg -> Bool -> PickerDay -> Model -> Posix -> TimePickerSettings -> Html msg
viewTimeOrTimePicker settings timePickerVisible baseDay model selection timePickerSettings =
    let
        displayTime =
            determineDateTime settings.zone model.selectionTuple model.hovered
    in
    case settings.timePickerVisibility of
        NeverVisible ->
            text ""

        Toggleable _ ->
            let
                ( viewToggleView, toggleIcon ) =
                    if timePickerVisible then
                        ( viewTimePicker settings model baseDay displayTime, Icons.check )

                    else
                        ( text (timePickerSettings.timeStringFn settings.zone selection), Icons.edit )
            in
            div [ class (classPrefix ++ "selection-time") ]
                [ Icons.clock
                    |> Icons.withSize 16
                    |> Icons.toHtml []
                , viewToggleView
                , div [ class (classPrefix ++ "time-picker-toggle"), onClick <| settings.internalMsg (update settings ToggleTimePickerVisibility (DatePicker model)) ]
                    [ toggleIcon
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    ]
                ]

        AlwaysVisible _ ->
            div [ class (classPrefix ++ "selection-time") ]
                [ Icons.clock
                    |> Icons.withSize 16
                    |> Icons.toHtml []
                , viewTimePicker settings model baseDay displayTime
                ]


timeIsStartOfDay : Settings msg -> Posix -> Bool
timeIsStartOfDay settings time =
    let
        { hour, minute } =
            Time.posixToParts settings.zone time
    in
    hour == 0 && minute == 0


viewDate : Settings msg -> Posix -> Html msg
viewDate settings dateTime =
    span [ class (classPrefix ++ "selection-date") ]
        [ Icons.calendar
            |> Icons.withSize 16
            |> Icons.toHtml []
        , text (settings.dateStringFn settings.zone dateTime)
        ]


viewTimePicker : Settings msg -> Model -> PickerDay -> Maybe ( PickerDay, Posix ) -> Html msg
viewTimePicker settings model baseDay selectionTuple =
    let
        { selectableHours, selectableMinutes } =
            SingleUtilities.filterSelectableTimes settings.zone baseDay selectionTuple
    in
    div
        [ class (classPrefix ++ "time-picker") ]
        [ div [ class (classPrefix ++ "select-container") ]
            -- Eventually we would like to use onInput instead of a custom on "change".
            --
            -- It will be easier to reason through. However, at the moment, a few browsers are not compatible
            -- with that behaviour. See: https://caniuse.com/#search=oninput
            [ viewSelect [ id "hour-select", on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update settings msg (DatePicker model)) (Decode.map SetHour targetValueIntParse))) ]
                (Utilities.generateHourOptions settings.zone selectionTuple selectableHours)
            , div [ class (classPrefix ++ "select-spacer") ] [ text ":" ]
            , viewSelect
                [ id "minute-select", on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update settings msg (DatePicker model)) (Decode.map SetMinute targetValueIntParse))) ]
                (Utilities.generateMinuteOptions settings.zone selectionTuple selectableMinutes)
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
