module DurationDatePicker exposing
    ( DatePicker, Msg, init, view, viewDurationInput, update, subscriptions
    , openPicker, closePicker, updatePickerPosition
    , isOpen, hasError
    )

{-| A date picker component for picking a datetime range.


# Architecture

@docs DatePicker, Msg, init, view, viewDurationInput, update, subscriptions


# Externally Triggered Actions

@docs openPicker, closePicker, updatePickerPosition


# Query

@docs isOpen, hasError

-}

import Browser.Dom as Dom
import Browser.Events
import Css
import DatePicker.Alignment as Alignment exposing (Alignment)
import DatePicker.DateInput as DateInput
import DatePicker.DurationUtilities as DurationUtilities
import DatePicker.Icons as Icons
import DatePicker.Settings exposing (..)
import DatePicker.Utilities as Utilities exposing (DomLocation(..), PickerDay, classPrefix)
import DatePicker.ViewComponents exposing (..)
import Html exposing (Html)
import Html.Events.Extra exposing (targetValueIntParse)
import Html.Styled exposing (div, fromUnstyled, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, id, start)
import Html.Styled.Events exposing (onClick)
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
    , domLocation : DomLocation
    , startDateInput : DateInput.DateInput msg
    , endDateInput : DateInput.DateInput msg
    , alignment : Maybe Alignment
    }


type Status
    = Closed
    | Open Bool PickerDay


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
        , domLocation = InsideHierarchy
        , startDateInput =
            DateInput.init (internalMsg << HandleStartDateInputUpdate)
        , endDateInput =
            DateInput.init (internalMsg << HandleEndDateInputUpdate)
        , alignment = Nothing
        }


{-| Events external to the picker to which it is subscribed.
-}
subscriptions : Settings -> DatePicker msg -> Sub msg
subscriptions settings (DatePicker model) =
    case model.status of
        Open _ _ ->
            Browser.Events.onMouseDown (Utilities.clickedOutsidePicker [ settings.id, DateInput.containerId (dateInputConfig settings) ] (model.internalMsg Close))

        Closed ->
            Sub.none


{-| Open the provided date picker and receive the updated picker instance. Also
takes a default time the picker should center on (in the event a time has not yet
been picked) as well as the picked start and end times. A common example of a default time
would be the datetime for the current day.
-}
openPicker : String -> Settings -> Posix -> Maybe Posix -> Maybe Posix -> DatePicker msg -> ( DatePicker msg, Cmd msg )
openPicker triggerElementId settings baseTime start end (DatePicker model) =
    let
        ( ( updatedPicker, _ ), cmd ) =
            update settings (OpenPicker baseTime start end triggerElementId) (DatePicker model)
    in
    ( updatedPicker, cmd )


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


{-| Indicates whether the DatePicker's date inputs have a validation error
-}
hasError : Settings -> DatePicker msg -> Bool
hasError { zone } (DatePicker { startDateInput, endDateInput }) =
    let
        ( hasDurationError, _ ) =
            DateInput.hasDurationError zone ( startDateInput, endDateInput )
    in
    hasDurationError


{-| Returns the command to update the trigger & picker DOM elements' instances.
Is used internally but can also be used externally in case of a changing viewport
(e.g. onScroll or onResize).
-}
updatePickerPosition : DatePicker msg -> ( DatePicker msg, Cmd msg )
updatePickerPosition (DatePicker model) =
    let
        cmd =
            case ( model.status, model.alignment ) of
                ( Open _ _, Just alignment ) ->
                    Alignment.update (model.internalMsg << GotAlignment) alignment

                ( _, _ ) ->
                    Cmd.none
    in
    ( DatePicker model, cmd )


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
    | GotAlignment (Result Dom.Error Alignment)
    | SetRange PickerDay
    | ToggleTimePickerVisibility
    | SetHour StartOrEnd Int
    | SetMinute StartOrEnd Int
    | Close
    | SetPresetRange PresetRangeConfig
    | HandleStartDateInputUpdate DateInput.Msg
    | HandleEndDateInputUpdate DateInput.Msg
    | OpenPicker Posix (Maybe Posix) (Maybe Posix) String


{-| Update the DurationDatePicker according to the given internal msg.

Returns the updated picker and the currently selected start and end datetime, if available.

-}
update : Settings -> Msg -> DatePicker msg -> ( ( DatePicker msg, Maybe ( Posix, Posix ) ), Cmd msg )
update settings msg (DatePicker model) =
    let
        pickedDuration =
            Maybe.map2 (\( _, startSelection ) ( _, endSelection ) -> ( startSelection, endSelection )) model.startSelectionTuple model.endSelectionTuple
    in
    case model.status of
        Open timePickerVisible baseDay ->
            case msg of
                NextMonth ->
                    ( ( DatePicker { model | viewOffset = model.viewOffset + 1 }, pickedDuration ), Cmd.none )

                PrevMonth ->
                    ( ( DatePicker { model | viewOffset = model.viewOffset - 1 }, pickedDuration ), Cmd.none )

                NextYear ->
                    ( ( DatePicker { model | viewOffset = model.viewOffset + 12 }, pickedDuration ), Cmd.none )

                PrevYear ->
                    ( ( DatePicker { model | viewOffset = model.viewOffset - 12 }, pickedDuration ), Cmd.none )

                SetHoveredDay pickerDay ->
                    ( ( DatePicker { model | hovered = Just pickerDay }, pickedDuration ), Cmd.none )

                ClearHoveredDay ->
                    ( ( DatePicker { model | hovered = Nothing }, pickedDuration ), Cmd.none )

                SetRange pickerDay ->
                    let
                        newSelection =
                            DurationUtilities.selectDay settings.zone model.startSelectionTuple model.endSelectionTuple pickerDay
                    in
                    ( updateSelection settings newSelection ( DatePicker model, pickedDuration )
                    , Cmd.none
                    )

                ToggleTimePickerVisibility ->
                    case settings.timePickerVisibility of
                        Toggleable _ ->
                            ( ( DatePicker { model | status = Open (not timePickerVisible) baseDay }, Nothing ), Cmd.none )

                        _ ->
                            ( ( DatePicker model, Nothing ), Cmd.none )

                SetHour startOrEnd hour ->
                    case startOrEnd of
                        Start ->
                            let
                                newSelection =
                                    DurationUtilities.selectStartHour settings.zone baseDay model.startSelectionTuple model.endSelectionTuple hour
                            in
                            ( updateSelection settings newSelection ( DatePicker model, pickedDuration )
                            , Cmd.none
                            )

                        End ->
                            let
                                newSelection =
                                    DurationUtilities.selectEndHour settings.zone baseDay model.startSelectionTuple model.endSelectionTuple hour
                            in
                            ( updateSelection settings newSelection ( DatePicker model, pickedDuration )
                            , Cmd.none
                            )

                SetMinute startOrEnd minute ->
                    case startOrEnd of
                        Start ->
                            let
                                newSelection =
                                    DurationUtilities.selectStartMinute settings.zone baseDay model.startSelectionTuple model.endSelectionTuple minute
                            in
                            ( updateSelection settings newSelection ( DatePicker model, pickedDuration )
                            , Cmd.none
                            )

                        End ->
                            let
                                newSelection =
                                    DurationUtilities.selectEndMinute settings.zone baseDay model.startSelectionTuple model.endSelectionTuple minute
                            in
                            ( updateSelection settings newSelection ( DatePicker model, pickedDuration )
                            , Cmd.none
                            )

                Close ->
                    ( ( DatePicker { model | status = Closed }, pickedDuration ), Cmd.none )

                GotAlignment result ->
                    case result of
                        Ok alignment ->
                            ( ( DatePicker { model | alignment = Just alignment }, pickedDuration ), Cmd.none )

                        Err _ ->
                            ( ( DatePicker model, pickedDuration ), Cmd.none )

                SetPresetRange { range } ->
                    let
                        startPickerDay =
                            generatePickerDay settings range.start

                        endPickerDay =
                            generatePickerDay settings range.end

                        newSelection =
                            ( Just ( startPickerDay, startPickerDay.start ), Just ( endPickerDay, endPickerDay.end ) )

                        updatedPickerAndDuration =
                            ( DatePicker model, pickedDuration )
                                |> updateSelection settings newSelection
                                |> updateViewOffset settings baseDay
                    in
                    ( updatedPickerAndDuration
                    , Cmd.none
                    )

                HandleStartDateInputUpdate subMsg ->
                    let
                        ( updatedDateInput, dateInputCmd ) =
                            DateInput.update (dateInputConfig settings) subMsg model.startDateInput
                    in
                    ( updateSelectionFromDateInput settings baseDay Start updatedDateInput ( DatePicker model, pickedDuration ), dateInputCmd )

                HandleEndDateInputUpdate subMsg ->
                    let
                        ( updatedDateInput, dateInputCmd ) =
                            DateInput.update (dateInputConfig settings) subMsg model.endDateInput
                    in
                    ( updateSelectionFromDateInput settings baseDay End updatedDateInput ( DatePicker model, pickedDuration ), dateInputCmd )

                _ ->
                    ( ( DatePicker model, pickedDuration ), Cmd.none )

        Closed ->
            case msg of
                HandleStartDateInputUpdate subMsg ->
                    let
                        ( updatedDateInput, dateInputCmd ) =
                            DateInput.update (dateInputConfig settings) subMsg model.startDateInput
                    in
                    ( ( DatePicker { model | startDateInput = updatedDateInput }, pickedDuration ), dateInputCmd )

                HandleEndDateInputUpdate subMsg ->
                    let
                        ( updatedDateInput, dateInputCmd ) =
                            DateInput.update (dateInputConfig settings) subMsg model.endDateInput
                    in
                    ( ( DatePicker { model | endDateInput = updatedDateInput }, pickedDuration ), dateInputCmd )

                OpenPicker baseTime pickedStartTime pickedEndTime triggerElementId ->
                    let
                        baseDay =
                            generatePickerDay settings baseTime

                        newStartSelectionTuple =
                            Maybe.map (\s -> ( generatePickerDay settings s, s )) pickedStartTime

                        newEndSelectionTuple =
                            Maybe.map (\e -> ( generatePickerDay settings e, e )) pickedEndTime

                        timePickerVisible =
                            isTimePickerVisible settings.timePickerVisibility

                        status =
                            Open timePickerVisible baseDay

                        ( DatePicker updatedModel, updatedPickedDuration ) =
                            ( DatePicker model, pickedDuration )
                                |> updateSelection settings ( newStartSelectionTuple, newEndSelectionTuple )
                                -- in case there was no pickedStartTime but the start input still has a valid value
                                |> updateSelectionFromDateInput settings baseDay Start model.startDateInput
                                -- in case there was no pickedEndTime but the end input still has a valid value
                                |> updateSelectionFromDateInput settings baseDay End model.endDateInput
                                |> updateViewOffset settings baseDay
                    in
                    ( ( DatePicker { updatedModel | status = status }
                      , updatedPickedDuration
                      )
                    , Alignment.init
                        { triggerId = triggerElementId, pickerId = settings.id }
                        (model.internalMsg << GotAlignment)
                    )

                _ ->
                    ( ( DatePicker model, pickedDuration ), Cmd.none )


updateSelection : Settings -> ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) ) -> ( DatePicker msg, Maybe ( Posix, Posix ) ) -> ( DatePicker msg, Maybe ( Posix, Posix ) )
updateSelection settings ( newStartSelectionTuple, newEndSelectionTuple ) ( DatePicker model, pickedDuration ) =
    let
        ( updatedDuration, updatedStartDateInput, updatedEndDateInput ) =
            case ( newStartSelectionTuple, newEndSelectionTuple ) of
                ( Just ( _, startSelection ), Just ( _, endSelection ) ) ->
                    ( Just ( startSelection, endSelection )
                    , DateInput.updateFromPosix (dateInputConfig settings) settings.zone startSelection model.startDateInput
                    , DateInput.updateFromPosix (dateInputConfig settings) settings.zone endSelection model.endDateInput
                    )

                ( Just ( _, startSelection ), Nothing ) ->
                    ( Nothing
                    , DateInput.updateFromPosix (dateInputConfig settings) settings.zone startSelection model.startDateInput
                    , DateInput.clear model.endDateInput
                    )

                ( Nothing, Just ( _, endSelection ) ) ->
                    ( Nothing
                    , DateInput.clear model.startDateInput
                    , DateInput.updateFromPosix (dateInputConfig settings) settings.zone endSelection model.endDateInput
                    )

                ( Nothing, Nothing ) ->
                    ( Nothing
                    , model.startDateInput
                    , model.endDateInput
                    )
    in
    ( DatePicker
        { model
            | startSelectionTuple = newStartSelectionTuple
            , endSelectionTuple = newEndSelectionTuple
            , startDateInput = updatedStartDateInput
            , endDateInput = updatedEndDateInput
        }
    , updatedDuration
    )


updateSelectionFromDateInput : Settings -> PickerDay -> StartOrEnd -> DateInput.DateInput msg -> ( DatePicker msg, Maybe ( Posix, Posix ) ) -> ( DatePicker msg, Maybe ( Posix, Posix ) )
updateSelectionFromDateInput settings baseDay startOrEnd updatedDateInput ( DatePicker model, pickedDuration ) =
    case DateInput.toPosix settings.zone updatedDateInput of
        Just posix ->
            let
                pickerDay =
                    generatePickerDay settings posix

                newSelectionTuple =
                    ( pickerDay, posix )

                ( newSelection, updatedStartDateInput, updatedEndDateInput ) =
                    case startOrEnd of
                        Start ->
                            ( ( Just newSelectionTuple, model.endSelectionTuple ), updatedDateInput, model.endDateInput )

                        End ->
                            ( ( model.startSelectionTuple, Just newSelectionTuple ), model.startDateInput, updatedDateInput )

                ( DatePicker updatedModel, updatedSelection ) =
                    ( DatePicker model, pickedDuration )
                        |> updateSelection settings newSelection
                        |> updateViewOffset settings baseDay
            in
            ( DatePicker { updatedModel | startDateInput = updatedStartDateInput, endDateInput = updatedEndDateInput }, updatedSelection )

        Nothing ->
            case startOrEnd of
                Start ->
                    ( DatePicker { model | startDateInput = updatedDateInput, startSelectionTuple = Nothing }, Nothing )

                End ->
                    ( DatePicker { model | endDateInput = updatedDateInput, endSelectionTuple = Nothing }, Nothing )


updateViewOffset : Settings -> PickerDay -> ( DatePicker msg, Maybe ( Posix, Posix ) ) -> ( DatePicker msg, Maybe ( Posix, Posix ) )
updateViewOffset settings baseDay ( DatePicker model, pickedDuration ) =
    let
        viewOffset =
            Maybe.map (\( _, startSelection ) -> Utilities.calculateViewOffset settings.zone baseDay.start (Just startSelection)) model.startSelectionTuple
                |> Maybe.withDefault model.viewOffset
    in
    ( DatePicker { model | viewOffset = viewOffset }
    , pickedDuration
    )


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
    viewStyled settings (DatePicker model)
        |> toUnstyled


viewStyled : Settings -> DatePicker msg -> Html.Styled.Html msg
viewStyled settings (DatePicker model) =
    case model.status of
        Open timePickerVisible baseDay ->
            let
                styles =
                    Alignment.pickerStylesFromAlignment settings.theme model.alignment
            in
            viewContainer settings.theme
                [ id settings.id
                , class (classPrefix settings.theme.classNamePrefix "duration")
                , css styles
                ]
                [ viewPresets [] settings model
                , viewPicker []
                    settings
                    timePickerVisible
                    baseDay
                    model
                ]

        _ ->
            text ""


dateInputConfig : Settings -> DateInput.Config
dateInputConfig settings =
    let
        defaultConfig =
            DateInput.defaultConfig settings.zone
    in
    { defaultConfig
        | dateInputSettings = settings.dateInputSettings
        , isDayDisabled = settings.isDayDisabled
        , theme = settings.theme
        , id = settings.id ++ "--date-input"
    }


{-| The duration date inputs view with the date picker opening on click.
Pass it the configured settings, the base time, the picked start and end times
and the date picker instance you wish to view.
-}
viewDurationInput : List (Html.Attribute msg) -> Settings -> Posix -> Maybe Posix -> Maybe Posix -> DatePicker msg -> Html msg
viewDurationInput attrs settings baseTime maybePickedStart maybePickedEnd (DatePicker model) =
    viewDurationInputStyled (Utilities.toStyledAttrs attrs) settings baseTime maybePickedStart maybePickedEnd (DatePicker model)
        |> toUnstyled


viewDurationInputStyled : List (Html.Styled.Attribute msg) -> Settings -> Posix -> Maybe Posix -> Maybe Posix -> DatePicker msg -> Html.Styled.Html msg
viewDurationInputStyled attrs settings baseTime maybePickedStart maybePickedEnd (DatePicker model) =
    let
        onClickMsg =
            model.internalMsg <|
                OpenPicker baseTime maybePickedStart maybePickedEnd (DateInput.containerId <| dateInputConfig settings)

        isPickerOpen =
            isOpen (DatePicker model)
    in
    DateInput.viewContainer settings.theme
        (id (DateInput.containerId <| dateInputConfig settings) :: attrs)
        [ DateInput.viewDurationInputs
            [ onClick onClickMsg
            , css
                (Alignment.dateInputStylesFromAlignment
                    settings.theme
                    isPickerOpen
                    (Alignment.calcDurationDateInputWidth settings.theme settings.showCalendarWeekNumbers)
                    model.alignment
                )
            ]
            (dateInputConfig settings)
            ( model.startDateInput, model.endDateInput )
        , case model.status of
            Open timePickerVisible baseDay ->
                viewContainer settings.theme
                    [ id settings.id
                    , class (classPrefix settings.theme.classNamePrefix "duration")
                    , css
                        (Alignment.applyPickerStyles
                            (\alignment ->
                                [ Alignment.pickerGridLayoutFromAlignment alignment
                                , Alignment.pickerPositionFromAlignment settings.theme alignment
                                , Alignment.pickerTranslationFromAlignment settings.theme alignment
                                ]
                            )
                            model.alignment
                        )
                    ]
                    [ viewPresets
                        [ css [ Css.property "grid-area" Alignment.gridAreaPresets ] ]
                        settings
                        model
                    , div
                        [ css [ Css.property "grid-area" Alignment.gridAreaDateInput, Css.padding (Css.px settings.theme.spacing.base) ] ]
                        [ DateInput.viewPlaceholder (dateInputConfig settings) ]
                    , viewPicker
                        [ css [ Css.property "grid-area" Alignment.gridAreaCalendar ] ]
                        settings
                        timePickerVisible
                        baseDay
                        model
                    ]

            Closed ->
                text ""
        ]


viewPicker : List (Html.Styled.Attribute msg) -> Settings -> Bool -> PickerDay -> Model msg -> Html.Styled.Html msg
viewPicker attributes settings timePickerVisible baseDay model =
    let
        leftViewTime =
            Time.add Month model.viewOffset settings.zone baseDay.start

        rightViewTime =
            Time.add Month (model.viewOffset + 1) settings.zone baseDay.start
    in
    viewPickerContainer settings.theme
        attributes
        [ div [ css [ Css.displayFlex ] ]
            [ viewCalendar [ class (classPrefix settings.theme.classNamePrefix "left-calendar") ] settings model leftViewTime Left
            , viewCalendar [ class (classPrefix settings.theme.classNamePrefix "right-calendar") ] settings model rightViewTime Right
            ]
        , viewFooter settings timePickerVisible baseDay model
        ]


viewPresets : List (Html.Styled.Attribute msg) -> Settings -> Model msg -> Html.Styled.Html msg
viewPresets attrs settings model =
    if List.length settings.presets > 0 then
        viewPresetsContainer settings.theme
            attrs
            (List.map
                (\preset ->
                    case preset of
                        PresetRange config ->
                            viewPresetTab settings.theme
                                []
                                { title = config.title
                                , active = isPresetRangeActive settings model.startSelectionTuple model.endSelectionTuple config
                                , onClickMsg = model.internalMsg (SetPresetRange config)
                                }

                        _ ->
                            text ""
                )
                settings.presets
            )

    else
        text ""


type CalendarSide
    = Left
    | Right


viewCalendar : List (Html.Styled.Attribute msg) -> Settings -> Model msg -> Posix -> CalendarSide -> Html.Styled.Html msg
viewCalendar attrs settings model viewTime side =
    let
        monthName =
            Time.toMonth settings.zone viewTime |> settings.formattedMonth

        year =
            Time.toYear settings.zone viewTime |> String.fromInt

        allowedTimesOfDayFn =
            Maybe.map .allowedTimesOfDay (getTimePickerSettings settings)

        weeks =
            Utilities.monthData settings.zone settings.isDayDisabled settings.firstWeekDay allowedTimesOfDayFn viewTime

        currentMonth =
            Time.posixToParts settings.zone viewTime |> .month

        dayStylesFn =
            \day ->
                let
                    dayParts =
                        Time.posixToParts settings.zone day.start

                    isFocused =
                        Maybe.map (\fday -> generatePickerDay settings fday == day) settings.focusedDate
                            |> Maybe.withDefault False

                    ( isPicked, isBetween ) =
                        DurationUtilities.dayPickedOrBetween settings.zone day model.hovered ( model.startSelectionTuple, model.endSelectionTuple )

                    dayStyles =
                        durationDayStyles settings.theme (dayParts.month /= currentMonth) day.disabled isPicked isFocused isBetween

                    dayClasses =
                        durationDayClasses settings.theme (dayParts.month /= currentMonth) day.disabled isPicked isFocused isBetween

                    startOrEndStyles =
                        durationStartOrEndStyles settings.theme
                            (DurationUtilities.isPickedDaySelectionTuple day model.startSelectionTuple)
                            (DurationUtilities.isPickedDaySelectionTuple day model.endSelectionTuple)

                    startOrEndClasses =
                        durationStartOrEndClasses settings.theme
                            (DurationUtilities.isPickedDaySelectionTuple day model.startSelectionTuple)
                            (DurationUtilities.isPickedDaySelectionTuple day model.endSelectionTuple)

                    classes =
                        dayClasses ++ " " ++ startOrEndClasses
                in
                ( Css.batch [ dayStyles, startOrEndStyles ], classes )

        ( ( previousYearMsg, previousMonthMsg ), ( nextYearMsg, nextMonthMsg ) ) =
            case side of
                Left ->
                    ( ( Just <| model.internalMsg <| PrevYear, Just <| model.internalMsg <| PrevMonth ), ( Nothing, Nothing ) )

                Right ->
                    ( ( Nothing, Nothing ), ( Just <| model.internalMsg <| NextYear, Just <| model.internalMsg <| NextMonth ) )
    in
    viewCalendarContainer settings.theme
        attrs
        [ viewCalendarHeader settings.theme
            { yearText = year
            , monthText = monthName
            , previousYearMsg = previousYearMsg
            , previousMonthMsg = previousMonthMsg
            , nextYearMsg = nextYearMsg
            , nextMonthMsg = nextMonthMsg
            , formattedDay = settings.formattedDay
            , firstWeekDay = settings.firstWeekDay
            , showCalendarWeekNumbers = settings.showCalendarWeekNumbers
            }
        , viewCalendarMonth settings.theme
            { weeks = weeks
            , onMouseOutMsg = model.internalMsg ClearHoveredDay
            , zone = settings.zone
            , showCalendarWeekNumbers = settings.showCalendarWeekNumbers
            , dayProps =
                { dayStylesFn = dayStylesFn
                , onDayClickMsg = \day -> model.internalMsg (SetRange day)
                , onDayMouseOverMsg = \day -> model.internalMsg (SetHoveredDay day)
                }
            }
        ]


viewFooter : Settings -> Bool -> PickerDay -> Model msg -> Html.Styled.Html msg
viewFooter settings timePickerVisible baseDay model =
    let
        ( startSelectionTuple, endSelectionTuple ) =
            determineDateTimeRange settings.zone model.startSelectionTuple model.endSelectionTuple model.hovered

        { selectableStartHours, selectableStartMinutes, selectableEndHours, selectableEndMinutes } =
            DurationUtilities.filterSelectableTimes settings.zone baseDay startSelectionTuple endSelectionTuple
    in
    viewFooterContainer settings.theme
        []
        [ case startSelectionTuple of
            Just ( _, startSelection ) ->
                let
                    dateTimeString =
                        settings.dateStringFn settings.zone startSelection

                    timePickerProps =
                        { zone = settings.zone
                        , selectionTuple = startSelectionTuple
                        , onHourChangeDecoder = Decode.map model.internalMsg (Decode.map (SetHour Start) targetValueIntParse)
                        , onMinuteChangeDecoder = Decode.map model.internalMsg (Decode.map (SetMinute Start) targetValueIntParse)
                        , selectableHours = selectableStartHours
                        , selectableMinutes = selectableStartMinutes
                        }
                in
                viewFooterBody settings.theme
                    { dateTimeString = dateTimeString
                    , timePickerView =
                        case settings.timePickerVisibility of
                            NeverVisible ->
                                text ""

                            Toggleable timePickerSettings ->
                                viewToggleableTimePicker settings.theme
                                    { timePickerProps = timePickerProps
                                    , timeString = timePickerSettings.timeStringFn timePickerProps.zone startSelection
                                    , isTimePickerVisible = timePickerVisible
                                    , onTimePickerToggleMsg = model.internalMsg ToggleTimePickerVisibility
                                    }

                            AlwaysVisible _ ->
                                viewAlwaysVisibleTimePicker settings.theme
                                    { timePickerProps = timePickerProps }
                    }

            Nothing ->
                viewEmpty settings.theme
        , viewDateTimesSeparator
        , case endSelectionTuple of
            Just ( _, endSelection ) ->
                let
                    dateTimeString =
                        settings.dateStringFn settings.zone endSelection

                    timePickerProps =
                        { zone = settings.zone
                        , selectionTuple = endSelectionTuple
                        , onHourChangeDecoder = Decode.map model.internalMsg (Decode.map (SetHour End) targetValueIntParse)
                        , onMinuteChangeDecoder = Decode.map model.internalMsg (Decode.map (SetMinute End) targetValueIntParse)
                        , selectableHours = selectableEndHours
                        , selectableMinutes = selectableEndMinutes
                        }
                in
                viewFooterBody settings.theme
                    { dateTimeString = dateTimeString
                    , timePickerView =
                        case settings.timePickerVisibility of
                            NeverVisible ->
                                text ""

                            Toggleable timePickerSettings ->
                                viewToggleableTimePicker settings.theme
                                    { timePickerProps = timePickerProps
                                    , timeString = timePickerSettings.timeStringFn timePickerProps.zone endSelection
                                    , isTimePickerVisible = timePickerVisible
                                    , onTimePickerToggleMsg = model.internalMsg ToggleTimePickerVisibility
                                    }

                            AlwaysVisible _ ->
                                viewAlwaysVisibleTimePicker settings.theme
                                    { timePickerProps = timePickerProps }
                    }

            Nothing ->
                viewEmpty settings.theme
        ]


viewDateTimesSeparator : Html.Styled.Html msg
viewDateTimesSeparator =
    div []
        [ Icons.arrowRight
            |> Icons.withSize 16
            |> Icons.toHtml []
            |> fromUnstyled
        ]
