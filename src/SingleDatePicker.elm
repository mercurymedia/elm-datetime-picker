module SingleDatePicker exposing
    ( DatePicker, Msg, init, view, update, subscriptions
    , openPicker, closePicker, updatePickerPosition
    , isOpen, hasError
    , viewDateInput
    )

{-| A date picker component for a single datetime.


# Architecture

@docs DatePicker, Msg, init, view, update, subscriptions


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
import DatePicker.Settings exposing (..)
import DatePicker.SingleUtilities as SingleUtilities
import DatePicker.Utilities as Utilities exposing (PickerDay, classPrefix)
import DatePicker.ViewComponents exposing (..)
import Html exposing (Html)
import Html.Events.Extra exposing (targetValueIntParse)
import Html.Styled exposing (div, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, id)
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
    , selectionTuple : Maybe ( PickerDay, Posix )
    , alignment : Maybe Alignment
    , dateInput : DateInput.DateInput msg
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
        , selectionTuple = Nothing
        , alignment = Nothing
        , dateInput =
            DateInput.init
                (internalMsg << HandleDateInputUpdate)
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
been picked) as well as the picked time. A common example of a default time
would be the datetime for the current day.
-}
openPicker : String -> Settings -> Posix -> Maybe Posix -> DatePicker msg -> ( DatePicker msg, Cmd msg )
openPicker triggerElementId settings baseTime pickedTime (DatePicker model) =
    let
        ( ( updatedPicker, _ ), cmd ) =
            update settings (OpenPicker baseTime pickedTime triggerElementId) (DatePicker model)
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
hasError : DatePicker msg -> Bool
hasError (DatePicker { dateInput }) =
    DateInput.hasError dateInput


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


{-| Internal Msg's to update the picker.
-}
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
    | GotAlignment (Result Dom.Error Alignment)
    | SetPresetDate PresetDateConfig
    | HandleDateInputUpdate DateInput.Msg
    | OpenPicker Posix (Maybe Posix) String


{-| Update the SingleDatePicker according to the given internal msg.

Returns the updated picker and the currently selected datetime, if available.

-}
update : Settings -> Msg -> DatePicker msg -> ( ( DatePicker msg, Maybe Posix ), Cmd msg )
update settings msg (DatePicker model) =
    let
        pickedTime =
            Maybe.map (\( _, time ) -> time) model.selectionTuple
    in
    case model.status of
        Open timePickerVisible baseDay ->
            case msg of
                NextMonth ->
                    ( ( DatePicker { model | viewOffset = model.viewOffset + 1 }, pickedTime ), Cmd.none )

                PrevMonth ->
                    ( ( DatePicker { model | viewOffset = model.viewOffset - 1 }, pickedTime ), Cmd.none )

                NextYear ->
                    ( ( DatePicker { model | viewOffset = model.viewOffset + 12 }, pickedTime ), Cmd.none )

                PrevYear ->
                    ( ( DatePicker { model | viewOffset = model.viewOffset - 12 }, pickedTime ), Cmd.none )

                SetHoveredDay pickerDay ->
                    ( ( DatePicker { model | hovered = Just pickerDay }, pickedTime ), Cmd.none )

                ClearHoveredDay ->
                    ( ( DatePicker { model | hovered = Nothing }, pickedTime ), Cmd.none )

                SetDay pickerDay ->
                    let
                        newSelectionTuple =
                            SingleUtilities.selectTime settings.zone baseDay (SingleUtilities.Day pickerDay) model.selectionTuple
                    in
                    ( updateSelection settings baseDay newSelectionTuple ( DatePicker model, pickedTime )
                    , Cmd.none
                    )

                SetHour hour ->
                    let
                        newSelectionTuple =
                            SingleUtilities.selectTime settings.zone baseDay (SingleUtilities.Hour hour) model.selectionTuple
                    in
                    ( updateSelection settings baseDay newSelectionTuple ( DatePicker model, pickedTime )
                    , Cmd.none
                    )

                SetMinute minute ->
                    let
                        newSelectionTuple =
                            SingleUtilities.selectTime settings.zone baseDay (SingleUtilities.Minute minute) model.selectionTuple
                    in
                    ( updateSelection settings baseDay newSelectionTuple ( DatePicker model, pickedTime )
                    , Cmd.none
                    )

                ToggleTimePickerVisibility ->
                    case settings.timePickerVisibility of
                        Toggleable _ ->
                            ( ( DatePicker { model | status = Open (not timePickerVisible) baseDay }, pickedTime ), Cmd.none )

                        _ ->
                            ( ( DatePicker model, pickedTime ), Cmd.none )

                Close ->
                    ( ( DatePicker { model | status = Closed, alignment = Nothing }, pickedTime ), Cmd.none )

                SetPresetDate presetDate ->
                    let
                        presetPickerDay =
                            generatePickerDay settings presetDate.date
                    in
                    ( updateSelection settings baseDay (Just ( presetPickerDay, presetPickerDay.start )) ( DatePicker model, pickedTime )
                    , Cmd.none
                    )

                GotAlignment result ->
                    case result of
                        Ok alignment ->
                            ( ( DatePicker { model | alignment = Just alignment }, pickedTime ), Cmd.none )

                        Err _ ->
                            ( ( DatePicker model, pickedTime ), Cmd.none )

                HandleDateInputUpdate subMsg ->
                    let
                        ( updatedDateInput, dateInputCmd ) =
                            DateInput.update (dateInputConfig settings) subMsg model.dateInput
                    in
                    case DateInput.toPosix settings.zone updatedDateInput of
                        Just posix ->
                            let
                                pickerDay =
                                    generatePickerDay settings posix

                                newSelectionTuple =
                                    ( pickerDay, posix )

                                ( DatePicker updatedModel, updatedSelection ) =
                                    updateSelection settings baseDay (Just newSelectionTuple) ( DatePicker model, pickedTime )
                            in
                            ( ( DatePicker { updatedModel | dateInput = updatedDateInput }, updatedSelection ), dateInputCmd )

                        Nothing ->
                            ( ( DatePicker { model | dateInput = updatedDateInput, selectionTuple = Nothing }, Nothing ), dateInputCmd )

                _ ->
                    ( ( DatePicker model, pickedTime ), Cmd.none )

        Closed ->
            case msg of
                HandleDateInputUpdate subMsg ->
                    let
                        ( updatedDateInput, dateInputCmd ) =
                            DateInput.update (dateInputConfig settings) subMsg model.dateInput
                    in
                    ( ( DatePicker { model | dateInput = updatedDateInput }, pickedTime ), dateInputCmd )

                OpenPicker baseTime pickedTime_ triggerElementId ->
                    let
                        basePickerDay =
                            generatePickerDay settings baseTime

                        newSelectionTuple =
                            Maybe.map (\time -> ( generatePickerDay settings time, time )) pickedTime_

                        timePickerVisible =
                            isTimePickerVisible settings.timePickerVisibility

                        status =
                            Open timePickerVisible basePickerDay

                        ( DatePicker updatedModel, updatedPickedTime ) =
                            updateSelection settings basePickerDay newSelectionTuple ( DatePicker model, pickedTime )
                    in
                    ( ( DatePicker { updatedModel | status = status }, updatedPickedTime )
                    , Alignment.init
                        { triggerId = triggerElementId
                        , pickerId = settings.id
                        }
                        (model.internalMsg << GotAlignment)
                    )

                _ ->
                    ( ( DatePicker model, pickedTime ), Cmd.none )


updateSelection : Settings -> PickerDay -> Maybe ( PickerDay, Posix ) -> ( DatePicker msg, Maybe Posix ) -> ( DatePicker msg, Maybe Posix )
updateSelection settings baseDay newSelectionTuple ( DatePicker model, pickedTime ) =
    case newSelectionTuple of
        Just ( newPickerDay, newSelection ) ->
            ( DatePicker
                { model
                    | selectionTuple = Just ( newPickerDay, newSelection )
                    , dateInput = DateInput.updateFromPosix (dateInputConfig settings) settings.zone newSelection model.dateInput
                    , viewOffset = Utilities.calculateViewOffset settings.zone baseDay.start (Just newPickerDay.start)
                }
            , Just newSelection
            )

        Nothing ->
            ( DatePicker model, pickedTime )


determineDateTime : Zone -> Maybe ( PickerDay, Posix ) -> Maybe PickerDay -> Maybe ( PickerDay, Posix )
determineDateTime zone selectionTuple hoveredDay =
    let
        hovered =
            Maybe.andThen Utilities.showHoveredIfEnabled hoveredDay
    in
    case hovered of
        Just h ->
            SingleUtilities.selectDay zone selectionTuple h

        Nothing ->
            selectionTuple


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
                , class (classPrefix settings.theme.classNamePrefix "single")
                , css styles
                ]
                [ viewPresets [] settings model
                , viewPicker [] settings timePickerVisible baseDay model
                ]

        Closed ->
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


{-| The date input view with the date picker opening on click.
Pass it the configured settings, the base time, the picked time
and the date picker instance you wish to view.
-}
viewDateInput : List (Html.Attribute msg) -> Settings -> Posix -> Maybe Posix -> DatePicker msg -> Html msg
viewDateInput attrs settings baseTime maybePickedTime (DatePicker model) =
    viewDateInputStyled (Utilities.toStyledAttrs attrs) settings baseTime maybePickedTime (DatePicker model)
        |> toUnstyled


viewDateInputStyled : List (Html.Styled.Attribute msg) -> Settings -> Posix -> Maybe Posix -> DatePicker msg -> Html.Styled.Html msg
viewDateInputStyled attrs settings baseTime maybePickedTime (DatePicker model) =
    let
        onClickMsg =
            model.internalMsg <|
                OpenPicker baseTime maybePickedTime (DateInput.containerId <| dateInputConfig settings)

        isPickerOpen =
            isOpen (DatePicker model)
    in
    DateInput.viewContainer settings.theme
        (id (DateInput.containerId <| dateInputConfig settings) :: attrs)
        [ DateInput.view
            [ onClick onClickMsg
            , css
                (Alignment.dateInputStylesFromAlignment settings.theme
                    isPickerOpen
                    (Alignment.calcDateInputWidth settings.theme settings.showCalendarWeekNumbers)
                    model.alignment
                )
            ]
            (dateInputConfig settings)
            model.dateInput
        , case model.status of
            Open timePickerVisible baseDay ->
                viewContainer settings.theme
                    [ id settings.id
                    , class (classPrefix settings.theme.classNamePrefix "single")
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
viewPicker attrs settings timePickerVisible baseDay model =
    let
        offsetTime =
            Time.add Month model.viewOffset settings.zone baseDay.start

        monthName =
            Time.toMonth settings.zone offsetTime |> settings.formattedMonth

        year =
            Time.toYear settings.zone offsetTime |> String.fromInt

        allowedTimesOfDayFn =
            Maybe.map .allowedTimesOfDay (getTimePickerSettings settings)

        weeks =
            Utilities.monthData settings.zone settings.isDayDisabled settings.firstWeekDay allowedTimesOfDayFn offsetTime

        currentMonth =
            Time.posixToParts settings.zone offsetTime |> .month

        dayStylesFn =
            \day ->
                let
                    dayParts =
                        Time.posixToParts settings.zone day.start

                    isPicked =
                        Maybe.map (\( pickerDay, _ ) -> pickerDay == day) model.selectionTuple
                            |> Maybe.withDefault False

                    isFocused =
                        Maybe.map (\fday -> generatePickerDay settings fday == day) settings.focusedDate
                            |> Maybe.withDefault False
                in
                ( singleDayStyles settings.theme
                    (dayParts.month /= currentMonth)
                    day.disabled
                    isPicked
                    isFocused
                , singleDayClasses
                    settings.theme
                    (dayParts.month /= currentMonth)
                    day.disabled
                    isPicked
                    isFocused
                )
    in
    viewPickerContainer settings.theme
        attrs
        [ viewCalendarContainer settings.theme
            []
            [ viewCalendarHeader settings.theme
                { yearText = year
                , monthText = monthName
                , previousYearMsg = Just <| model.internalMsg <| PrevYear
                , previousMonthMsg = Just <| model.internalMsg <| PrevMonth
                , nextYearMsg = Just <| model.internalMsg <| NextYear
                , nextMonthMsg = Just <| model.internalMsg <| NextMonth
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
                    , onDayClickMsg = \day -> model.internalMsg (SetDay day)
                    , onDayMouseOverMsg = \day -> model.internalMsg (SetHoveredDay day)
                    }
                }
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
                        PresetDate config ->
                            viewPresetTab settings.theme
                                []
                                { title = config.title
                                , active = isPresetDateActive settings model.selectionTuple config
                                , onClickMsg = model.internalMsg (SetPresetDate config)
                                }

                        _ ->
                            text ""
                )
                settings.presets
            )

    else
        text ""


viewFooter : Settings -> Bool -> PickerDay -> Model msg -> Html.Styled.Html msg
viewFooter settings timePickerVisible baseDay model =
    let
        displayTime =
            determineDateTime settings.zone model.selectionTuple model.hovered

        { selectableHours, selectableMinutes } =
            SingleUtilities.filterSelectableTimes settings.zone baseDay model.selectionTuple
    in
    viewFooterContainer settings.theme
        []
        [ case displayTime of
            Nothing ->
                viewEmpty settings.theme

            Just ( _, selection ) ->
                let
                    dateTimeString =
                        settings.dateStringFn settings.zone selection

                    timePickerProps =
                        { zone = settings.zone
                        , selectionTuple = displayTime
                        , onHourChangeDecoder = Decode.map model.internalMsg (Decode.map SetHour targetValueIntParse)
                        , onMinuteChangeDecoder = Decode.map model.internalMsg (Decode.map SetMinute targetValueIntParse)
                        , selectableHours = selectableHours
                        , selectableMinutes = selectableMinutes
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
                                    , timeString = timePickerSettings.timeStringFn timePickerProps.zone selection
                                    , isTimePickerVisible = timePickerVisible
                                    , onTimePickerToggleMsg = model.internalMsg ToggleTimePickerVisibility
                                    }

                            AlwaysVisible _ ->
                                viewAlwaysVisibleTimePicker settings.theme
                                    { timePickerProps = timePickerProps }
                    }
        ]
