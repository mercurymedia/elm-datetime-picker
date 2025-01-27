module DurationDatePicker exposing
    ( DatePicker, Msg, init, view, update, subscriptions
    , openPicker, closePicker, updatePickerPosition
    , isOpen
    )

{-| A date picker component for picking a datetime range.


# Architecture

@docs DatePicker, Msg, init, view, update, subscriptions


# Externally Triggered Actions

@docs openPicker, closePicker, updatePickerPosition


# Query

@docs isOpen

-}

import Browser.Dom as Dom
import Browser.Events
import Css
import DatePicker.Alignment as Alignment exposing (Alignment)
import DatePicker.DurationUtilities as DurationUtilities
import DatePicker.Icons as Icons
import DatePicker.Settings exposing (..)
import DatePicker.Utilities as Utilities exposing (DomLocation(..), PickerDay, classPrefix)
import DatePicker.ViewComponents exposing (..)
import Html exposing (Html)
import Html.Events.Extra exposing (targetValueIntParse)
import Html.Styled exposing (div, fromUnstyled, text, toUnstyled)
import Html.Styled.Attributes exposing (class, id, start)
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
        , alignment = Nothing
        }


{-| Events external to the picker to which it is subscribed.
-}
subscriptions : Settings -> DatePicker msg -> Sub msg
subscriptions settings (DatePicker model) =
    case model.status of
        Open _ _ ->
            Browser.Events.onMouseDown (Utilities.clickedOutsidePicker [ settings.id ] (model.internalMsg Close))

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


{-| Returns the command to update the trigger & picker DOM elements' instances.
Is used internally but can also be used externally in case of a changing viewport
(e.g. onScroll or onResize).
-}
updatePickerPosition : DatePicker msg -> Cmd msg
updatePickerPosition (DatePicker model) =
    case model.domLocation of
        OutsideHierarchy { triggerDomElement, pickerDomElement } ->
            Utilities.updateDomElements
                { triggerElementId = triggerDomElement.id
                , pickerElementId = pickerDomElement.id
                , onSuccess = \result -> model.internalMsg (SetDomElements result)
                , onError = model.internalMsg NoOp
                }

        InsideHierarchy ->
            Cmd.none


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
    | SetDomElements { triggerDomElement : Dom.Element, pickerDomElement : Dom.Element }
    | SetPresetRange PresetRangeConfig
    | OpenPicker Posix (Maybe Posix) (Maybe Posix) String
    | NoOp


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
                    ( ( DatePicker { model | viewOffset = model.viewOffset + 1 }, Nothing ), Cmd.none )

                PrevMonth ->
                    ( ( DatePicker { model | viewOffset = model.viewOffset - 1 }, Nothing ), Cmd.none )

                NextYear ->
                    ( ( DatePicker { model | viewOffset = model.viewOffset + 12 }, Nothing ), Cmd.none )

                PrevYear ->
                    ( ( DatePicker { model | viewOffset = model.viewOffset - 12 }, Nothing ), Cmd.none )

                SetHoveredDay pickerDay ->
                    ( ( DatePicker { model | hovered = Just pickerDay }, Nothing ), Cmd.none )

                ClearHoveredDay ->
                    ( ( DatePicker { model | hovered = Nothing }, Nothing ), Cmd.none )

                SetRange pickerDay ->
                    let
                        newSelection =
                            DurationUtilities.selectDay settings.zone model.startSelectionTuple model.endSelectionTuple pickerDay
                    in
                    ( updateSelection newSelection ( DatePicker model, pickedDuration )
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
                            ( updateSelection newSelection ( DatePicker model, pickedDuration )
                            , Cmd.none
                            )

                        End ->
                            let
                                newSelection =
                                    DurationUtilities.selectEndHour settings.zone baseDay model.startSelectionTuple model.endSelectionTuple hour
                            in
                            ( updateSelection newSelection ( DatePicker model, pickedDuration )
                            , Cmd.none
                            )

                SetMinute startOrEnd minute ->
                    case startOrEnd of
                        Start ->
                            let
                                newSelection =
                                    DurationUtilities.selectStartMinute settings.zone baseDay model.startSelectionTuple model.endSelectionTuple minute
                            in
                            ( updateSelection newSelection ( DatePicker model, pickedDuration )
                            , Cmd.none
                            )

                        End ->
                            let
                                newSelection =
                                    DurationUtilities.selectEndMinute settings.zone baseDay model.startSelectionTuple model.endSelectionTuple minute
                            in
                            ( updateSelection newSelection ( DatePicker model, pickedDuration )
                            , Cmd.none
                            )

                Close ->
                    ( ( DatePicker { model | status = Closed }, Nothing ), Cmd.none )

                GotAlignment result ->
                    case result of
                        Ok alignment ->
                            ( ( DatePicker { model | alignment = Just alignment }, Nothing ), Cmd.none )

                        Err _ ->
                            ( ( DatePicker model, Nothing ), Cmd.none )

                SetDomElements newDomElements ->
                    let
                        updatedDomLocation =
                            case model.domLocation of
                                OutsideHierarchy ({ triggerDomElement, pickerDomElement } as domElements) ->
                                    OutsideHierarchy
                                        { domElements
                                            | triggerDomElement = { triggerDomElement | element = Just newDomElements.triggerDomElement }
                                            , pickerDomElement = { pickerDomElement | element = Just newDomElements.pickerDomElement }
                                        }

                                InsideHierarchy ->
                                    InsideHierarchy
                    in
                    ( ( DatePicker { model | domLocation = updatedDomLocation }, Nothing ), Cmd.none )

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
                                |> updateSelection newSelection
                                |> updateViewOffset settings baseDay
                    in
                    ( updatedPickerAndDuration
                    , Cmd.none
                    )

                _ ->
                    ( ( DatePicker model, Nothing ), Cmd.none )

        Closed ->
            case msg of
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
                                |> updateSelection ( newStartSelectionTuple, newEndSelectionTuple )
                                |> updateViewOffset settings baseDay
                    in
                    ( ( DatePicker { updatedModel | status = status }
                      , updatedPickedDuration
                      )
                    , Alignment.init
                        { triggerId = triggerElementId
                        , pickerId = settings.id
                        }
                        (model.internalMsg << GotAlignment)
                    )

                _ ->
                    ( ( DatePicker model, Nothing ), Cmd.none )


updateSelection : ( Maybe ( PickerDay, Posix ), Maybe ( PickerDay, Posix ) ) -> ( DatePicker msg, Maybe ( Posix, Posix ) ) -> ( DatePicker msg, Maybe ( Posix, Posix ) )
updateSelection ( newStartSelectionTuple, newEndSelectionTuple ) ( DatePicker model, pickedDuration ) =
    let
        newDuration =
            Maybe.map2 (\( _, startSelection ) ( _, endSelection ) -> ( startSelection, endSelection )) newStartSelectionTuple newEndSelectionTuple
    in
    ( DatePicker
        { model
            | startSelectionTuple = newStartSelectionTuple
            , endSelectionTuple = newEndSelectionTuple
        }
    , newDuration
    )


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
            viewPicker [ Html.Styled.Attributes.css styles ]
                settings
                timePickerVisible
                baseDay
                model

        _ ->
            text ""


viewPicker : List (Html.Styled.Attribute msg) -> Settings -> Bool -> PickerDay -> Model msg -> Html.Styled.Html msg
viewPicker attributes settings timePickerVisible baseDay model =
    let
        leftViewTime =
            Time.add Month model.viewOffset settings.zone baseDay.start

        rightViewTime =
            Time.add Month (model.viewOffset + 1) settings.zone baseDay.start
    in
    viewContainer settings.theme
        ([ id settings.id, class (classPrefix settings.theme.classNamePrefix "duration") ] ++ attributes)
        [ viewPresets settings model
        , viewPickerContainer settings.theme
            []
            [ div [ Html.Styled.Attributes.css [ Css.displayFlex ] ]
                [ viewCalendar [ class (classPrefix settings.theme.classNamePrefix "left-calendar") ] settings model leftViewTime Left
                , viewCalendar [ class (classPrefix settings.theme.classNamePrefix "right-calendar") ] settings model rightViewTime Right
                ]
            , viewFooter settings timePickerVisible baseDay model
            ]
        ]


viewPresets : Settings -> Model msg -> Html.Styled.Html msg
viewPresets settings model =
    if List.length settings.presets > 0 then
        viewPresetsContainer settings.theme
            []
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
