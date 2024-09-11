module DurationDatePicker exposing
    ( DatePicker, Msg, init, view, update, subscriptions
    , openPicker, closePicker, openPickerOutsideHierarchy, updatePickerPosition
    , isOpen
    )

{-| A date picker component for picking a datetime range.


# Architecture

@docs DatePicker, Msg, init, view, update, subscriptions


# Externally Triggered Actions

@docs openPicker, closePicker, openPickerOutsideHierarchy, updatePickerPosition


# Query

@docs isOpen

-}

import Browser.Dom as Dom
import Browser.Events
import Css
import DatePicker.DurationUtilities as DurationUtilities
import DatePicker.Icons as Icons
import DatePicker.Settings exposing (..)
import DatePicker.Utilities as Utilities exposing (DomLocation(..), PickerDay)
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
        }


{-| Events external to the picker to which it is subscribed.
-}
subscriptions : Settings -> DatePicker msg -> Sub msg
subscriptions settings (DatePicker model) =
    case model.status of
        Open _ _ ->
            Browser.Events.onMouseDown (Utilities.clickedOutsidePicker settings.id (model.internalMsg Close))

        Closed ->
            Sub.none


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


{-| Open the provided date picker outside the DOM hierarchy. Uses the openPicker function
and additionally takes an id of the trigger DOM element (e.g. a button) to manually attach
the picker's position to it. Returns the updated picker instance plus the necessary command
in order to find DOM elements and their positions.
-}
openPickerOutsideHierarchy : String -> Settings -> Posix -> Maybe Posix -> Maybe Posix -> DatePicker msg -> ( DatePicker msg, Cmd msg )
openPickerOutsideHierarchy triggerElementId settings baseTime start end (DatePicker model) =
    let
        (DatePicker openedModel) =
            openPicker settings baseTime start end (DatePicker model)

        initialDomElements =
            { triggerDomElement = { id = triggerElementId, element = Nothing }
            , pickerDomElement = { id = settings.id, element = Nothing }
            }

        (DatePicker updatedModel) =
            DatePicker { openedModel | domLocation = OutsideHierarchy initialDomElements }

        cmd =
            updatePickerPosition (DatePicker updatedModel)
    in
    ( DatePicker updatedModel, cmd )


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
    | SetRange PickerDay
    | ToggleTimePickerVisibility
    | SetHour StartOrEnd Int
    | SetMinute StartOrEnd Int
    | Close
    | SetDomElements { triggerDomElement : Dom.Element, pickerDomElement : Dom.Element }
    | SetPresetRange PresetRangeConfig
    | NoOp


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
                    ( DatePicker { model | domLocation = updatedDomLocation }, Nothing )

                SetPresetRange { range } ->
                    let
                        startPickerDay =
                            generatePickerDay settings range.start

                        endPickerDay =
                            generatePickerDay settings range.end

                        viewOffset =
                            Utilities.calculateViewOffset settings.zone baseDay.start (Just startPickerDay.start)
                    in
                    processSelection { model | viewOffset = viewOffset }
                        ( Just ( startPickerDay, startPickerDay.start ), Just ( endPickerDay, endPickerDay.end ) )

                NoOp ->
                    ( DatePicker model, Nothing )

        Closed ->
            ( DatePicker model, Nothing )


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
    case ( model.domLocation, model.status ) of
        ( InsideHierarchy, Open timePickerVisible baseDay ) ->
            viewPicker [] settings timePickerVisible baseDay model

        ( OutsideHierarchy elements, Open timePickerVisible baseDay ) ->
            viewPicker (Utilities.outsideHierarchyStyles elements)
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
                in
                viewFooterBody settings.theme
                    { timePickerProps =
                        { zone = settings.zone
                        , selectionTuple = startSelectionTuple
                        , onHourChangeDecoder = Decode.map model.internalMsg (Decode.map (SetHour Start) targetValueIntParse)
                        , onMinuteChangeDecoder = Decode.map model.internalMsg (Decode.map (SetMinute Start) targetValueIntParse)
                        , selectableHours = selectableStartHours
                        , selectableMinutes = selectableStartMinutes
                        }
                    , isTimePickerVisible = timePickerVisible
                    , timePickerVisibility = settings.timePickerVisibility
                    , selection = startSelection
                    , onTimePickerToggleMsg = model.internalMsg ToggleTimePickerVisibility
                    , dateTimeString = dateTimeString
                    }

            Nothing ->
                viewEmpty settings.theme
        , viewDateTimesSeparator
        , case endSelectionTuple of
            Just ( _, endSelection ) ->
                let
                    dateTimeString =
                        settings.dateStringFn settings.zone endSelection
                in
                viewFooterBody settings.theme
                    { timePickerProps =
                        { zone = settings.zone
                        , selectionTuple = endSelectionTuple
                        , onHourChangeDecoder = Decode.map model.internalMsg (Decode.map (SetHour End) targetValueIntParse)
                        , onMinuteChangeDecoder = Decode.map model.internalMsg (Decode.map (SetMinute End) targetValueIntParse)
                        , selectableHours = selectableEndHours
                        , selectableMinutes = selectableEndMinutes
                        }
                    , isTimePickerVisible = timePickerVisible
                    , timePickerVisibility = settings.timePickerVisibility
                    , selection = endSelection
                    , onTimePickerToggleMsg = model.internalMsg ToggleTimePickerVisibility
                    , dateTimeString = dateTimeString
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
