module SingleDatePicker exposing
    ( DatePicker, Msg, init, view, update, subscriptions
    , openPicker, closePicker, openPickerOutsideHierarchy, updatePickerPosition
    , isOpen
    )

{-| A date picker component for a single datetime.


# Architecture

@docs DatePicker, Msg, init, view, update, subscriptions


# Externally Triggered Actions

@docs openPicker, closePicker, openPickerOutsideHierarchy, updatePickerPosition


# Query

@docs isOpen

-}

import Browser.Dom as Dom
import Browser.Events
import DatePicker.Settings exposing (..)
import DatePicker.SingleUtilities as SingleUtilities
import DatePicker.Utilities as Utilities exposing (DomLocation(..), PickerDay)
import DatePicker.ViewComponents exposing (..)
import Html exposing (Html)
import Html.Events.Extra exposing (targetValueIntParse)
import Html.Styled exposing (text, toUnstyled)
import Html.Styled.Attributes exposing (class, id)
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
        , selectionTuple = Nothing
        , domLocation = InsideHierarchy
        }


{-| Events external to the picker to which it is subscribed.
-}
subscriptions : Settings -> DatePicker msg -> Sub msg
subscriptions { id } (DatePicker model) =
    case model.status of
        Open _ _ ->
            Browser.Events.onMouseDown (Utilities.clickedOutsidePicker id (model.internalMsg Close))

        Closed ->
            Sub.none


{-| Open the provided date picker and receive the updated picker instance. Also
takes a default time the picker should center on (in the event a time has not yet
been picked) as well as the picked time. A common example of a default time
would be the datetime for the current day.
-}
openPicker : Settings -> Posix -> Maybe Posix -> DatePicker msg -> DatePicker msg
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


{-| Open the provided date picker outside the DOM hierarchy. Uses the openPicker function
and additionally takes an id of the trigger DOM element (e.g. a button) to manually attach
the picker's position to it. Returns the updated picker instance plus the necessary command
in order to find DOM elements and their positions.
-}
openPickerOutsideHierarchy : String -> Settings -> Posix -> Maybe Posix -> DatePicker msg -> ( DatePicker msg, Cmd msg )
openPickerOutsideHierarchy triggerElementId settings baseTime pickedTime (DatePicker model) =
    let
        (DatePicker openedModel) =
            openPicker settings baseTime pickedTime (DatePicker model)

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
    | SetDomElements { triggerDomElement : Dom.Element, pickerDomElement : Dom.Element }
    | SetPresetDate PresetDateConfig
    | NoOp


{-| Update the SingleDatePicker according to the given internal msg.

Returns the updated picker and the currently selected datetime, if available.

-}
update : Settings -> Msg -> DatePicker msg -> ( DatePicker msg, Maybe Posix )
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

                SetPresetDate presetDate ->
                    let
                        presetPickerDay =
                            generatePickerDay settings presetDate.date

                        viewOffset =
                            Utilities.calculateViewOffset settings.zone baseDay.start (Just presetPickerDay.start)
                    in
                    ( DatePicker
                        { model
                            | selectionTuple = Just ( presetPickerDay, presetPickerDay.start )
                            , viewOffset = viewOffset
                        }
                    , Just presetPickerDay.start
                    )

                NoOp ->
                    ( DatePicker model, Nothing )

        Closed ->
            ( DatePicker model, Nothing )


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
    viewContainer settings.theme
        ([ id settings.id, class (classPrefix settings.theme.classNamePrefix "single") ] ++ attributes)
        [ viewPresets settings model
        , viewPickerContainer settings.theme
            []
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
        ]


viewPresets : Settings -> Model msg -> Html.Styled.Html msg
viewPresets settings model =
    if List.length settings.presets > 0 then
        viewPresetsContainer settings.theme
            []
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
                in
                viewFooterBody settings.theme
                    { timePickerProps =
                        { zone = settings.zone
                        , selectionTuple = displayTime
                        , onHourChangeDecoder = Decode.map model.internalMsg (Decode.map SetHour targetValueIntParse)
                        , onMinuteChangeDecoder = Decode.map model.internalMsg (Decode.map SetMinute targetValueIntParse)
                        , selectableHours = selectableHours
                        , selectableMinutes = selectableMinutes
                        }
                    , isTimePickerVisible = timePickerVisible
                    , timePickerVisibility = settings.timePickerVisibility
                    , selection = selection
                    , onTimePickerToggleMsg = model.internalMsg ToggleTimePickerVisibility
                    , dateTimeString = dateTimeString
                    }
        ]
