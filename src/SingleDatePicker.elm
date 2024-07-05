module SingleDatePicker exposing
    ( DatePicker, Msg, init, view, update, subscriptions
    , openPicker, closePicker, openPickerOutsideHierarchy, updatePickerPosition
    , isOpen
    )

{-| A date picker component for a single datetime.


# Architecture

@docs DatePicker, Msg, init, view, update, subscriptions


# Settings

@docs Settings, defaultSettings, TimePickerVisibility
@docs TimePickerSettings, defaultTimePickerSettings


# Externally Triggered Actions

@docs openPicker, closePicker, openPickerOutsideHierarchy, updatePickerPosition


# Query

@docs isOpen

-}

import Browser.Dom as Dom
import Browser.Events
import Date
import DatePicker.Icons as Icons
import DatePicker.Settings exposing (..)
import DatePicker.SingleUtilities as SingleUtilities
import DatePicker.Styles
import DatePicker.Utilities as Utilities exposing (DomLocation(..), PickerDay)
import Html exposing (Html, button, div, select, span, text)
import Html.Attributes exposing (class, disabled, id, style, type_)
import Html.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Decode
import List.Extra as List
import Task
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


classPrefix : String
classPrefix =
    "elm-datetimepicker--"


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
    case ( model.domLocation, model.status ) of
        ( InsideHierarchy, Open timePickerVisible baseDay ) ->
            viewPicker [] settings timePickerVisible baseDay model

        ( OutsideHierarchy { triggerDomElement, pickerDomElement }, Open timePickerVisible baseDay ) ->
            let
                attributes =
                    case ( triggerDomElement.element, pickerDomElement.element ) of
                        ( Just triggerEl, Just pickerEl ) ->
                            Utilities.calculatePositionStyles { triggerEl = triggerEl, pickerEl = pickerEl }

                        _ ->
                            -- hide picker element until the DOM elements have been found and the positions have been calculated correctly
                            [ style "visibility" "hidden" ]
            in
            viewPicker attributes settings timePickerVisible baseDay model

        _ ->
            text ""


viewPicker : List (Html.Attribute msg) -> Settings -> Bool -> PickerDay -> Model msg -> Html msg
viewPicker attributes settings timePickerVisible baseDay model =
    let
        offsetTime =
            Time.add Month model.viewOffset settings.zone baseDay.start
    in
    div ([ id settings.id, class (classPrefix ++ "container"), class (classPrefix ++ "single") ] ++ attributes)
        [ if List.length settings.presets > 0 then
            div [ class (classPrefix ++ "presets-container") ]
                (List.map
                    (\preset ->
                        case preset of
                            PresetDate config ->
                                viewPresetTab settings model.selectionTuple model.internalMsg config

                            _ ->
                                text ""
                    )
                    settings.presets
                )

          else
            text ""
        , div [ class (classPrefix ++ "picker-container") ]
            [ div [ class (classPrefix ++ "calendar-container") ]
                [ viewCalendarHeader settings model offsetTime
                , viewMonth settings model offsetTime
                ]
            , viewFooter settings timePickerVisible baseDay model
            ]
        ]


viewPresetTab : Settings -> Maybe ( PickerDay, Posix ) -> (Msg -> msg) -> PresetDateConfig -> Html msg
viewPresetTab settings selectionTuple internalMsg presetDate =
    let
        activeClass =
            if isPresetDateActive settings selectionTuple presetDate then
                classPrefix ++ "active"

            else
                ""
    in
    div
        [ class (classPrefix ++ "preset")
        , class activeClass
        , onClick <| internalMsg (SetPresetDate presetDate)
        ]
        [ text presetDate.title
        ]


viewCalendarHeader : Settings -> Model msg -> Posix -> Html msg
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
                    , onClick <| model.internalMsg <| PrevYear
                    ]
                    [ Icons.chevronsLeft
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    ]
                , div
                    [ id "previous-month"
                    , class (classPrefix ++ "calendar-header-chevron")
                    , onClick <| model.internalMsg <| PrevMonth
                    ]
                    [ Icons.chevronLeft
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    ]
                ]
            , div
                [ class (classPrefix ++ "calendar-header-text") ]
                [ div []
                    [ span [ id "month" ] [ text monthName ]
                    , span [] [ text " " ]
                    , span [ id "year" ] [ text year ]
                    ]
                ]
            , div [ class (classPrefix ++ "calendar-header-navigation") ]
                [ div
                    [ id "next-month"
                    , class (classPrefix ++ "calendar-header-chevron")
                    , onClick <| model.internalMsg <| NextMonth
                    ]
                    [ Icons.chevronRight
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    ]
                , div
                    [ id "next-year"
                    , class (classPrefix ++ "calendar-header-chevron")
                    , onClick <| model.internalMsg <| NextYear
                    ]
                    [ Icons.chevronsRight
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    ]
                ]
            ]
        , viewWeekHeader settings
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
        [ div [] (List.map (viewWeek settings currentMonth model) weeks)
        ]


viewWeek : Settings -> Month -> Model msg -> List PickerDay -> Html msg
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


viewDay : Settings -> Model msg -> Month -> PickerDay -> Html msg
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
        , onClick <| model.internalMsg (SetDay day)
        , onMouseOver <| model.internalMsg (SetHoveredDay day)
        ]
        [ text (String.fromInt dayParts.day) ]


viewFooter : Settings -> Bool -> PickerDay -> Model msg -> Html msg
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


viewDateOrDateTime : Settings -> Bool -> PickerDay -> Model msg -> Posix -> Html msg
viewDateOrDateTime settings timePickerVisible baseDay model selection =
    Maybe.map
        (\timePickerSettings ->
            if settings.timePickerVisibility == NeverVisible && timeIsStartOfDay settings selection then
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


viewDateTime : Settings -> Bool -> PickerDay -> Model msg -> Posix -> TimePickerSettings -> Html msg
viewDateTime settings timePickerVisible baseDay model selection timePickerSettings =
    div [ class (classPrefix ++ "footer-datetime-container") ]
        [ viewDate settings selection
        , viewTimeOrTimePicker settings timePickerVisible baseDay model selection timePickerSettings
        ]


viewTimeOrTimePicker : Settings -> Bool -> PickerDay -> Model msg -> Posix -> TimePickerSettings -> Html msg
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
                , div [ class (classPrefix ++ "time-picker-toggle"), onClick <| model.internalMsg ToggleTimePickerVisibility ]
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


timeIsStartOfDay : Settings -> Posix -> Bool
timeIsStartOfDay settings time =
    let
        { hour, minute } =
            Time.posixToParts settings.zone time
    in
    hour == 0 && minute == 0


viewDate : Settings -> Posix -> Html msg
viewDate settings dateTime =
    span [ class (classPrefix ++ "selection-date") ]
        [ Icons.calendar
            |> Icons.withSize 16
            |> Icons.toHtml []
        , text (settings.dateStringFn settings.zone dateTime)
        ]


viewTimePicker : Settings -> Model msg -> PickerDay -> Maybe ( PickerDay, Posix ) -> Html msg
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
            [ viewSelect [ id "hour-select", on "change" (Decode.map model.internalMsg (Decode.map SetHour targetValueIntParse)) ]
                (Utilities.generateHourOptions settings.zone selectionTuple selectableHours)
            , div [ class (classPrefix ++ "select-spacer") ] [ text ":" ]
            , viewSelect
                [ id "minute-select", on "change" (Decode.map model.internalMsg (Decode.map SetMinute targetValueIntParse)) ]
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
