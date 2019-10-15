module DurationDatePicker exposing
    ( DatePicker, init, view, subscriptions
    , Settings, defaultSettings
    , getPickedTimes, isOpen
    , open, close, setPickedTimes
    )

{-| A date picker component for picking a datetime range.


# Architecture

@docs DatePicker, init, view, subscriptions


# Settings

@docs Settings, defaultSettings


# Query

@docs getPickedTimes, isOpen


# Externally Triggered Actions

@docs open, close, setPickedTimes

-}

import Browser.Events
import DatePicker.Icons as Icons
import DatePicker.Styles
import DatePicker.Utilities as Utilities
import Html exposing (Html, button, div, select, text)
import Html.Attributes exposing (attribute, class, id, type_)
import Html.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Decode
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..))
import Time.Extra as Time exposing (Interval(..), Parts)


type alias Model msg =
    { settings : Settings msg
    , status : Status
    , hovered : Maybe Posix
    , leftPickerViewOffset : Int
    , rightPickerViewOffset : Int
    , stagedStartDay : Maybe Posix
    , stagedStartTime : Maybe Posix
    , stagedEndDay : Maybe Posix
    , stagedEndTime : Maybe Posix
    , pickedStartDateTime : Maybe Posix
    , pickedEndDateTime : Maybe Posix
    }


{-| The opaque type representing a particular date picker instance.
-}
type DatePicker msg
    = DatePicker (Model msg)


{-| The type facilitating the configuration of the datepicker settings.

You might be wondering what the difference between `today` and `baseTime` is. Indeed
in many cases, the `baseTime` that the picker is centered on will likely be the
day on which it is being used. However, it is possible that the implementer wishes to
center the picker on a day that is not the day on which it is being used.

-}
type alias Settings msg =
    -- this will get more advanced as we develop the api.
    -- Potential additions:
    -- * hide time selection (default to midnight)
    -- * hide prev and next year chevrons
    { formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , today : Maybe Posix
    , baseTime : Posix
    , dayDisabled : Posix -> Bool
    , internalMsg : DatePicker msg -> msg
    }


type Status
    = Closed Posix
    | Open Posix


type LeftOrRight
    = Left
    | Right


type StartOrEnd
    = Start
    | End


{-| A record of default settings for the date picker. Extend this if
you want to further customize the date picker.

Requires an internal msg (DatePicker -> msg) as well as a default
time the picker should center on (in the event a time has not yet
been picked). A common example of a default time would be the
datetime for the current day.

-}
defaultSettings : Posix -> (DatePicker msg -> msg) -> Settings msg
defaultSettings baseTime internalMsg =
    { formattedDay = Utilities.dayToNameString
    , formattedMonth = Utilities.monthToNameString
    , today = Nothing
    , baseTime = baseTime
    , dayDisabled = always False
    , internalMsg = internalMsg
    }


{-| Instantiates and returns a date picker.
-}
init : Settings msg -> DatePicker msg
init settings =
    DatePicker
        { settings = settings
        , status = Closed <| Time.floor Day Time.utc settings.baseTime
        , hovered = Nothing
        , leftPickerViewOffset = 0
        , rightPickerViewOffset = 0
        , stagedStartDay = Nothing
        , stagedStartTime = Nothing
        , stagedEndDay = Nothing
        , stagedEndTime = Nothing
        , pickedStartDateTime = Nothing
        , pickedEndDateTime = Nothing
        }


datePickerId : String
datePickerId =
    "date-picker-component"


{-| Events external to the picker to which it is subscribed.
-}
subscriptions : DatePicker msg -> Sub msg
subscriptions (DatePicker model) =
    case model.status of
        Open _ ->
            Browser.Events.onMouseDown (clickedOutsidePicker datePickerId (DatePicker model))

        Closed _ ->
            Sub.none


clickedOutsidePicker : String -> DatePicker msg -> Decode.Decoder msg
clickedOutsidePicker componentId (DatePicker model) =
    Decode.field "target" (Utilities.eventIsOutsideComponent componentId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| model.settings.internalMsg (update Close model)

                else
                    Decode.fail "inside component"
            )


calculatePickerOffsets : Posix -> Maybe Posix -> Maybe Posix -> ( Int, Int )
calculatePickerOffsets baseTime startDay endDay =
    let
        flooredBase =
            Time.floor Month Time.utc baseTime
    in
    case ( startDay, endDay ) of
        ( Nothing, Nothing ) ->
            ( 0, 1 )

        ( Just start, Nothing ) ->
            let
                flooredStart =
                    Time.floor Month Time.utc start

                leftOffset =
                    if Time.posixToMillis flooredBase <= Time.posixToMillis flooredStart then
                        Time.diff Month Time.utc flooredBase flooredStart

                    else
                        0 - Time.diff Month Time.utc flooredStart flooredBase
            in
            ( leftOffset, leftOffset + 1 )

        ( Nothing, Just end ) ->
            let
                flooredEnd =
                    Time.floor Month Time.utc end

                rightOffset =
                    if Time.posixToMillis flooredBase <= Time.posixToMillis flooredEnd then
                        Time.diff Month Time.utc flooredBase flooredEnd

                    else
                        0 - Time.diff Month Time.utc flooredEnd flooredBase
            in
            ( rightOffset - 1, rightOffset )

        ( Just start, Just end ) ->
            let
                flooredStart =
                    Time.floor Month Time.utc start

                flooredEnd =
                    Time.floor Month Time.utc end

                leftOffset =
                    if Time.posixToMillis flooredBase <= Time.posixToMillis flooredStart then
                        Time.diff Month Time.utc flooredBase flooredStart

                    else
                        0 - Time.diff Month Time.utc flooredStart flooredBase

                rightOffset =
                    let
                        startEndDiff =
                            Time.diff Month Time.utc flooredStart flooredEnd
                    in
                    case startEndDiff of
                        0 ->
                            leftOffset + 1

                        _ ->
                            leftOffset + startEndDiff
            in
            ( leftOffset, rightOffset )


{-| Retrieve the picked times, if any, from the provided date picker.
-}
getPickedTimes : DatePicker msg -> Maybe ( Posix, Posix )
getPickedTimes (DatePicker model) =
    Maybe.map2 (\start end -> ( start, end )) model.pickedStartDateTime model.pickedEndDateTime


{-| Open the provided date picker and receive the updated picker instance.
-}
open : DatePicker msg -> DatePicker msg
open (DatePicker model) =
    let
        ( leftPickerOffset, rightPickerOffset ) =
            calculatePickerOffsets (Time.floor Day Time.utc model.settings.baseTime) model.pickedStartDateTime model.pickedEndDateTime
    in
    DatePicker
        { model
            | status = Open model.settings.baseTime
            , stagedStartDay = Maybe.map (Time.floor Day Time.utc) model.pickedStartDateTime
            , stagedStartTime = model.pickedStartDateTime
            , stagedEndDay = Maybe.map (Time.floor Day Time.utc) model.pickedEndDateTime
            , stagedEndTime = model.pickedEndDateTime
            , leftPickerViewOffset = leftPickerOffset
            , rightPickerViewOffset = rightPickerOffset
        }


{-| Close the provided date picker and receive the updated picker instance.
-}
close : DatePicker msg -> DatePicker msg
close (DatePicker model) =
    DatePicker
        { model
            | status = Closed model.settings.baseTime
            , stagedStartDay = Nothing
            , stagedStartTime = Nothing
            , stagedEndDay = Nothing
            , stagedEndTime = Nothing
        }


{-| It is possible that the picker is instantiated on your model before data representing
previously picked datetimes is loaded. Use this function to update the picked datetimes in
the picker when the appropriate data is received. Use this method sparingly, if at all.
The picked state should ideally only change due to user input. Provide the datetimes like so:
( start, end ).
-}
setPickedTimes : ( Posix, Posix ) -> DatePicker msg -> DatePicker msg
setPickedTimes ( startDateTime, endDateTime ) (DatePicker model) =
    DatePicker { model | pickedStartDateTime = Just startDateTime, pickedEndDateTime = Just endDateTime }


{-| Indicates whether the DatePicker is open
-}
isOpen : DatePicker msg -> Bool
isOpen (DatePicker { status }) =
    case status of
        Open _ ->
            True

        Closed _ ->
            False


type Msg
    = BothViewsPrevMonth
    | BothViewsNextMonth
    | NextMonth LeftOrRight
    | PrevMonth LeftOrRight
    | NextYear LeftOrRight
    | PrevYear LeftOrRight
    | SetHoveredDay Posix
    | ClearHoveredDay
    | SetDay Posix
    | SetHour StartOrEnd Int
    | SetMinute StartOrEnd Int
    | ConfirmDateTimes ( Posix, Posix )
    | Close


update : Msg -> Model msg -> DatePicker msg
update msg model =
    case model.status of
        Open baseTime ->
            let
                -- this serves as the basis for time (not day) selection. In the event there is no previously picked DateTime, we use the baseTime
                -- passed into the picker on open to serve as a default. However, the time of day of the baseTime is not always guaranteed to be at 00:00,
                -- so here we ensure that the default time is 00:00 by flooring it.
                flooredBaseTime =
                    Time.floor Day Time.utc baseTime
            in
            case msg of
                BothViewsPrevMonth ->
                    DatePicker { model | leftPickerViewOffset = model.leftPickerViewOffset - 1, rightPickerViewOffset = model.rightPickerViewOffset - 1 }

                BothViewsNextMonth ->
                    DatePicker { model | leftPickerViewOffset = model.leftPickerViewOffset + 1, rightPickerViewOffset = model.rightPickerViewOffset + 1 }

                NextMonth leftOrRight ->
                    case leftOrRight of
                        Left ->
                            DatePicker { model | leftPickerViewOffset = model.leftPickerViewOffset + 1 }

                        Right ->
                            DatePicker { model | rightPickerViewOffset = model.rightPickerViewOffset + 1 }

                PrevMonth leftOrRight ->
                    case leftOrRight of
                        Left ->
                            DatePicker { model | leftPickerViewOffset = model.leftPickerViewOffset - 1 }

                        Right ->
                            DatePicker { model | rightPickerViewOffset = model.rightPickerViewOffset - 1 }

                NextYear leftOrRight ->
                    case leftOrRight of
                        Left ->
                            DatePicker { model | leftPickerViewOffset = model.leftPickerViewOffset + 12 }

                        Right ->
                            DatePicker { model | rightPickerViewOffset = model.rightPickerViewOffset + 12 }

                PrevYear leftOrRight ->
                    case leftOrRight of
                        Left ->
                            DatePicker { model | leftPickerViewOffset = model.leftPickerViewOffset - 12 }

                        Right ->
                            DatePicker { model | rightPickerViewOffset = model.rightPickerViewOffset - 12 }

                SetHoveredDay time ->
                    case ( model.stagedStartDay, model.stagedEndDay ) of
                        ( Nothing, Nothing ) ->
                            DatePicker model

                        ( Just _, Nothing ) ->
                            DatePicker { model | hovered = Just time }

                        ( Nothing, Just _ ) ->
                            DatePicker { model | hovered = Just time }

                        ( Just _, Just _ ) ->
                            DatePicker model

                ClearHoveredDay ->
                    DatePicker { model | hovered = Nothing }

                SetDay time ->
                    case ( model.stagedStartDay, model.stagedEndDay ) of
                        ( Nothing, Nothing ) ->
                            let
                                ( leftPickerOffset, rightPickerOffset ) =
                                    calculatePickerOffsets baseTime (Just time) Nothing
                            in
                            DatePicker { model | stagedStartDay = Just time, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                        ( Just start, Nothing ) ->
                            let
                                sParts =
                                    Time.posixToParts Time.utc start

                                tParts =
                                    Time.posixToParts Time.utc time

                                ( leftPickerOffset, rightPickerOffset ) =
                                    calculatePickerOffsets baseTime (Just start) (Just time)

                                ( switchedLeftPickerOffset, switchedRightPickerOffset ) =
                                    calculatePickerOffsets baseTime (Just time) (Just start)
                            in
                            if Time.posixToMillis time < Time.posixToMillis start then
                                let
                                    ( newEnd, newStart ) =
                                        Utilities.switchTimes sParts tParts
                                in
                                DatePicker { model | stagedStartDay = Just newStart, stagedEndDay = Just newEnd, leftPickerViewOffset = switchedLeftPickerOffset, rightPickerViewOffset = switchedRightPickerOffset }

                            else
                                DatePicker { model | stagedStartDay = Just start, stagedEndDay = Just time, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                        ( Nothing, Just end ) ->
                            let
                                eParts =
                                    Time.posixToParts Time.utc end

                                tParts =
                                    Time.posixToParts Time.utc time

                                ( leftPickerOffset, rightPickerOffset ) =
                                    calculatePickerOffsets baseTime (Just time) (Just end)

                                ( switchedLeftPickerOffset, switchedRightPickerOffset ) =
                                    calculatePickerOffsets baseTime (Just end) (Just time)
                            in
                            if Time.posixToMillis time < Time.posixToMillis end then
                                DatePicker { model | stagedStartDay = Just time, stagedEndDay = Just end, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                            else
                                let
                                    ( newStart, newEnd ) =
                                        Utilities.switchTimes eParts tParts
                                in
                                DatePicker { model | stagedStartDay = Just newStart, stagedEndDay = Just newEnd, leftPickerViewOffset = switchedLeftPickerOffset, rightPickerViewOffset = switchedRightPickerOffset }

                        ( Just start, Just end ) ->
                            let
                                sParts =
                                    Time.posixToParts Time.utc start

                                eParts =
                                    Time.posixToParts Time.utc end

                                tParts =
                                    Time.posixToParts Time.utc time

                                isDayStart =
                                    Utilities.doDaysMatch sParts tParts

                                isDayEnd =
                                    Utilities.doDaysMatch eParts tParts
                            in
                            if isDayStart then
                                DatePicker { model | stagedStartDay = Nothing }

                            else if isDayEnd then
                                DatePicker { model | stagedEndDay = Nothing }

                            else
                                let
                                    ( leftPickerOffset, rightPickerOffset ) =
                                        calculatePickerOffsets baseTime (Just time) Nothing
                                in
                                DatePicker { model | stagedStartDay = Just time, stagedEndDay = Nothing, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                SetHour startOrEnd hour ->
                    case startOrEnd of
                        Start ->
                            let
                                newTime =
                                    Utilities.setHourNotDay hour (Maybe.withDefault flooredBaseTime model.stagedStartTime)
                            in
                            DatePicker { model | stagedStartTime = Just newTime }

                        End ->
                            let
                                newTime =
                                    Utilities.setHourNotDay hour (Maybe.withDefault flooredBaseTime model.stagedEndTime)
                            in
                            DatePicker { model | stagedEndTime = Just newTime }

                SetMinute startOrEnd minute ->
                    case startOrEnd of
                        Start ->
                            let
                                newTime =
                                    Utilities.setMinuteNotDay minute (Maybe.withDefault flooredBaseTime model.stagedStartTime)
                            in
                            DatePicker { model | stagedStartTime = Just newTime }

                        End ->
                            let
                                newTime =
                                    Utilities.setMinuteNotDay minute (Maybe.withDefault flooredBaseTime model.stagedEndTime)
                            in
                            DatePicker { model | stagedEndTime = Just newTime }

                ConfirmDateTimes ( start, end ) ->
                    DatePicker { model | pickedStartDateTime = Just start, pickedEndDateTime = Just end } |> close

                Close ->
                    close <| DatePicker model

        Closed _ ->
            DatePicker model


classPrefix : String
classPrefix =
    "elm-datetimepicker-duration--"


{-| The date picker view. Simply pass it the date picker instance you wish to view.
-}
view : DatePicker msg -> Html msg
view (DatePicker model) =
    case model.status of
        Open baseTime ->
            let
                leftViewTime =
                    Time.add Month model.leftPickerViewOffset Time.utc baseTime

                rightViewTime =
                    Time.add Month model.rightPickerViewOffset Time.utc baseTime
            in
            div
                [ id datePickerId, class (classPrefix ++ "picker-container") ]
                [ viewPickerHeader model
                , div
                    [ class (classPrefix ++ "calendars-container") ]
                    [ div
                        [ class (classPrefix ++ "calendar") ]
                        [ viewCalendar model Left ( leftViewTime, model.leftPickerViewOffset, model.rightPickerViewOffset ) ]
                    , div [ class (classPrefix ++ "calendar-spacer") ] []
                    , div
                        [ class (classPrefix ++ "calendar") ]
                        [ viewCalendar model Right ( rightViewTime, model.leftPickerViewOffset, model.rightPickerViewOffset ) ]
                    ]
                , div [ class (classPrefix ++ "footer-container") ] [ viewFooter model ]
                ]

        Closed _ ->
            text ""


viewPickerHeader : Model msg -> Html msg
viewPickerHeader model =
    div [ class (classPrefix ++ "picker-header") ]
        [ div
            [ class (classPrefix ++ "picker-header-chevron")
            , onClick <| model.settings.internalMsg <| update BothViewsPrevMonth model
            ]
            [ Icons.chevronLeft
                |> Icons.withSize 15
                |> Icons.toHtml []
            ]
        , div
            [ class (classPrefix ++ "picker-header-chevron")
            , onClick <| model.settings.internalMsg <| update BothViewsNextMonth model
            ]
            [ Icons.chevronRight
                |> Icons.withSize 15
                |> Icons.toHtml []
            ]
        ]


viewCalendar : Model msg -> LeftOrRight -> ( Posix, Int, Int ) -> Html msg
viewCalendar model leftOrRight ( viewTime, leftPointer, rightPointer ) =
    div
        []
        [ viewCalendarHeader model leftOrRight ( viewTime, leftPointer, rightPointer )
        , viewMonth model viewTime
        ]


viewCalendarHeader : Model msg -> LeftOrRight -> ( Posix, Int, Int ) -> Html msg
viewCalendarHeader model leftOrRight ( viewTime, leftPointer, rightPointer ) =
    let
        monthName =
            Time.toMonth Time.utc viewTime |> model.settings.formattedMonth

        year =
            Time.toYear Time.utc viewTime |> String.fromInt

        ( ( prevMonthChevCls, prevYearChevCls ), ( nextMonthChevCls, nextYearChevCls ) ) =
            let
                baseChevronClass =
                    classPrefix ++ "calendar-header-chevron"
            in
            case leftOrRight of
                Left ->
                    let
                        nextMonthCls =
                            if leftPointer + 1 >= rightPointer then
                                classPrefix ++ "calendar-header-chevron " ++ classPrefix ++ "hidden"

                            else
                                baseChevronClass

                        nextYearCls =
                            if leftPointer + 12 >= rightPointer then
                                classPrefix ++ "calendar-header-chevron " ++ classPrefix ++ "hidden"

                            else
                                baseChevronClass
                    in
                    ( ( baseChevronClass, baseChevronClass ), ( nextMonthCls, nextYearCls ) )

                Right ->
                    let
                        prevMonthCls =
                            if rightPointer - 1 <= leftPointer then
                                classPrefix ++ "calendar-header-chevron " ++ classPrefix ++ "hidden"

                            else
                                baseChevronClass

                        prevYearCls =
                            if rightPointer - 12 <= leftPointer then
                                classPrefix ++ "calendar-header-chevron " ++ classPrefix ++ "hidden"

                            else
                                baseChevronClass
                    in
                    ( ( prevMonthCls, prevYearCls ), ( baseChevronClass, baseChevronClass ) )
    in
    div
        [ class (classPrefix ++ "calendar-header") ]
        [ div [ class (classPrefix ++ "calendar-header-row") ]
            [ div
                [ class prevMonthChevCls, onClick <| model.settings.internalMsg <| update (PrevMonth leftOrRight) model ]
                [ Icons.chevronLeft
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            , div
                [ class (classPrefix ++ "calendar-header-text") ]
                [ div [] [ text monthName ] ]
            , div
                [ class nextMonthChevCls, onClick <| model.settings.internalMsg <| update (NextMonth leftOrRight) model ]
                [ Icons.chevronRight
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            ]
        , div [ class (classPrefix ++ "calendar-header-row") ]
            [ div
                [ class prevYearChevCls, onClick <| model.settings.internalMsg <| update (PrevYear leftOrRight) model ]
                [ Icons.chevronsLeft
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            , div
                [ class (classPrefix ++ "calendar-header-text") ]
                [ div [] [ text year ] ]
            , div
                [ class nextYearChevCls, onClick <| model.settings.internalMsg <| update (NextYear leftOrRight) model ]
                [ Icons.chevronsRight
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            ]
        , viewWeekHeader model.settings [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]
        ]


viewWeekHeader : Settings msg -> List Weekday -> Html msg
viewWeekHeader settings days =
    div
        [ class (classPrefix ++ "calendar-header-week") ]
        (List.map (viewHeaderDay settings.formattedDay) days)


viewHeaderDay : (Weekday -> String) -> Weekday -> Html msg
viewHeaderDay formatDay day =
    div
        [ class (classPrefix ++ "calendar-header-day") ]
        [ text (formatDay day) ]


viewMonth : Model msg -> Posix -> Html msg
viewMonth model viewTime =
    let
        monthRenderData =
            Utilities.monthData viewTime

        currentMonth =
            Time.posixToParts Time.utc viewTime |> .month

        weeks =
            List.reverse (Utilities.splitIntoWeeks monthRenderData [])
    in
    div
        [ class (classPrefix ++ "calendar-month"), onMouseOut <| model.settings.internalMsg (update ClearHoveredDay model) ]
        [ div [] (List.map (viewWeek model currentMonth) weeks)
        ]


viewWeek : Model msg -> Month -> List Parts -> Html msg
viewWeek model currentMonth week =
    div [ class (classPrefix ++ "calendar-week") ]
        (List.map (viewDay model currentMonth) week)


viewDay : Model msg -> Month -> Parts -> Html msg
viewDay model currentMonth day =
    let
        isToday =
            Maybe.map (\tday -> Utilities.doDaysMatch day (Time.posixToParts Time.utc tday)) model.settings.today
                |> Maybe.withDefault False

        ( isPicked, isBetween ) =
            Utilities.durationDayPickedOrBetween day model.hovered ( model.stagedStartDay, model.stagedEndDay )

        isDisabled =
            model.settings.dayDisabled (Time.partsToPosix Time.utc day)

        dayClasses =
            DatePicker.Styles.durationDayClasses classPrefix (day.month /= currentMonth) isDisabled isPicked isToday isBetween

        baseAttrs =
            [ class dayClasses
            , onMouseOver <| model.settings.internalMsg (update (SetHoveredDay (Time.partsToPosix Time.utc day)) model)
            ]

        attrs =
            if isDisabled then
                baseAttrs

            else
                baseAttrs ++ [ onClick <| model.settings.internalMsg (update (SetDay (Time.partsToPosix Time.utc day)) model) ]
    in
    div
        attrs
        [ text (String.fromInt day.day) ]


viewFooter : Model msg -> Html msg
viewFooter model =
    div
        [ class (classPrefix ++ "footer") ]
        [ div [ class (classPrefix ++ "time-pickers-container") ]
            [ div []
                [ div
                    [ class (classPrefix ++ "time-picker-container") ]
                    [ text "Start", viewTimePicker model Start ]
                ]
            , div []
                [ div
                    [ class (classPrefix ++ "time-picker-container") ]
                    [ text "End", viewTimePicker model End ]
                ]
            ]
        , div [ class (classPrefix ++ "confirm-button-container") ] [ viewConfirmButton model ]
        ]


startTimeBeforeEndTime : Posix -> Posix -> Bool
startTimeBeforeEndTime startTime endTime =
    let
        startParts =
            Time.posixToParts Time.utc startTime

        endParts =
            Time.posixToParts Time.utc endTime
    in
    if startParts.hour == endParts.hour then
        startParts.minute < endParts.minute

    else
        startParts.hour < endParts.hour


maybeValidTimes : Posix -> Maybe Posix -> Maybe Posix -> Maybe ( Posix, Posix )
maybeValidTimes baseTime pickedStartTime pickedEndTime =
    -- We don't use Maybe.map2 here because only the end time needs to be selected
    -- the start time can default to 00:00 without being explicitly selected
    Maybe.map
        (\endTime ->
            let
                startTime =
                    Maybe.withDefault (Time.floor Day Time.utc baseTime) pickedStartTime
            in
            ( startTime, endTime )
        )
        pickedEndTime


uniteDayAndTime : Posix -> Posix -> Posix
uniteDayAndTime dayPosix timePosix =
    let
        dayParts =
            Time.posixToParts Time.utc dayPosix

        timeParts =
            Time.posixToParts Time.utc timePosix
    in
    Time.partsToPosix Time.utc { dayParts | hour = timeParts.hour, minute = timeParts.minute }


unifyTimesWithDays : Maybe ( Posix, Posix ) -> ( Posix, Posix ) -> Maybe ( Posix, Posix )
unifyTimesWithDays startEndDayTuple ( startTime, endTime ) =
    if startTimeBeforeEndTime startTime endTime then
        case startEndDayTuple of
            Just ( startDay, endDay ) ->
                Just ( uniteDayAndTime startDay startTime, uniteDayAndTime endDay endTime )

            Nothing ->
                Just ( startTime, endTime )

    else
        Nothing


produceClassStringActionTuple : Model msg -> Maybe ( Posix, Posix ) -> ( String, List (Html.Attribute msg) )
produceClassStringActionTuple model startEndDateTimeTuple =
    case startEndDateTimeTuple of
        Just ( startDateTime, endDateTime ) ->
            ( classPrefix ++ "confirm-button", [ onClick <| model.settings.internalMsg (update (ConfirmDateTimes ( startDateTime, endDateTime )) model) ] )

        Nothing ->
            ( classPrefix ++ "confirm-button " ++ classPrefix ++ "disabled", [] )


viewConfirmButton : Model msg -> Html msg
viewConfirmButton model =
    let
        ( classStr, confirmAction ) =
            case ( model.stagedStartDay, model.stagedEndDay ) of
                ( Nothing, Nothing ) ->
                    maybeValidTimes model.settings.baseTime model.stagedStartTime model.stagedEndTime
                        |> Maybe.andThen (unifyTimesWithDays Nothing)
                        |> produceClassStringActionTuple model

                ( Just startDay, Nothing ) ->
                    maybeValidTimes model.settings.baseTime model.stagedStartTime model.stagedEndTime
                        |> Maybe.andThen (unifyTimesWithDays <| Just ( startDay, startDay ))
                        |> produceClassStringActionTuple model

                ( Nothing, Just endDay ) ->
                    maybeValidTimes model.settings.baseTime model.stagedStartTime model.stagedEndTime
                        |> Maybe.andThen (unifyTimesWithDays <| Just ( endDay, endDay ))
                        |> produceClassStringActionTuple model

                ( Just startDay, Just endDay ) ->
                    if Utilities.doDaysMatch (Time.posixToParts Time.utc startDay) (Time.posixToParts Time.utc endDay) then
                        maybeValidTimes model.settings.baseTime model.stagedStartTime model.stagedEndTime
                            |> Maybe.andThen (unifyTimesWithDays <| Just ( startDay, endDay ))
                            |> produceClassStringActionTuple model

                    else
                        let
                            startTime =
                                Maybe.withDefault (Time.floor Day Time.utc model.settings.baseTime) model.stagedStartTime

                            endTime =
                                Maybe.withDefault (Time.floor Day Time.utc model.settings.baseTime) model.stagedEndTime

                            unifiedStart =
                                uniteDayAndTime startDay startTime

                            unifiedEnd =
                                uniteDayAndTime endDay endTime
                        in
                        ( classPrefix ++ "confirm-button", [ onClick <| model.settings.internalMsg (update (ConfirmDateTimes ( unifiedStart, unifiedEnd )) model) ] )

        confirmAttrs =
            [ class classStr, type_ "button", attribute "aria-label" "confirm" ] ++ confirmAction
    in
    button confirmAttrs
        [ Icons.check
            |> Icons.withSize 16
            |> Icons.toHtml []
        ]


viewTimePicker : Model msg -> StartOrEnd -> Html msg
viewTimePicker model startOrEnd =
    let
        stagedTime =
            case startOrEnd of
                Start ->
                    model.stagedStartTime

                End ->
                    model.stagedEndTime

        ( hour, minute ) =
            Maybe.map
                (\time ->
                    let
                        timeParts =
                            Time.posixToParts Time.utc time
                    in
                    ( timeParts.hour, timeParts.minute )
                )
                stagedTime
                |> Maybe.withDefault ( 0, 0 )
    in
    div
        [ class (classPrefix ++ "time-picker") ]
        [ div [ class (classPrefix ++ "select-container") ]
            -- Eventually we would like to use onInput instead of a custom on "change".
            --
            -- select [ onInput <| \val -> settings.internalMsg (update (SetHour startOrEnd val) model) ] (generateHourOptions hour)
            --
            -- It will be easier to reason through. However, at the moment, a few browsers are not compatible
            -- with that behaviour. See: https://caniuse.com/#search=oninput
            [ div [ class (classPrefix ++ "select") ] [ select [ on "change" (Decode.map model.settings.internalMsg (Decode.map (\msg -> update msg model) (Decode.map (SetHour startOrEnd) targetValueIntParse))) ] (Utilities.generateHourOptions hour) ]
            , div [ class (classPrefix ++ "select-spacer") ] [ text ":" ]
            , div [ class (classPrefix ++ "select") ] [ select [ on "change" (Decode.map model.settings.internalMsg (Decode.map (\msg -> update msg model) (Decode.map (SetMinute startOrEnd) targetValueIntParse))) ] (Utilities.generateMinuteOptions minute) ]
            ]
        ]
