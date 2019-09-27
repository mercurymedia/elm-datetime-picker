module DurationDatePicker exposing
    ( DatePicker, init, view, subscriptions
    , Settings, defaultSettings
    , openPicker, closePicker
    )

{-| A date picker component for picking a datetime range.


# Architecture

@docs DatePicker, init, view, subscriptions


# Settings

@docs Settings, defaultSettings


# Externally Triggered Actions

@docs openPicker, closePicker

-}

import Browser.Events
import DatePicker.Icons as Icons
import DatePicker.Styles
import DatePicker.Utilities as Utilities
import Html exposing (Html, button, div, select, text)
import Html.Attributes exposing (attribute, class, id)
import Html.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Decode
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..))
import Time.Extra as Time exposing (Interval(..), Parts)


type alias Model =
    { status : Status
    , hovered : Maybe Posix
    , leftPickerViewOffset : Int
    , rightPickerViewOffset : Int
    , pickedStartDay : Maybe Posix
    , pickedStartTime : Maybe Posix
    , pickedEndDay : Maybe Posix
    , pickedEndTime : Maybe Posix
    }


{-| The opaque type representing a particular date picker instance.
-}
type DatePicker
    = DatePicker Model


{-| The type facilitating the configuration of the datepicker settings.
-}
type alias Settings msg =
    -- this will get more advanced as we develop the api.
    -- Potential additions:
    -- * hide time selection (default to midnight)
    -- * hide prev and next year chevrons
    { formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , today : Maybe Posix
    , dayDisabled : Posix -> Bool
    , internalMsg : DatePicker -> msg
    , selectedMsg : Posix -> Posix -> msg
    }


type Status
    = Closed
    | Open Posix


type LeftOrRight
    = Left
    | Right


type StartOrEnd
    = Start
    | End


type alias MsgConfig msg =
    { internalMsg : DatePicker -> msg
    , externalMsg : Posix -> Posix -> msg
    }


{-| A record of default settings for the date picker. Extend this if
you want to further customize the date picker.

Requires a MsgConfig msg record to set the date picker's internal and external
msg indicators

    { internalMsg : DatePicker -> msg
    , externalMsg : Posix -> msg
    }

-}
defaultSettings : MsgConfig msg -> Settings msg
defaultSettings { internalMsg, externalMsg } =
    { formattedDay = Utilities.dayToNameString
    , formattedMonth = Utilities.monthToNameString
    , today = Nothing
    , dayDisabled = always False
    , internalMsg = internalMsg
    , selectedMsg = externalMsg
    }


{-| Instantiates and returns a date picker.
-}
init : DatePicker
init =
    DatePicker
        { status = Closed
        , hovered = Nothing
        , leftPickerViewOffset = 0
        , rightPickerViewOffset = 0
        , pickedStartDay = Nothing
        , pickedStartTime = Nothing
        , pickedEndDay = Nothing
        , pickedEndTime = Nothing
        }


datePickerId : String
datePickerId =
    "date-picker-component"


{-| Events external to the picker to which it is subscribed.
-}
subscriptions : (DatePicker -> msg) -> DatePicker -> Sub msg
subscriptions internalMsg (DatePicker model) =
    case model.status of
        Open _ ->
            Browser.Events.onMouseDown (clickedOutsidePicker datePickerId internalMsg (DatePicker model))

        Closed ->
            Sub.none


clickedOutsidePicker : String -> (DatePicker -> msg) -> DatePicker -> Decode.Decoder msg
clickedOutsidePicker componentId internalMsg datePicker =
    Decode.field "target" (Utilities.eventIsOutsideComponent componentId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| internalMsg (update Close datePicker)

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


{-| Open the provided date picker and receive the updated picker instance. Also
takes a default time the picker should center on (in the event a time has not yet
been picked) as well as the picked start and end times. A common example of a default time
would be the datetime for the current day.
-}
openPicker : Posix -> Maybe Posix -> Maybe Posix -> DatePicker -> DatePicker
openPicker baseTime startTime endTime (DatePicker model) =
    let
        ( leftPickerOffset, rightPickerOffset ) =
            calculatePickerOffsets baseTime startTime endTime
    in
    DatePicker
        { model
            | status = Open baseTime
            , pickedStartDay = Maybe.map (Time.floor Day Time.utc) startTime
            , pickedStartTime = startTime
            , pickedEndDay = Maybe.map (Time.floor Day Time.utc) endTime
            , pickedEndTime = endTime
            , leftPickerViewOffset = leftPickerOffset
            , rightPickerViewOffset = rightPickerOffset
        }


{-| Close the provided date picker and receive the updated picker instance.
-}
closePicker : DatePicker -> DatePicker
closePicker (DatePicker model) =
    DatePicker { model | status = Closed }


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
    | Close


update : Msg -> DatePicker -> DatePicker
update msg (DatePicker model) =
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
                    case ( model.pickedStartDay, model.pickedEndDay ) of
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
                    case ( model.pickedStartDay, model.pickedEndDay ) of
                        ( Nothing, Nothing ) ->
                            let
                                ( leftPickerOffset, rightPickerOffset ) =
                                    calculatePickerOffsets baseTime (Just time) Nothing
                            in
                            DatePicker { model | pickedStartDay = Just time, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

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
                                DatePicker { model | pickedStartDay = Just newStart, pickedEndDay = Just newEnd, leftPickerViewOffset = switchedLeftPickerOffset, rightPickerViewOffset = switchedRightPickerOffset }

                            else
                                DatePicker { model | pickedStartDay = Just start, pickedEndDay = Just time, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

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
                                DatePicker { model | pickedStartDay = Just time, pickedEndDay = Just end, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                            else
                                let
                                    ( newStart, newEnd ) =
                                        Utilities.switchTimes eParts tParts
                                in
                                DatePicker { model | pickedStartDay = Just newStart, pickedEndDay = Just newEnd, leftPickerViewOffset = switchedLeftPickerOffset, rightPickerViewOffset = switchedRightPickerOffset }

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
                                DatePicker { model | pickedStartDay = Nothing }

                            else if isDayEnd then
                                DatePicker { model | pickedEndDay = Nothing }

                            else
                                let
                                    ( leftPickerOffset, rightPickerOffset ) =
                                        calculatePickerOffsets baseTime (Just time) Nothing
                                in
                                DatePicker { model | pickedStartDay = Just time, pickedEndDay = Nothing, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                SetHour startOrEnd hour ->
                    case startOrEnd of
                        Start ->
                            let
                                newTime =
                                    Utilities.setHourNotDay hour (Maybe.withDefault flooredBaseTime model.pickedStartTime)
                            in
                            DatePicker { model | pickedStartTime = Just newTime }

                        End ->
                            let
                                newTime =
                                    Utilities.setHourNotDay hour (Maybe.withDefault flooredBaseTime model.pickedEndTime)
                            in
                            DatePicker { model | pickedEndTime = Just newTime }

                SetMinute startOrEnd minute ->
                    case startOrEnd of
                        Start ->
                            let
                                newTime =
                                    Utilities.setMinuteNotDay minute (Maybe.withDefault flooredBaseTime model.pickedStartTime)
                            in
                            DatePicker { model | pickedStartTime = Just newTime }

                        End ->
                            let
                                newTime =
                                    Utilities.setMinuteNotDay minute (Maybe.withDefault flooredBaseTime model.pickedEndTime)
                            in
                            DatePicker { model | pickedEndTime = Just newTime }

                Close ->
                    DatePicker { model | status = Closed }

        Closed ->
            DatePicker model


classPrefix : String
classPrefix =
    "elm-datetimepicker-duration--"


{-| The date picker view. Simply pass it the configured settings
and the date picker instance you wish to view.
-}
view : Settings msg -> DatePicker -> Html msg
view settings (DatePicker model) =
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
                [ viewPickerHeader settings model
                , div
                    [ class (classPrefix ++ "calendars-container") ]
                    [ div
                        [ class (classPrefix ++ "calendar") ]
                        [ viewCalendar settings model Left ( leftViewTime, model.leftPickerViewOffset, model.rightPickerViewOffset ) ]
                    , div [ class (classPrefix ++ "calendar-spacer") ] []
                    , div
                        [ class (classPrefix ++ "calendar") ]
                        [ viewCalendar settings model Right ( rightViewTime, model.leftPickerViewOffset, model.rightPickerViewOffset ) ]
                    ]
                , div [ class (classPrefix ++ "footer-container") ] [ viewFooter settings baseTime model ]
                ]

        Closed ->
            text ""


viewPickerHeader : Settings msg -> Model -> Html msg
viewPickerHeader settings model =
    div [ class (classPrefix ++ "picker-header") ]
        [ div
            [ class (classPrefix ++ "picker-header-chevron")
            , onClick <| settings.internalMsg <| update BothViewsPrevMonth (DatePicker model)
            ]
            [ Icons.chevronLeft
                |> Icons.withSize 15
                |> Icons.toHtml []
            ]
        , div
            [ class (classPrefix ++ "picker-header-chevron")
            , onClick <| settings.internalMsg <| update BothViewsNextMonth (DatePicker model)
            ]
            [ Icons.chevronRight
                |> Icons.withSize 15
                |> Icons.toHtml []
            ]
        ]


viewCalendar : Settings msg -> Model -> LeftOrRight -> ( Posix, Int, Int ) -> Html msg
viewCalendar settings model leftOrRight ( viewTime, leftPointer, rightPointer ) =
    div
        []
        [ viewCalendarHeader settings model leftOrRight viewTime leftPointer rightPointer
        , viewMonth settings model viewTime
        ]


viewCalendarHeader : Settings msg -> Model -> LeftOrRight -> Posix -> Int -> Int -> Html msg
viewCalendarHeader settings model leftOrRight viewTime leftPointer rightPointer =
    let
        monthName =
            Time.toMonth Time.utc viewTime |> settings.formattedMonth

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
                [ class prevMonthChevCls, onClick <| settings.internalMsg <| update (PrevMonth leftOrRight) (DatePicker model) ]
                [ Icons.chevronLeft
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            , div
                [ class (classPrefix ++ "calendar-header-text") ]
                [ div [] [ text monthName ] ]
            , div
                [ class nextMonthChevCls, onClick <| settings.internalMsg <| update (NextMonth leftOrRight) (DatePicker model) ]
                [ Icons.chevronRight
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            ]
        , div [ class (classPrefix ++ "calendar-header-row") ]
            [ div
                [ class prevYearChevCls, onClick <| settings.internalMsg <| update (PrevYear leftOrRight) (DatePicker model) ]
                [ Icons.chevronsLeft
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            , div
                [ class (classPrefix ++ "calendar-header-text") ]
                [ div [] [ text year ] ]
            , div
                [ class nextYearChevCls, onClick <| settings.internalMsg <| update (NextYear leftOrRight) (DatePicker model) ]
                [ Icons.chevronsRight
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            ]
        , viewWeekHeader settings [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]
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


viewMonth : Settings msg -> Model -> Posix -> Html msg
viewMonth settings model viewTime =
    let
        monthRenderData =
            Utilities.monthData viewTime

        currentMonth =
            Time.posixToParts Time.utc viewTime |> .month

        weeks =
            List.reverse (Utilities.splitIntoWeeks monthRenderData [])
    in
    div
        [ class (classPrefix ++ "calendar-month"), onMouseOut <| settings.internalMsg (update ClearHoveredDay (DatePicker model)) ]
        [ div [] (List.map (viewWeek settings model currentMonth) weeks)
        ]


viewWeek : Settings msg -> Model -> Month -> List Parts -> Html msg
viewWeek settings model currentMonth week =
    div [ class (classPrefix ++ "calendar-week") ]
        (List.map (viewDay settings model currentMonth) week)


viewDay : Settings msg -> Model -> Month -> Parts -> Html msg
viewDay settings model currentMonth day =
    let
        isToday =
            Maybe.map (\tday -> Utilities.doDaysMatch day (Time.posixToParts Time.utc tday)) settings.today
                |> Maybe.withDefault False

        ( isPicked, isBetween ) =
            Utilities.durationDayPickedOrBetween day model.hovered ( model.pickedStartDay, model.pickedEndDay )

        isDisabled =
            settings.dayDisabled (Time.partsToPosix Time.utc day)

        dayClasses =
            DatePicker.Styles.durationDayClasses classPrefix (day.month /= currentMonth) isDisabled isPicked isToday isBetween

        baseAttrs =
            [ class dayClasses
            , onMouseOver <| settings.internalMsg (update (SetHoveredDay (Time.partsToPosix Time.utc day)) (DatePicker model))
            ]

        attrs =
            if isDisabled then
                baseAttrs

            else
                baseAttrs ++ [ onClick <| settings.internalMsg (update (SetDay (Time.partsToPosix Time.utc day)) (DatePicker model)) ]
    in
    div
        attrs
        [ text (String.fromInt day.day) ]


viewFooter : Settings msg -> Posix -> Model -> Html msg
viewFooter settings baseTime model =
    div
        [ class (classPrefix ++ "footer") ]
        [ div [ class (classPrefix ++ "time-pickers-container") ]
            [ div []
                [ div
                    [ class (classPrefix ++ "time-picker-container") ]
                    [ text "Start", viewTimePicker settings model Start model.pickedStartTime ]
                ]
            , div []
                [ div
                    [ class (classPrefix ++ "time-picker-container") ]
                    [ text "End", viewTimePicker settings model End model.pickedEndTime ]
                ]
            ]
        , div [ class (classPrefix ++ "confirm-button-container") ] [ viewConfirmButton settings baseTime model ]
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


produceClassStringActionTuple : Settings msg -> Maybe ( Posix, Posix ) -> ( String, List (Html.Attribute msg) )
produceClassStringActionTuple settings startEndDateTimeTuple =
    case startEndDateTimeTuple of
        Just ( startDateTime, endDateTime ) ->
            ( classPrefix ++ "confirm-button", [ onClick <| settings.selectedMsg startDateTime endDateTime ] )

        Nothing ->
            ( classPrefix ++ "confirm-button " ++ classPrefix ++ "disabled", [] )


viewConfirmButton : Settings msg -> Posix -> Model -> Html msg
viewConfirmButton settings baseTime model =
    let
        ( classStr, confirmAction ) =
            case ( model.pickedStartDay, model.pickedEndDay ) of
                ( Nothing, Nothing ) ->
                    maybeValidTimes baseTime model.pickedStartTime model.pickedEndTime
                        |> Maybe.andThen (unifyTimesWithDays Nothing)
                        |> produceClassStringActionTuple settings

                ( Just startDay, Nothing ) ->
                    maybeValidTimes baseTime model.pickedStartTime model.pickedEndTime
                        |> Maybe.andThen (unifyTimesWithDays <| Just ( startDay, startDay ))
                        |> produceClassStringActionTuple settings

                ( Nothing, Just endDay ) ->
                    maybeValidTimes baseTime model.pickedStartTime model.pickedEndTime
                        |> Maybe.andThen (unifyTimesWithDays <| Just ( endDay, endDay ))
                        |> produceClassStringActionTuple settings

                ( Just startDay, Just endDay ) ->
                    if Utilities.doDaysMatch (Time.posixToParts Time.utc startDay) (Time.posixToParts Time.utc endDay) then
                        maybeValidTimes baseTime model.pickedStartTime model.pickedEndTime
                            |> Maybe.andThen (unifyTimesWithDays <| Just ( startDay, endDay ))
                            |> produceClassStringActionTuple settings

                    else
                        let
                            startTime =
                                Maybe.withDefault (Time.floor Day Time.utc baseTime) model.pickedStartTime

                            endTime =
                                Maybe.withDefault (Time.floor Day Time.utc baseTime) model.pickedEndTime

                            unifiedStart =
                                uniteDayAndTime startDay startTime

                            unifiedEnd =
                                uniteDayAndTime endDay endTime
                        in
                        ( classPrefix ++ "confirm-button", [ onClick <| settings.selectedMsg unifiedStart unifiedEnd ] )

        confirmAttrs =
            [ class classStr, attribute "aria-label" "confirm" ] ++ confirmAction
    in
    button confirmAttrs
        [ Icons.check
            |> Icons.withSize 16
            |> Icons.toHtml []
        ]


viewTimePicker : Settings msg -> Model -> StartOrEnd -> Maybe Posix -> Html msg
viewTimePicker settings model startOrEnd pickedTime =
    let
        ( hour, minute ) =
            Maybe.map
                (\time ->
                    let
                        timeParts =
                            Time.posixToParts Time.utc time
                    in
                    ( timeParts.hour, timeParts.minute )
                )
                pickedTime
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
            [ div [ class (classPrefix ++ "select") ] [ select [ on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update msg (DatePicker model)) (Decode.map (SetHour startOrEnd) targetValueIntParse))) ] (Utilities.generateHourOptions hour) ]
            , div [ class (classPrefix ++ "select-spacer") ] [ text ":" ]
            , div [ class (classPrefix ++ "select") ] [ select [ on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update msg (DatePicker model)) (Decode.map (SetMinute startOrEnd) targetValueIntParse))) ] (Utilities.generateMinuteOptions minute) ]
            ]
        ]
