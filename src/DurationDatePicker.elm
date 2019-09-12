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
import DatePicker.Utilities exposing (addLeadingZero, dayToNameString, doDaysMatch, durationDayPickedOrBetween, monthData, monthToNameString, splitIntoWeeks, switchTimes)
import Html exposing (Html, button, div, option, select, text)
import Html.Attributes exposing (class, id, selected, value)
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
    , pickedStart : Maybe Posix
    , pickedEnd : Maybe Posix
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


type HourOrMinute
    = Hour Int
    | Minute Int


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
    { formattedDay = dayToNameString
    , formattedMonth = monthToNameString
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
        , pickedStart = Nothing
        , pickedEnd = Nothing
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
            Browser.Events.onMouseDown (outsideComponent datePickerId internalMsg (DatePicker model))

        Closed ->
            Sub.none


calculatePickerOffsets : Posix -> Maybe Posix -> Maybe Posix -> ( Int, Int )
calculatePickerOffsets baseTime startTime endTime =
    let
        flooredBase =
            Time.floor Month Time.utc baseTime
    in
    case ( startTime, endTime ) of
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
            , pickedStart = startTime
            , pickedEnd = endTime
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
    | SelectDateTime Posix
    | SetHour StartOrEnd Int
    | SetMinute StartOrEnd Int
    | Close


update : Msg -> DatePicker -> DatePicker
update msg (DatePicker model) =
    case model.status of
        Open baseTime ->
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
                    case ( model.pickedStart, model.pickedEnd ) of
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

                SelectDateTime time ->
                    case ( model.pickedStart, model.pickedEnd ) of
                        ( Nothing, Nothing ) ->
                            let
                                ( leftPickerOffset, rightPickerOffset ) =
                                    calculatePickerOffsets baseTime (Just time) Nothing
                            in
                            DatePicker { model | pickedStart = Just time, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                        ( Just start, Nothing ) ->
                            let
                                sParts =
                                    Time.posixToParts Time.utc start

                                tParts =
                                    Time.posixToParts Time.utc time

                                areSameDay =
                                    doDaysMatch sParts tParts

                                ( leftPickerOffset, rightPickerOffset ) =
                                    calculatePickerOffsets baseTime (Just start) (Just time)

                                ( switchedLeftPickerOffset, switchedRightPickerOffset ) =
                                    calculatePickerOffsets baseTime (Just time) (Just start)
                            in
                            if Time.posixToMillis time < Time.posixToMillis start then
                                let
                                    ( newStart, newTime ) =
                                        switchTimes sParts tParts
                                in
                                DatePicker { model | pickedStart = Just newTime, pickedEnd = Just newStart, leftPickerViewOffset = switchedLeftPickerOffset, rightPickerViewOffset = switchedRightPickerOffset }

                            else if areSameDay then
                                let
                                    newStart =
                                        { sParts | hour = 0, minute = 0 } |> Time.partsToPosix Time.utc

                                    newTime =
                                        { tParts | hour = 23, minute = 59 } |> Time.partsToPosix Time.utc
                                in
                                DatePicker { model | pickedStart = Just newStart, pickedEnd = Just newTime, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                            else
                                DatePicker { model | pickedStart = Just start, pickedEnd = Just time, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                        ( Nothing, Just end ) ->
                            let
                                eParts =
                                    Time.posixToParts Time.utc end

                                tParts =
                                    Time.posixToParts Time.utc time

                                areSameDay =
                                    doDaysMatch eParts tParts

                                ( leftPickerOffset, rightPickerOffset ) =
                                    calculatePickerOffsets baseTime (Just time) (Just end)

                                ( switchedLeftPickerOffset, switchedRightPickerOffset ) =
                                    calculatePickerOffsets baseTime (Just end) (Just time)
                            in
                            if Time.posixToMillis time < Time.posixToMillis end then
                                DatePicker { model | pickedStart = Just time, pickedEnd = Just end, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                            else if areSameDay then
                                let
                                    newEnd =
                                        { eParts | hour = 23, minute = 59 } |> Time.partsToPosix Time.utc

                                    newTime =
                                        { tParts | hour = 0, minute = 0 } |> Time.partsToPosix Time.utc
                                in
                                DatePicker { model | pickedStart = Just newTime, pickedEnd = Just newEnd, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                            else
                                let
                                    ( newEnd, newTime ) =
                                        switchTimes eParts tParts
                                in
                                DatePicker { model | pickedStart = Just newEnd, pickedEnd = Just newTime, leftPickerViewOffset = switchedLeftPickerOffset, rightPickerViewOffset = switchedRightPickerOffset }

                        ( Just start, Just end ) ->
                            let
                                sParts =
                                    Time.posixToParts Time.utc start

                                eParts =
                                    Time.posixToParts Time.utc end

                                tParts =
                                    Time.posixToParts Time.utc time

                                isTimeStart =
                                    doDaysMatch sParts tParts

                                isTimeEnd =
                                    doDaysMatch eParts tParts
                            in
                            if isTimeStart then
                                DatePicker { model | pickedStart = Nothing }

                            else if isTimeEnd then
                                DatePicker { model | pickedEnd = Nothing }

                            else
                                let
                                    ( leftPickerOffset, rightPickerOffset ) =
                                        calculatePickerOffsets baseTime (Just time) Nothing
                                in
                                DatePicker { model | pickedStart = Just time, pickedEnd = Nothing, leftPickerViewOffset = leftPickerOffset, rightPickerViewOffset = rightPickerOffset }

                SetHour startOrEnd hour ->
                    case startOrEnd of
                        Start ->
                            let
                                newTime =
                                    setTimeOfDay (Maybe.withDefault baseTime model.pickedStart) (Hour hour)
                            in
                            DatePicker { model | pickedStart = Just newTime }

                        End ->
                            let
                                newTime =
                                    setTimeOfDay (Maybe.withDefault baseTime model.pickedEnd) (Hour hour)
                            in
                            DatePicker { model | pickedEnd = Just newTime }

                SetMinute startOrEnd minute ->
                    case startOrEnd of
                        Start ->
                            let
                                newTime =
                                    setTimeOfDay (Maybe.withDefault baseTime model.pickedStart) (Minute minute)
                            in
                            DatePicker { model | pickedStart = Just newTime }

                        End ->
                            let
                                newTime =
                                    setTimeOfDay (Maybe.withDefault baseTime model.pickedEnd) (Minute minute)
                            in
                            DatePicker { model | pickedEnd = Just newTime }

                Close ->
                    DatePicker { model | status = Closed }

        Closed ->
            DatePicker model


setTimeOfDay : Posix -> HourOrMinute -> Posix
setTimeOfDay timeToUpdate hourOrMinute =
    let
        parts =
            Time.posixToParts Time.utc timeToUpdate

        newParts =
            case hourOrMinute of
                Hour hour ->
                    { parts | hour = hour }

                Minute minute ->
                    { parts | minute = minute }
    in
    Time.partsToPosix Time.utc newParts


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
                        [ viewCalendar settings model Left ( model.pickedStart, model.pickedEnd ) ( leftViewTime, model.leftPickerViewOffset, model.rightPickerViewOffset ) ]
                    , div [ class (classPrefix ++ "calendar-spacer") ] []
                    , div
                        [ class (classPrefix ++ "calendar") ]
                        [ viewCalendar settings model Right ( model.pickedStart, model.pickedEnd ) ( rightViewTime, model.leftPickerViewOffset, model.rightPickerViewOffset ) ]
                    ]
                , div [ class (classPrefix ++ "footer-container") ] [ viewFooter settings model ( model.pickedStart, model.pickedEnd ) ]
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


viewCalendar : Settings msg -> Model -> LeftOrRight -> ( Maybe Posix, Maybe Posix ) -> ( Posix, Int, Int ) -> Html msg
viewCalendar settings model leftOrRight ( pickedStart, pickedEnd ) ( viewTime, leftPointer, rightPointer ) =
    div
        []
        [ viewCalendarHeader settings model leftOrRight viewTime leftPointer rightPointer
        , viewMonth settings model ( pickedStart, pickedEnd ) viewTime
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


viewMonth : Settings msg -> Model -> ( Maybe Posix, Maybe Posix ) -> Posix -> Html msg
viewMonth settings model ( pickedStart, pickedEnd ) viewTime =
    let
        monthRenderData =
            monthData viewTime

        currentMonth =
            Time.posixToParts Time.utc viewTime |> .month

        weeks =
            List.reverse (splitIntoWeeks monthRenderData [])
    in
    div
        [ class (classPrefix ++ "calendar-month"), onMouseOut <| settings.internalMsg (update ClearHoveredDay (DatePicker model)) ]
        [ div [] (List.map (viewWeek settings model currentMonth ( pickedStart, pickedEnd )) weeks)
        ]


viewWeek : Settings msg -> Model -> Month -> ( Maybe Posix, Maybe Posix ) -> List Parts -> Html msg
viewWeek settings model currentMonth ( pickedStart, pickedEnd ) week =
    div [ class (classPrefix ++ "calendar-week") ]
        (List.map (viewDay settings model currentMonth ( pickedStart, pickedEnd )) week)


viewDay : Settings msg -> Model -> Month -> ( Maybe Posix, Maybe Posix ) -> Parts -> Html msg
viewDay settings model currentMonth ( pickedStart, pickedEnd ) day =
    let
        isToday =
            Maybe.map (\tday -> doDaysMatch day (Time.posixToParts Time.utc tday)) settings.today
                |> Maybe.withDefault False

        ( isPicked, isBetween ) =
            durationDayPickedOrBetween day model.hovered ( pickedStart, pickedEnd )

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
                baseAttrs ++ [ onClick <| settings.internalMsg (update (SelectDateTime (Time.partsToPosix Time.utc day)) (DatePicker model)) ]
    in
    div
        attrs
        [ text (String.fromInt day.day) ]


viewFooter : Settings msg -> Model -> ( Maybe Posix, Maybe Posix ) -> Html msg
viewFooter settings model ( pickedStart, pickedEnd ) =
    div
        [ class (classPrefix ++ "footer") ]
        [ div [ class (classPrefix ++ "time-pickers-container") ]
            [ div []
                [ div
                    [ class (classPrefix ++ "time-picker-container") ]
                    [ text "Start", viewTimePicker settings model Start pickedStart ]
                ]
            , div []
                [ div
                    [ class (classPrefix ++ "time-picker-container") ]
                    [ text "End", viewTimePicker settings model End pickedEnd ]
                ]
            ]
        , div [ class (classPrefix ++ "confirm-button-container") ] [ viewConfirmButton settings pickedStart pickedEnd ]
        ]


viewConfirmButton : Settings msg -> Maybe Posix -> Maybe Posix -> Html msg
viewConfirmButton settings pickedStart pickedEnd =
    let
        ( classStr, confirmAction ) =
            Maybe.map2 (\start end -> ( classPrefix ++ "confirm-button", [ onClick <| settings.selectedMsg start end ] )) pickedStart pickedEnd
                |> Maybe.withDefault ( classPrefix ++ "confirm-button " ++ classPrefix ++ "disabled", [] )

        confirmAttrs =
            [ class classStr ] ++ confirmAction
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
            [ div [ class (classPrefix ++ "select") ] [ select [ on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update msg (DatePicker model)) (Decode.map (SetHour startOrEnd) targetValueIntParse))) ] (generateHourOptions hour) ]
            , div [ class (classPrefix ++ "select-spacer") ] [ text ":" ]
            , div [ class (classPrefix ++ "select") ] [ select [ on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update msg (DatePicker model)) (Decode.map (SetMinute startOrEnd) targetValueIntParse))) ] (generateMinuteOptions minute) ]
            ]
        ]


generateHourOptions : Int -> List (Html msg)
generateHourOptions selectedHour =
    List.range 0 23
        |> List.map (\hour -> option [ value (String.fromInt hour), selected (selectedHour == hour) ] [ text (addLeadingZero hour) ])


generateMinuteOptions : Int -> List (Html msg)
generateMinuteOptions selectedMinute =
    List.range 0 59
        |> List.map (\minute -> option [ value (String.fromInt minute), selected (selectedMinute == minute) ] [ text (addLeadingZero minute) ])


outsideComponent : String -> (DatePicker -> msg) -> DatePicker -> Decode.Decoder msg
outsideComponent componentId internalMsg datePicker =
    Decode.field "target" (isOutsideComponent componentId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| internalMsg (update Close datePicker)

                else
                    Decode.fail "inside component"
            )


isOutsideComponent : String -> Decode.Decoder Bool
isOutsideComponent componentId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if componentId == id then
                        -- found match by id
                        Decode.succeed False

                    else
                        -- try next decoder
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> isOutsideComponent componentId |> Decode.field "parentNode")

        -- fallback if all previous decoders failed
        , Decode.succeed True
        ]
