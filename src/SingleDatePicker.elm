module SingleDatePicker exposing
    ( DatePicker, init, view, subscriptions
    , Settings, defaultSettings
    , openPicker, closePicker
    , isOpen
    )

{-| A date picker component for a single datetime.


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
import Html exposing (Html, div, select, span, text)
import Html.Attributes exposing (class, disabled, id)
import Html.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Decode
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..))
import Time.Extra as Time exposing (Interval(..))


type alias Model =
    { status : Status
    , hovered : Maybe Posix
    , viewOffset : Int
    , pickedTime : Maybe Posix
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
    , focusedDate : Maybe Posix
    , dateTimeProcessor :
        { isDayDisabled : Posix -> Bool
        , allowedTimesOfDay :
            Posix
            ->
                { startHour : Int
                , startMinute : Int
                , endHour : Int
                , endMinute : Int
                }
        }
    , internalMsg : ( DatePicker, Maybe Posix ) -> msg
    , dateStringFn : Posix -> String
    , timeStringFn : Posix -> String
    }


type Status
    = Closed
    | Open Posix


{-| A record of default settings for the date picker. Extend this if
you want to further customize the date picker.

Requires a `msg` that expects a tuple containing a datepicker instance
and a `Maybe Posix` representing a selected datetime.

    ( DatePicker, Maybe ( Posix, Posix ) ) -> msg

-}
defaultSettings : (( DatePicker, Maybe Posix ) -> msg) -> Settings msg
defaultSettings internalMsg =
    { formattedDay = Utilities.dayToNameString
    , formattedMonth = Utilities.monthToNameString
    , focusedDate = Nothing
    , dateTimeProcessor =
        { isDayDisabled = always False
        , allowedTimesOfDay = \_ -> { startHour = 0, startMinute = 0, endHour = 23, endMinute = 59 }
        }
    , internalMsg = internalMsg
    , dateStringFn = \_ -> ""
    , timeStringFn = \_ -> ""
    }


areAllowedTimesValid : { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int } -> Bool
areAllowedTimesValid { startHour, startMinute, endHour, endMinute } =
    if startHour == endHour then
        startMinute < endMinute

    else
        startHour < endHour


{-| Instantiates and returns a date picker.
-}
init : DatePicker
init =
    DatePicker
        { status = Closed
        , hovered = Nothing
        , viewOffset = 0
        , pickedTime = Nothing
        }


datePickerId : String
datePickerId =
    "date-picker-component"


{-| Events external to the picker to which it is subscribed.
-}
subscriptions : Settings msg -> (( DatePicker, Maybe Posix ) -> msg) -> DatePicker -> Sub msg
subscriptions settings internalMsg (DatePicker model) =
    case model.status of
        Open _ ->
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


calculatePickerOffset : Posix -> Maybe Posix -> Int
calculatePickerOffset baseTime pickedTime =
    let
        flooredBase =
            Time.floor Month Time.utc baseTime
    in
    case pickedTime of
        Nothing ->
            0

        Just time ->
            let
                flooredPick =
                    Time.floor Month Time.utc time
            in
            if Time.posixToMillis flooredBase <= Time.posixToMillis flooredPick then
                Time.diff Month Time.utc flooredBase flooredPick

            else
                0 - Time.diff Month Time.utc flooredPick flooredBase


{-| Open the provided date picker and receive the updated picker instance. Also
takes a default time the picker should center on (in the event a time has not yet
been picked) as well as the picked time. A common example of a default time
would be the datetime for the current day.
-}
openPicker : Posix -> Maybe Posix -> DatePicker -> DatePicker
openPicker baseTime pickedTime (DatePicker model) =
    let
        pickerOffset =
            calculatePickerOffset baseTime pickedTime
    in
    DatePicker { model | status = Open baseTime, pickedTime = pickedTime, viewOffset = pickerOffset }


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
        Open _ ->
            True

        Closed ->
            False


type Msg
    = NextMonth
    | PrevMonth
    | NextYear
    | PrevYear
    | SetHoveredDay Posix
    | ClearHoveredDay
    | SetDay
    | SetHour Int
    | SetMinute Int
    | Close


validTimeOrNothing : Settings msg -> Maybe Posix -> Maybe Posix
validTimeOrNothing settings time =
    Maybe.map
        (\t ->
            if timeWithinBoundariesOfGivenDay settings t && not (settings.dateTimeProcessor.isDayDisabled t) then
                Just t

            else
                Nothing
        )
        time
        |> Maybe.withDefault Nothing


update : Settings msg -> Msg -> DatePicker -> ( DatePicker, Maybe Posix )
update settings msg (DatePicker model) =
    case model.status of
        Open baseTime ->
            let
                boundedBaseTime =
                    Utilities.enforceTimeBoundaries baseTime settings.dateTimeProcessor.allowedTimesOfDay
            in
            case msg of
                NextMonth ->
                    ( DatePicker { model | viewOffset = model.viewOffset + 1 }, Nothing )

                PrevMonth ->
                    ( DatePicker { model | viewOffset = model.viewOffset - 1 }, Nothing )

                NextYear ->
                    ( DatePicker { model | viewOffset = model.viewOffset + 12 }, Nothing )

                PrevYear ->
                    ( DatePicker { model | viewOffset = model.viewOffset - 12 }, Nothing )

                SetHoveredDay time ->
                    ( DatePicker { model | hovered = Just <| Utilities.enforceTimeBoundaries time settings.dateTimeProcessor.allowedTimesOfDay }, Nothing )

                ClearHoveredDay ->
                    ( DatePicker { model | hovered = Nothing }, Nothing )

                SetDay ->
                    let
                        time =
                            determineDateTime settings.dateTimeProcessor.isDayDisabled model.pickedTime model.hovered
                    in
                    ( DatePicker { model | pickedTime = time }, validTimeOrNothing settings time )

                SetHour hour ->
                    let
                        newTime =
                            Just <| Utilities.setHourNotDay hour (Maybe.withDefault boundedBaseTime model.pickedTime)
                    in
                    ( DatePicker { model | pickedTime = newTime }, validTimeOrNothing settings newTime )

                SetMinute minute ->
                    let
                        newTime =
                            Just <| Utilities.setMinuteNotDay minute (Maybe.withDefault boundedBaseTime model.pickedTime)
                    in
                    ( DatePicker { model | pickedTime = newTime }, validTimeOrNothing settings newTime )

                Close ->
                    ( DatePicker { model | status = Closed }, Nothing )

        Closed ->
            ( DatePicker model, Nothing )


classPrefix : String
classPrefix =
    "elm-datetimepicker-single--"


determineDateTime : (Posix -> Bool) -> Maybe Posix -> Maybe Posix -> Maybe Posix
determineDateTime isDayDisabled pickedTime hoveredDate =
    case hoveredDate of
        Just hovered ->
            let
                maybeHovered =
                    if isDayDisabled hovered then
                        Nothing

                    else
                        Just hovered
            in
            case pickedTime of
                Just time ->
                    if Utilities.doDaysMatch time hovered then
                        Just time

                    else
                        maybeHovered

                Nothing ->
                    maybeHovered

        Nothing ->
            pickedTime


{-| The date picker view. Simply pass it the configured settings
and the date picker instance you wish to view.
-}
view : Settings msg -> DatePicker -> Html msg
view settings (DatePicker model) =
    case model.status of
        Open baseTime ->
            let
                offsetTime =
                    Time.add Month model.viewOffset Time.utc baseTime
            in
            div
                [ id datePickerId, class (classPrefix ++ "picker-container") ]
                [ div [ class (classPrefix ++ "calendar-container") ]
                    [ viewCalendarHeader settings model offsetTime
                    , viewMonth settings model model.pickedTime offsetTime
                    ]
                , viewFooter settings model
                ]

        Closed ->
            text ""


viewCalendarHeader : Settings msg -> Model -> Posix -> Html msg
viewCalendarHeader settings model time =
    let
        monthName =
            Time.toMonth Time.utc time |> settings.formattedMonth

        year =
            Time.toYear Time.utc time |> String.fromInt
    in
    div
        [ class (classPrefix ++ "calendar-header") ]
        [ div [ class (classPrefix ++ "calendar-header-row") ]
            [ div
                [ class (classPrefix ++ "calendar-header-chevron")
                , onClick <| settings.internalMsg <| update settings PrevMonth (DatePicker model)
                ]
                [ Icons.chevronLeft
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            , div
                [ class (classPrefix ++ "calendar-header-text") ]
                [ div [] [ text monthName ] ]
            , div
                [ class (classPrefix ++ "calendar-header-chevron")
                , onClick <| settings.internalMsg <| update settings NextMonth (DatePicker model)
                ]
                [ Icons.chevronRight
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            ]
        , div [ class (classPrefix ++ "calendar-header-row") ]
            [ div
                [ class (classPrefix ++ "calendar-header-chevron")
                , onClick <| settings.internalMsg <| update settings PrevYear (DatePicker model)
                ]
                [ Icons.chevronsLeft
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            , div
                [ class (classPrefix ++ "calendar-header-text") ]
                [ div [] [ text year ] ]
            , div
                [ class (classPrefix ++ "calendar-header-chevron")
                , onClick <| settings.internalMsg <| update settings NextYear (DatePicker model)
                ]
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


viewMonth : Settings msg -> Model -> Maybe Posix -> Posix -> Html msg
viewMonth settings model pickedTime viewTime =
    let
        monthRenderData =
            Utilities.monthData viewTime

        currentMonth =
            Time.posixToParts Time.utc viewTime |> .month

        weeks =
            List.reverse (Utilities.splitIntoWeeks monthRenderData [])
    in
    div
        [ class (classPrefix ++ "calendar-month"), onMouseOut <| settings.internalMsg (update settings ClearHoveredDay (DatePicker model)) ]
        [ div [] (List.map (viewWeek settings currentMonth pickedTime model) weeks)
        ]


viewWeek : Settings msg -> Month -> Maybe Posix -> Model -> List Posix -> Html msg
viewWeek settings currentMonth pickedTime model week =
    div [ class (classPrefix ++ "calendar-week") ]
        (List.map (viewDay settings model currentMonth pickedTime) week)


viewDay : Settings msg -> Model -> Month -> Maybe Posix -> Posix -> Html msg
viewDay settings model currentMonth pickedTime day =
    let
        dayParts =
            Time.posixToParts Time.utc day

        isToday =
            Maybe.map (\tday -> Utilities.doDaysMatch day tday) settings.focusedDate
                |> Maybe.withDefault False

        isPicked =
            case pickedTime of
                Just time ->
                    Utilities.doDaysMatch day time

                Nothing ->
                    False

        isDisabled =
            settings.dateTimeProcessor.isDayDisabled day || not (areAllowedTimesValid (settings.dateTimeProcessor.allowedTimesOfDay day))

        dayClasses =
            DatePicker.Styles.singleDayClasses classPrefix (dayParts.month /= currentMonth) isDisabled isPicked isToday

        attrs =
            if isDisabled then
                [ class dayClasses ]

            else
                [ class dayClasses
                , onClick <| settings.internalMsg (update settings SetDay (DatePicker model))
                , onMouseOver <| settings.internalMsg (update settings (SetHoveredDay day) (DatePicker model))
                ]
    in
    div
        attrs
        [ text (String.fromInt dayParts.day) ]


viewDateTime : Settings msg -> String -> Posix -> Html msg
viewDateTime settings classString dateTime =
    span []
        [ text (settings.dateStringFn dateTime)
        , span [ class (classPrefix ++ classString) ] [ text (settings.timeStringFn dateTime) ]
        ]


viewEmpty : Html msg
viewEmpty =
    span [] [ text "" ]


timeWithinBoundariesOfGivenDay : Settings msg -> Posix -> Bool
timeWithinBoundariesOfGivenDay settings time =
    let
        { hour, minute } =
            Time.posixToParts Time.utc time

        { startHour, startMinute, endHour, endMinute } =
            settings.dateTimeProcessor.allowedTimesOfDay time
    in
    if startHour == hour && endHour /= hour then
        startMinute <= minute

    else if startHour /= hour && endHour == hour then
        minute <= endMinute

    else if startHour == hour && endHour == hour then
        startMinute <= minute && minute <= endMinute

    else
        startHour < hour && hour < endHour


determineDateTimeView : Settings msg -> Maybe Posix -> Html msg
determineDateTimeView settings displayTime =
    let
        tWithinBoundaries =
            \time ->
                timeWithinBoundariesOfGivenDay settings time
    in
    case displayTime of
        Nothing ->
            viewEmpty

        Just time ->
            if tWithinBoundaries time then
                viewDateTime settings "selection-time" time

            else
                viewDateTime settings "selection-time-danger" time


viewFooter : Settings msg -> Model -> Html msg
viewFooter settings model =
    let
        displayTime =
            determineDateTime settings.dateTimeProcessor.isDayDisabled model.pickedTime model.hovered

        displayTimeView =
            determineDateTimeView settings displayTime
    in
    div
        [ class (classPrefix ++ "footer") ]
        [ div []
            [ div [ class (classPrefix ++ "time-picker-container") ] [ viewTimePicker settings model displayTime ]
            , div [ class (classPrefix ++ "date-display-container") ] [ displayTimeView ]
            ]
        ]


viewTimePicker : Settings msg -> Model -> Maybe Posix -> Html msg
viewTimePicker settings model pickedTime =
    let
        selectEnabled =
            Maybe.map (\time -> areAllowedTimesValid (settings.dateTimeProcessor.allowedTimesOfDay time)) pickedTime |> Maybe.withDefault False

        { selectedHour, selectableHours, selectedMinute, selectableMinutes } =
            if selectEnabled then
                Utilities.selectedAndSelectableTimeParts pickedTime settings.dateTimeProcessor.allowedTimesOfDay

            else
                { selectedHour = 0, selectableHours = [ 0 ], selectedMinute = 0, selectableMinutes = [ 0 ] }
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
            [ div [ class (classPrefix ++ "select") ] [ select [ disabled <| not selectEnabled, on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update settings msg (DatePicker model)) (Decode.map SetHour targetValueIntParse))) ] (Utilities.generateHourOptions selectableHours selectedHour) ]
            , div [ class (classPrefix ++ "select-spacer") ] [ text ":" ]
            , div [ class (classPrefix ++ "select") ] [ select [ disabled <| not selectEnabled, on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update settings msg (DatePicker model)) (Decode.map SetMinute targetValueIntParse))) ] (Utilities.generateMinuteOptions selectableMinutes selectedMinute) ]
            ]
        ]
