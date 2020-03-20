module DurationDatePicker exposing
    ( DatePicker, init, view, subscriptions
    , Settings, defaultSettings
    , openPicker, closePicker
    , isOpen
    )

{-| A date picker component for picking a datetime range.


# Architecture

@docs DatePicker, init, view, subscriptions


# Settings

@docs Settings, defaultSettings


# Externally Triggered Actions

@docs openPicker, closePicker


# Query

@docs isOpen

-}

import Browser.Events
import DatePicker.Icons as Icons
import DatePicker.Styles
import DatePicker.Utilities as Utilities
import Html exposing (Html, button, div, select, span, text)
import Html.Attributes exposing (class, disabled, id, type_)
import Html.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Decode
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra as Time exposing (Interval(..))


type alias Model =
    { status : Status
    , hovered : Maybe Posix
    , pickerOffset : Int
    , pickedStart : Maybe Posix
    , pickedEnd : Maybe Posix
    }


{-| The opaque type representing a particular date picker instance.
-}
type DatePicker
    = DatePicker Model


{-| The type facilitating the configuration of the datepicker settings.

Because it could be the case that a picker is being used in a different
timezone than the home timezone of the implementor, the subfunctions of
the `dateTimeProcessor` both ingest a `Zone` in addition to a `Posix`. The
`Zone` represents the time zone in which the picker is being used. An
implementor can leverage this to compare against a base time zone when
enforcing allowable times of day, etc. You SHOULD assume that the `Posix`
passed into these functions is floored to the start of its respective `Day`.

More information can be found in the [examples](https://github.com/mercurymedia/elm-datetime-picker/tree/master/examples).

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
        { isDayDisabled : Zone -> Posix -> Bool
        , allowedTimesOfDay :
            Zone
            -> Posix
            ->
                { startHour : Int
                , startMinute : Int
                , endHour : Int
                , endMinute : Int
                }
        }
    , internalMsg : ( DatePicker, Maybe ( Posix, Posix ) ) -> msg
    , dateStringFn : Zone -> Posix -> String
    , timeStringFn : Zone -> Posix -> String
    , zone : Zone
    , isFooterDisabled : Bool
    }


type Status
    = Closed
    | Open Posix


type StartOrEnd
    = Start
    | End


{-| A record of default settings for the date picker. Extend this if
you want to further customize the date picker.

Requires a `Zone` to inform the picker in which time zone it should
display the selected duration as well as a `msg` that expects a tuple containing
a datepicker instance and a `Maybe` tuple representing a selected duration.

    ( DatePicker, Maybe ( Posix, Posix ) ) -> msg

-}
defaultSettings : Zone -> (( DatePicker, Maybe ( Posix, Posix ) ) -> msg) -> Settings msg
defaultSettings zone internalMsg =
    { formattedDay = Utilities.dayToNameString
    , formattedMonth = Utilities.monthToNameString
    , focusedDate = Nothing
    , dateTimeProcessor =
        { isDayDisabled = \_ _ -> False
        , allowedTimesOfDay = \_ _ -> { startHour = 0, startMinute = 0, endHour = 23, endMinute = 59 }
        }
    , internalMsg = internalMsg
    , dateStringFn = \_ _ -> ""
    , timeStringFn = \_ _ -> ""
    , zone = zone
    , isFooterDisabled = False
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
        , pickerOffset = 0
        , pickedStart = Nothing
        , pickedEnd = Nothing
        }


datePickerId : String
datePickerId =
    "date-picker-component"


{-| Events external to the picker to which it is subscribed.
-}
subscriptions : Settings msg -> (( DatePicker, Maybe ( Posix, Posix ) ) -> msg) -> DatePicker -> Sub msg
subscriptions settings internalMsg (DatePicker model) =
    case model.status of
        Open _ ->
            Browser.Events.onMouseDown (clickedOutsidePicker settings datePickerId internalMsg (DatePicker model))

        Closed ->
            Sub.none


clickedOutsidePicker : Settings msg -> String -> (( DatePicker, Maybe ( Posix, Posix ) ) -> msg) -> DatePicker -> Decode.Decoder msg
clickedOutsidePicker settings componentId internalMsg datePicker =
    Decode.field "target" (Utilities.eventIsOutsideComponent componentId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| internalMsg (update settings Close datePicker)

                else
                    Decode.fail "inside component"
            )


calculatePickerOffset : Zone -> Posix -> Maybe Posix -> Int
calculatePickerOffset zone baseTime pickedStartTime =
    let
        flooredBase =
            Time.floor Month zone baseTime
    in
    case pickedStartTime of
        Nothing ->
            0

        Just time ->
            let
                flooredStart =
                    Time.floor Month zone time
            in
            if Time.posixToMillis flooredBase <= Time.posixToMillis flooredStart then
                Time.diff Month zone flooredBase flooredStart

            else
                0 - Time.diff Month zone flooredStart flooredBase


{-| Open the provided date picker and receive the updated picker instance. Also
takes a default time the picker should center on (in the event a time has not yet
been picked) as well as the picked start and end times. A common example of a default time
would be the datetime for the current day.
-}
openPicker : Zone -> Posix -> Maybe Posix -> Maybe Posix -> DatePicker -> DatePicker
openPicker zone baseTime start end (DatePicker model) =
    let
        pickerOffset =
            calculatePickerOffset zone baseTime start
    in
    DatePicker
        { model
            | status = Open baseTime
            , pickedStart = start
            , pickedEnd = end
            , pickerOffset = pickerOffset
        }


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
    | SetRange
    | SetHour StartOrEnd Int
    | SetMinute StartOrEnd Int
    | Close


validRuntimeOrNothing : Settings msg -> Maybe Posix -> Maybe Posix -> Maybe ( Posix, Posix )
validRuntimeOrNothing settings start end =
    Maybe.map2
        (\s e ->
            if
                Time.posixToMillis s
                    < Time.posixToMillis e
                    && timeWithinBoundariesOfGivenDay settings s
                    && timeWithinBoundariesOfGivenDay settings e
                    && not (settings.dateTimeProcessor.isDayDisabled settings.zone (Time.floor Day settings.zone s))
                    && not (settings.dateTimeProcessor.isDayDisabled settings.zone (Time.floor Day settings.zone e))
            then
                Just ( s, e )

            else
                Nothing
        )
        start
        end
        |> Maybe.withDefault Nothing


update : Settings msg -> Msg -> DatePicker -> ( DatePicker, Maybe ( Posix, Posix ) )
update settings msg (DatePicker model) =
    case model.status of
        Open baseTime ->
            let
                boundedBaseTime =
                    Utilities.enforceTimeBoundaries settings.zone baseTime settings.dateTimeProcessor.allowedTimesOfDay
            in
            case msg of
                NextMonth ->
                    ( DatePicker { model | pickerOffset = model.pickerOffset + 1 }, Nothing )

                PrevMonth ->
                    ( DatePicker { model | pickerOffset = model.pickerOffset - 1 }, Nothing )

                NextYear ->
                    ( DatePicker { model | pickerOffset = model.pickerOffset + 12 }, Nothing )

                PrevYear ->
                    ( DatePicker { model | pickerOffset = model.pickerOffset - 12 }, Nothing )

                SetHoveredDay time ->
                    ( DatePicker { model | hovered = Just <| Utilities.enforceTimeBoundaries settings.zone time settings.dateTimeProcessor.allowedTimesOfDay }, Nothing )

                ClearHoveredDay ->
                    ( DatePicker { model | hovered = Nothing }, Nothing )

                SetRange ->
                    case ( model.pickedStart, model.pickedEnd ) of
                        ( Just s, Just e ) ->
                            Maybe.map
                                (\hovered ->
                                    if Utilities.doDaysMatch settings.zone e hovered then
                                        ( DatePicker { model | pickedStart = Just s, pickedEnd = Nothing }, Nothing )

                                    else if Utilities.doDaysMatch settings.zone s hovered then
                                        ( DatePicker { model | pickedStart = Nothing, pickedEnd = Just e }, Nothing )

                                    else
                                        ( DatePicker { model | pickedStart = Just hovered, pickedEnd = Nothing }, Nothing )
                                )
                                model.hovered
                                |> Maybe.withDefault ( DatePicker model, Nothing )

                        _ ->
                            let
                                ( start, end ) =
                                    determineDateTimeRange settings.zone settings.dateTimeProcessor.isDayDisabled model.pickedStart model.pickedEnd model.hovered
                            in
                            ( DatePicker { model | pickedStart = start, pickedEnd = end }, validRuntimeOrNothing settings start end )

                SetHour startOrEnd hour ->
                    case startOrEnd of
                        Start ->
                            let
                                newTime =
                                    Utilities.setHourNotDay settings.zone hour (Maybe.withDefault boundedBaseTime model.pickedStart)
                            in
                            ( DatePicker { model | pickedStart = Just newTime }, validRuntimeOrNothing settings (Just newTime) model.pickedEnd )

                        End ->
                            let
                                defaultTime =
                                    Maybe.map (Time.floor Day settings.zone) model.pickedStart |> Maybe.withDefault boundedBaseTime

                                newTime =
                                    Utilities.setHourNotDay settings.zone hour (Maybe.withDefault defaultTime model.pickedEnd)
                            in
                            ( DatePicker { model | pickedEnd = Just newTime }, validRuntimeOrNothing settings model.pickedStart (Just newTime) )

                SetMinute startOrEnd minute ->
                    case startOrEnd of
                        Start ->
                            let
                                newTime =
                                    Utilities.setMinuteNotDay settings.zone minute (Maybe.withDefault boundedBaseTime model.pickedStart)
                            in
                            ( DatePicker { model | pickedStart = Just newTime }, validRuntimeOrNothing settings (Just newTime) model.pickedEnd )

                        End ->
                            let
                                defaultTime =
                                    Maybe.map (Time.floor Day settings.zone) model.pickedStart |> Maybe.withDefault boundedBaseTime

                                newTime =
                                    Utilities.setMinuteNotDay settings.zone minute (Maybe.withDefault defaultTime model.pickedEnd)
                            in
                            ( DatePicker { model | pickedEnd = Just newTime }, validRuntimeOrNothing settings model.pickedStart (Just newTime) )

                Close ->
                    ( DatePicker { model | status = Closed }, Nothing )

        Closed ->
            ( DatePicker model, Nothing )


classPrefix : String
classPrefix =
    "elm-datetimepicker-duration--"


determineDateTimeRange : Zone -> (Zone -> Posix -> Bool) -> Maybe Posix -> Maybe Posix -> Maybe Posix -> ( Maybe Posix, Maybe Posix )
determineDateTimeRange zone isDayDisabled pickedStart pickedEnd hoveredDate =
    case hoveredDate of
        Just hovered ->
            let
                maybeHovered =
                    if isDayDisabled zone hovered then
                        Nothing

                    else
                        Just hovered
            in
            case ( pickedStart, pickedEnd ) of
                ( Just start, Just end ) ->
                    ( Just start, Just end )

                ( Just start, Nothing ) ->
                    if firstLessThanOrEqualsSecond start hovered then
                        ( Just start, maybeHovered )

                    else
                        ( maybeHovered, Just start )

                ( Nothing, Just end ) ->
                    if firstLessThanOrEqualsSecond hovered end then
                        ( maybeHovered, Just end )

                    else
                        ( Just end, maybeHovered )

                ( Nothing, Nothing ) ->
                    ( maybeHovered, Nothing )

        Nothing ->
            ( pickedStart, pickedEnd )


{-| The date picker view. Simply pass it the configured settings
and the date picker instance you wish to view.
-}
view : Settings msg -> DatePicker -> Html msg
view settings (DatePicker model) =
    case model.status of
        Open baseTime ->
            let
                leftViewTime =
                    Time.add Month model.pickerOffset settings.zone baseTime

                rightViewTime =
                    Time.add Month (model.pickerOffset + 1) settings.zone baseTime
            in
            div
                [ id datePickerId, class (classPrefix ++ "picker-container") ]
                [ viewPickerHeader settings model
                , div
                    [ class (classPrefix ++ "calendars-container") ]
                    [ div
                        [ class (classPrefix ++ "calendar") ]
                        [ viewCalendar settings model leftViewTime ]
                    , div [ class (classPrefix ++ "calendar-spacer") ] []
                    , div
                        [ class (classPrefix ++ "calendar") ]
                        [ viewCalendar settings model rightViewTime ]
                    ]
                , if settings.isFooterDisabled then
                    text ""

                  else
                    div [ class (classPrefix ++ "footer-container") ] [ viewFooter settings model ]
                ]

        Closed ->
            text ""


viewPickerHeader : Settings msg -> Model -> Html msg
viewPickerHeader settings model =
    div []
        [ div [ class (classPrefix ++ "picker-header-chevrons") ]
            [ div
                [ class (classPrefix ++ "picker-header-chevron")
                , onClick <| settings.internalMsg <| update settings PrevMonth (DatePicker model)
                ]
                [ Icons.chevronLeft
                    |> Icons.withSize 15
                    |> Icons.toHtml []
                ]
            , div
                [ class (classPrefix ++ "picker-header-chevron")
                , onClick <| settings.internalMsg <| update settings NextMonth (DatePicker model)
                ]
                [ Icons.chevronRight
                    |> Icons.withSize 15
                    |> Icons.toHtml []
                ]
            ]
        , div [ class (classPrefix ++ "picker-header-chevrons") ]
            [ div
                [ class (classPrefix ++ "picker-header-chevron")
                , onClick <| settings.internalMsg <| update settings PrevYear (DatePicker model)
                ]
                [ Icons.chevronsLeft
                    |> Icons.withSize 15
                    |> Icons.toHtml []
                ]
            , div
                [ class (classPrefix ++ "picker-header-chevron")
                , onClick <| settings.internalMsg <| update settings NextYear (DatePicker model)
                ]
                [ Icons.chevronsRight
                    |> Icons.withSize 15
                    |> Icons.toHtml []
                ]
            ]
        ]


viewCalendar : Settings msg -> Model -> Posix -> Html msg
viewCalendar settings model viewTime =
    div
        []
        [ viewCalendarHeader settings viewTime
        , viewMonth settings model viewTime
        ]


viewCalendarHeader : Settings msg -> Posix -> Html msg
viewCalendarHeader settings viewTime =
    let
        monthName =
            Time.toMonth settings.zone viewTime |> settings.formattedMonth

        year =
            Time.toYear settings.zone viewTime |> String.fromInt
    in
    div
        [ class (classPrefix ++ "calendar-header") ]
        [ div [ class (classPrefix ++ "calendar-header-row") ]
            [ div
                [ class (classPrefix ++ "calendar-header-text") ]
                [ div [] [ text monthName ] ]
            ]
        , div [ class (classPrefix ++ "calendar-header-row") ]
            [ div
                [ class (classPrefix ++ "calendar-header-text") ]
                [ div [] [ text year ] ]
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
            Utilities.monthData settings.zone viewTime

        currentMonth =
            Time.posixToParts settings.zone viewTime |> .month

        weeks =
            List.reverse (Utilities.splitIntoWeeks monthRenderData [])
    in
    div
        [ class (classPrefix ++ "calendar-month"), onMouseOut <| settings.internalMsg (update settings ClearHoveredDay (DatePicker model)) ]
        [ div [] (List.map (viewWeek settings model currentMonth) weeks)
        ]


viewWeek : Settings msg -> Model -> Month -> List Posix -> Html msg
viewWeek settings model currentMonth week =
    div [ class (classPrefix ++ "calendar-week") ]
        (List.map (viewDay settings model currentMonth) week)


viewDay : Settings msg -> Model -> Month -> Posix -> Html msg
viewDay settings model currentMonth day =
    let
        dayParts =
            Time.posixToParts settings.zone day

        isToday =
            Maybe.map (\tday -> Utilities.doDaysMatch settings.zone day tday) settings.focusedDate
                |> Maybe.withDefault False

        ( isPicked, isBetween ) =
            Utilities.durationDayPickedOrBetween settings.zone day model.hovered ( model.pickedStart, model.pickedEnd )

        isDisabled =
            settings.dateTimeProcessor.isDayDisabled settings.zone day || not (areAllowedTimesValid (settings.dateTimeProcessor.allowedTimesOfDay settings.zone day))

        dayClasses =
            DatePicker.Styles.durationDayClasses classPrefix (dayParts.month /= currentMonth) isDisabled isPicked isToday isBetween

        attrs =
            if isDisabled then
                [ class dayClasses ]

            else
                [ class dayClasses
                , onClick <| settings.internalMsg (update settings SetRange (DatePicker model))
                , onMouseOver <| settings.internalMsg (update settings (SetHoveredDay day) (DatePicker model))
                ]
    in
    button
        ([ type_ "button", disabled isDisabled ] ++ attrs)
        [ text <| String.fromInt dayParts.day ]


viewDateTime : Settings msg -> String -> Posix -> Html msg
viewDateTime settings classString dateTime =
    span []
        [ text <| settings.dateStringFn settings.zone dateTime
        , span [ class (classPrefix ++ classString) ] [ text (settings.timeStringFn settings.zone dateTime) ]
        ]


viewEmpty : Html msg
viewEmpty =
    span [] [ text "" ]


timeWithinBoundariesOfGivenDay : Settings msg -> Posix -> Bool
timeWithinBoundariesOfGivenDay settings time =
    let
        { hour, minute } =
            Time.posixToParts settings.zone time

        { startHour, startMinute, endHour, endMinute } =
            settings.dateTimeProcessor.allowedTimesOfDay settings.zone time
    in
    if startHour == hour && endHour /= hour then
        startMinute <= minute

    else if startHour /= hour && endHour == hour then
        minute <= endMinute

    else if startHour == hour && endHour == hour then
        startMinute <= minute && minute <= endMinute

    else
        startHour < hour && hour < endHour


determineDateTimeViews : Settings msg -> ( Maybe Posix, Maybe Posix ) -> ( Html msg, Html msg )
determineDateTimeViews settings ( displayStart, displayEnd ) =
    let
        startWithinBoundaries =
            \start ->
                timeWithinBoundariesOfGivenDay settings start

        endWithinBoundaries =
            \end ->
                timeWithinBoundariesOfGivenDay settings end
    in
    case ( displayStart, displayEnd ) of
        ( Nothing, Nothing ) ->
            ( viewEmpty, viewEmpty )

        ( Just start, Nothing ) ->
            if startWithinBoundaries start then
                ( viewDateTime settings "selection-time" start, viewEmpty )

            else
                ( viewDateTime settings "selection-time-danger" start, viewEmpty )

        ( Nothing, Just end ) ->
            if endWithinBoundaries end then
                ( viewEmpty, viewDateTime settings "selection-time" end )

            else
                ( viewEmpty, viewDateTime settings "selection-time-danger" end )

        ( Just start, Just end ) ->
            if start == end then
                ( viewDateTime settings "selection-time" start, viewDateTime settings "selection-time-danger" end )

            else if Time.posixToMillis start > Time.posixToMillis end then
                ( viewDateTime settings "selection-time-danger" start, viewDateTime settings "selection-time" end )

            else if not (startWithinBoundaries start) && endWithinBoundaries end then
                ( viewDateTime settings "selection-time-danger" start, viewDateTime settings "selection-time" end )

            else if startWithinBoundaries start && not (endWithinBoundaries end) then
                ( viewDateTime settings "selection-time" start, viewDateTime settings "selection-time-danger" end )

            else if not (startWithinBoundaries start) && not (endWithinBoundaries end) then
                ( viewDateTime settings "selection-time-danger" start, viewDateTime settings "selection-time-danger" end )

            else
                ( viewDateTime settings "selection-time" start, viewDateTime settings "selection-time" end )


viewFooter : Settings msg -> Model -> Html msg
viewFooter settings model =
    let
        ( startDate, endDate ) =
            determineDateTimeRange settings.zone settings.dateTimeProcessor.isDayDisabled model.pickedStart model.pickedEnd model.hovered

        ( startDisplayDate, endDisplayDate ) =
            determineDateTimeViews settings ( startDate, endDate )
    in
    div
        [ class (classPrefix ++ "footer") ]
        [ div [ class (classPrefix ++ "time-pickers-container") ]
            [ div [ class (classPrefix ++ "time-picker-information-container") ]
                [ div
                    [ class (classPrefix ++ "time-picker-container") ]
                    [ text "Start", viewTimePicker settings model Start startDate ]
                , div [ class (classPrefix ++ "date-display-container") ] [ startDisplayDate ]
                ]
            , div [ class (classPrefix ++ "time-picker-information-container") ]
                [ div
                    [ class (classPrefix ++ "time-picker-container") ]
                    [ text "End", viewTimePicker settings model End endDate ]
                , div [ class (classPrefix ++ "date-display-container") ] [ endDisplayDate ]
                ]
            ]
        ]


firstLessThanOrEqualsSecond : Posix -> Posix -> Bool
firstLessThanOrEqualsSecond first second =
    Time.posixToMillis first <= Time.posixToMillis second


viewTimePicker : Settings msg -> Model -> StartOrEnd -> Maybe Posix -> Html msg
viewTimePicker settings model startOrEnd pickedTime =
    let
        selectEnabled =
            Maybe.map (\time -> areAllowedTimesValid (settings.dateTimeProcessor.allowedTimesOfDay settings.zone time)) pickedTime |> Maybe.withDefault False

        { selectedHour, selectableHours, selectedMinute, selectableMinutes } =
            if selectEnabled then
                Utilities.selectedAndSelectableTimeParts settings.zone pickedTime settings.dateTimeProcessor.allowedTimesOfDay

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
            [ div [ class (classPrefix ++ "select") ] [ select [ disabled <| not selectEnabled, on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update settings msg (DatePicker model)) (Decode.map (SetHour startOrEnd) targetValueIntParse))) ] (Utilities.generateHourOptions selectableHours selectedHour) ]
            , div [ class (classPrefix ++ "select-spacer") ] [ text ":" ]
            , div [ class (classPrefix ++ "select") ] [ select [ disabled <| not selectEnabled, on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update settings msg (DatePicker model)) (Decode.map (SetMinute startOrEnd) targetValueIntParse))) ] (Utilities.generateMinuteOptions selectableMinutes selectedMinute) ]
            ]
        ]
