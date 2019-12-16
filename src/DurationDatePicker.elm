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

-}

import Browser.Events
import DatePicker.Icons as Icons
import DatePicker.Styles
import DatePicker.Utilities as Utilities
import Html exposing (Html, button, div, select, span, text)
import Html.Attributes exposing (attribute, class, id, type_)
import Html.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Decode
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..))
import Time.Extra as Time exposing (Interval(..), Parts)


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
    , internalMsg : ( DatePicker, Maybe ( Posix, Posix ) ) -> msg
    , dateStringFn : Posix -> String
    , timeStringFn : Posix -> String
    }


type Status
    = Closed
    | Open Posix


type StartOrEnd
    = Start
    | End


{-| A record of default settings for the date picker. Extend this if
you want to further customize the date picker.

Requires a `msg` that expects a tuple containing a datepicker instance
and a `Maybe` tuple representing a selected duration.

    ( DatePicker, Maybe ( Posix, Posix ) ) -> msg

-}
defaultSettings : (( DatePicker, Maybe ( Posix, Posix ) ) -> msg) -> Settings msg
defaultSettings internalMsg =
    { formattedDay = Utilities.dayToNameString
    , formattedMonth = Utilities.monthToNameString
    , today = Nothing
    , dayDisabled = always False
    , internalMsg = internalMsg
    , dateStringFn = \_ -> ""
    , timeStringFn = \_ -> ""
    }


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
subscriptions : (( DatePicker, Maybe ( Posix, Posix ) ) -> msg) -> DatePicker -> Sub msg
subscriptions internalMsg (DatePicker model) =
    case model.status of
        Open _ ->
            Browser.Events.onMouseDown (clickedOutsidePicker datePickerId internalMsg (DatePicker model))

        Closed ->
            Sub.none


clickedOutsidePicker : String -> (( DatePicker, Maybe ( Posix, Posix ) ) -> msg) -> DatePicker -> Decode.Decoder msg
clickedOutsidePicker componentId internalMsg datePicker =
    Decode.field "target" (Utilities.eventIsOutsideComponent componentId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed <| internalMsg (update Close datePicker)

                else
                    Decode.fail "inside component"
            )


calculatePickerOffset : Posix -> Maybe Posix -> Int
calculatePickerOffset baseTime pickedStartTime =
    let
        flooredBase =
            Time.floor Month Time.utc baseTime
    in
    case pickedStartTime of
        Nothing ->
            0

        Just time ->
            let
                flooredStart =
                    Time.floor Month Time.utc time
            in
            if Time.posixToMillis flooredBase <= Time.posixToMillis flooredStart then
                Time.diff Month Time.utc flooredBase flooredStart

            else
                0 - Time.diff Month Time.utc flooredStart flooredBase


{-| Open the provided date picker and receive the updated picker instance. Also
takes a default time the picker should center on (in the event a time has not yet
been picked) as well as the picked start and end times. A common example of a default time
would be the datetime for the current day.
-}
openPicker : Posix -> Maybe Posix -> Maybe Posix -> DatePicker -> DatePicker
openPicker baseTime start end (DatePicker model) =
    let
        pickerOffset =
            calculatePickerOffset baseTime start
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
    | SetRange Posix ( Maybe Posix, Maybe Posix )
    | SetHour StartOrEnd Int
    | SetMinute StartOrEnd Int
    | Close


validRuntimeOrNothing : Maybe Posix -> Maybe Posix -> Maybe ( Posix, Posix )
validRuntimeOrNothing start end =
    Maybe.map2
        (\s e ->
            if Time.posixToMillis s < Time.posixToMillis e then
                Just ( s, e )

            else
                Nothing
        )
        start
        end
        |> Maybe.withDefault Nothing


update : Msg -> DatePicker -> ( DatePicker, Maybe ( Posix, Posix ) )
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
                NextMonth ->
                    ( DatePicker { model | pickerOffset = model.pickerOffset + 1 }, Nothing )

                PrevMonth ->
                    ( DatePicker { model | pickerOffset = model.pickerOffset - 1 }, Nothing )

                NextYear ->
                    ( DatePicker { model | pickerOffset = model.pickerOffset + 12 }, Nothing )

                PrevYear ->
                    ( DatePicker { model | pickerOffset = model.pickerOffset - 12 }, Nothing )

                SetHoveredDay time ->
                    ( DatePicker { model | hovered = Just time }, Nothing )

                ClearHoveredDay ->
                    ( DatePicker { model | hovered = Nothing }, Nothing )

                SetRange dayClicked ( start, end ) ->
                    case ( model.pickedStart, model.pickedEnd ) of
                        ( Just s, Just e ) ->
                            if Utilities.doDaysMatch e dayClicked then
                                ( DatePicker { model | pickedStart = start, pickedEnd = Nothing }, Nothing )

                            else if Utilities.doDaysMatch s dayClicked then
                                ( DatePicker { model | pickedStart = Nothing, pickedEnd = end }, Nothing )

                            else
                                ( DatePicker { model | pickedStart = Just dayClicked, pickedEnd = Nothing }, Nothing )

                        _ ->
                            let
                                runtime =
                                    validRuntimeOrNothing start end
                            in
                            ( DatePicker { model | pickedStart = start, pickedEnd = end }, runtime )

                SetHour startOrEnd hour ->
                    case startOrEnd of
                        Start ->
                            let
                                newTime =
                                    Utilities.setHourNotDay hour (Maybe.withDefault flooredBaseTime model.pickedStart)
                            in
                            ( DatePicker { model | pickedStart = Just newTime }, validRuntimeOrNothing (Just newTime) model.pickedEnd )

                        End ->
                            let
                                defaultTime =
                                    Maybe.map (Time.floor Day Time.utc) model.pickedStart |> Maybe.withDefault flooredBaseTime

                                newTime =
                                    Utilities.setHourNotDay hour (Maybe.withDefault defaultTime model.pickedEnd)
                            in
                            ( DatePicker { model | pickedEnd = Just newTime }, validRuntimeOrNothing model.pickedStart (Just newTime) )

                SetMinute startOrEnd minute ->
                    case startOrEnd of
                        Start ->
                            let
                                newTime =
                                    Utilities.setMinuteNotDay minute (Maybe.withDefault flooredBaseTime model.pickedStart)
                            in
                            ( DatePicker { model | pickedStart = Just newTime }, validRuntimeOrNothing (Just newTime) model.pickedEnd )

                        End ->
                            let
                                defaultTime =
                                    Maybe.map (Time.floor Day Time.utc) model.pickedStart |> Maybe.withDefault flooredBaseTime

                                newTime =
                                    Utilities.setMinuteNotDay minute (Maybe.withDefault defaultTime model.pickedEnd)
                            in
                            ( DatePicker { model | pickedEnd = Just newTime }, validRuntimeOrNothing model.pickedStart (Just newTime) )

                Close ->
                    ( DatePicker { model | status = Closed }, Nothing )

        Closed ->
            ( DatePicker model, Nothing )


classPrefix : String
classPrefix =
    "elm-datetimepicker-duration--"


determineDisplayDateTimes : Settings msg -> Model -> ( Maybe Posix, Maybe Posix )
determineDisplayDateTimes settings model =
    case ( model.pickedStart, model.pickedEnd ) of
        ( Nothing, Nothing ) ->
            ( model.hovered, Nothing )

        ( Just start, Nothing ) ->
            case model.hovered of
                Just hovered ->
                    if firstLessThanOrEqualsSecond start hovered then
                        ( Just start, Just hovered )

                    else
                        let
                            ( newStart, newEnd ) =
                                Utilities.switchTimes hovered start
                        in
                        ( Just newStart, Just newEnd )

                Nothing ->
                    ( Just start, Nothing )

        ( Nothing, Just end ) ->
            case model.hovered of
                Just hovered ->
                    if firstLessThanOrEqualsSecond hovered end then
                        ( Just hovered, Just end )

                    else
                        let
                            ( newStart, newEnd ) =
                                Utilities.switchTimes end hovered
                        in
                        ( Just newStart, Just newEnd )

                Nothing ->
                    ( Nothing, Just end )

        ( Just start, Just end ) ->
            ( Just start, Just end )


{-| The date picker view. Simply pass it the configured settings
and the date picker instance you wish to view.
-}
view : Settings msg -> DatePicker -> Html msg
view settings (DatePicker model) =
    case model.status of
        Open baseTime ->
            let
                leftViewTime =
                    Time.add Month model.pickerOffset Time.utc baseTime

                rightViewTime =
                    Time.add Month (model.pickerOffset + 1) Time.utc baseTime
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
                , div [ class (classPrefix ++ "footer-container") ] [ viewFooter settings baseTime model ]
                ]

        Closed ->
            text ""


viewPickerHeader : Settings msg -> Model -> Html msg
viewPickerHeader settings model =
    div []
        [ div [ class (classPrefix ++ "picker-header-chevrons") ]
            [ div
                [ class (classPrefix ++ "picker-header-chevron")
                , onClick <| settings.internalMsg <| update PrevMonth (DatePicker model)
                ]
                [ Icons.chevronLeft
                    |> Icons.withSize 15
                    |> Icons.toHtml []
                ]
            , div
                [ class (classPrefix ++ "picker-header-chevron")
                , onClick <| settings.internalMsg <| update NextMonth (DatePicker model)
                ]
                [ Icons.chevronRight
                    |> Icons.withSize 15
                    |> Icons.toHtml []
                ]
            ]
        , div [ class (classPrefix ++ "picker-header-chevrons") ]
            [ div
                [ class (classPrefix ++ "picker-header-chevron")
                , onClick <| settings.internalMsg <| update PrevYear (DatePicker model)
                ]
                [ Icons.chevronsLeft
                    |> Icons.withSize 15
                    |> Icons.toHtml []
                ]
            , div
                [ class (classPrefix ++ "picker-header-chevron")
                , onClick <| settings.internalMsg <| update NextYear (DatePicker model)
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
        [ viewCalendarHeader settings model viewTime
        , viewMonth settings model viewTime
        ]


viewCalendarHeader : Settings msg -> Model -> Posix -> Html msg
viewCalendarHeader settings model viewTime =
    let
        monthName =
            Time.toMonth Time.utc viewTime |> settings.formattedMonth

        year =
            Time.toYear Time.utc viewTime |> String.fromInt
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
            Maybe.map (\tday -> Utilities.doDaysMatch (Time.partsToPosix Time.utc day) tday) settings.today
                |> Maybe.withDefault False

        ( displayStart, displayEnd ) =
            determineDisplayDateTimes settings model

        ( isPicked, isBetween ) =
            Utilities.durationDayPickedOrBetween (Time.partsToPosix Time.utc day) model.hovered ( model.pickedStart, model.pickedEnd )

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
                baseAttrs ++ [ onClick <| settings.internalMsg (update (SetRange (Time.partsToPosix Time.utc day) ( displayStart, displayEnd )) (DatePicker model)) ]
    in
    div
        attrs
        [ text (String.fromInt day.day) ]


viewDateTime : Settings msg -> String -> Posix -> Html msg
viewDateTime settings classString dateTime =
    span []
        [ text (settings.dateStringFn dateTime)
        , span [ class (classPrefix ++ classString) ] [ text (settings.timeStringFn dateTime) ]
        ]


viewEmpty : Html msg
viewEmpty =
    span [] [ text "" ]


determineDateTimeViews : Settings msg -> ( Maybe Posix, Maybe Posix ) -> ( Html msg, Html msg )
determineDateTimeViews settings ( displayStart, displayEnd ) =
    case ( displayStart, displayEnd ) of
        ( Nothing, Nothing ) ->
            ( viewEmpty, viewEmpty )

        ( Just start, Nothing ) ->
            ( viewDateTime settings "selection-time" start, viewEmpty )

        ( Nothing, Just end ) ->
            ( viewEmpty, viewDateTime settings "selection-time" end )

        ( Just start, Just end ) ->
            if start == end then
                ( viewDateTime settings "selection-time" start, viewDateTime settings "selection-time-danger" end )

            else if Time.posixToMillis start > Time.posixToMillis end then
                ( viewDateTime settings "selection-time-danger" start, viewDateTime settings "selection-time" end )

            else
                ( viewDateTime settings "selection-time" start, viewDateTime settings "selection-time" end )


viewFooter : Settings msg -> Posix -> Model -> Html msg
viewFooter settings baseTime model =
    let
        ( startDate, endDate ) =
            determineDisplayDateTimes settings model

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
