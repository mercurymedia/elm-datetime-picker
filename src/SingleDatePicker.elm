module SingleDatePicker exposing
    ( DatePicker, init, view, subscriptions
    , Settings, defaultSettings
    , openPicker, closePicker
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
import Html exposing (Html, button, div, select, text)
import Html.Attributes exposing (attribute, class, id, type_)
import Html.Events exposing (on, onClick)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Decode
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..))
import Time.Extra as Time exposing (Interval(..), Parts)


type alias Model =
    { status : Status
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
    , today : Maybe Posix
    , dayDisabled : Posix -> Bool
    , internalMsg : DatePicker -> msg
    , selectedMsg : Posix -> msg
    }


type Status
    = Closed
    | Open Posix


type alias MsgConfig msg =
    { internalMsg : DatePicker -> msg
    , externalMsg : Posix -> msg
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
        , viewOffset = 0
        , pickedTime = Nothing
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


type Msg
    = NextMonth
    | PrevMonth
    | NextYear
    | PrevYear
    | SetDay Posix
    | SetHour Int
    | SetMinute Int
    | Close


update : Msg -> DatePicker -> DatePicker
update msg (DatePicker model) =
    case model.status of
        Open baseTime ->
            case msg of
                NextMonth ->
                    DatePicker { model | viewOffset = model.viewOffset + 1 }

                PrevMonth ->
                    DatePicker { model | viewOffset = model.viewOffset - 1 }

                NextYear ->
                    DatePicker { model | viewOffset = model.viewOffset + 12 }

                PrevYear ->
                    DatePicker { model | viewOffset = model.viewOffset - 12 }

                SetDay time ->
                    DatePicker { model | pickedTime = Just <| Utilities.setDayNotTime time (Maybe.withDefault (Time.floor Day Time.utc time) model.pickedTime) }

                SetHour hour ->
                    DatePicker { model | pickedTime = Just <| Utilities.setHourNotDay hour (Maybe.withDefault (Time.floor Day Time.utc baseTime) model.pickedTime) }

                SetMinute minute ->
                    DatePicker { model | pickedTime = Just <| Utilities.setMinuteNotDay minute (Maybe.withDefault (Time.floor Day Time.utc baseTime) model.pickedTime) }

                Close ->
                    DatePicker { model | status = Closed }

        Closed ->
            DatePicker model


classPrefix : String
classPrefix =
    "elm-datetimepicker-single--"


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
                , viewFooter settings model model.pickedTime
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
                , onClick <| settings.internalMsg <| update PrevMonth (DatePicker model)
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
                , onClick <| settings.internalMsg <| update NextMonth (DatePicker model)
                ]
                [ Icons.chevronRight
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            ]
        , div [ class (classPrefix ++ "calendar-header-row") ]
            [ div
                [ class (classPrefix ++ "calendar-header-chevron")
                , onClick <| settings.internalMsg <| update PrevYear (DatePicker model)
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
                , onClick <| settings.internalMsg <| update NextYear (DatePicker model)
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
        [ class (classPrefix ++ "calendar-month") ]
        [ div [] (List.map (viewWeek settings currentMonth pickedTime model) weeks)
        ]


viewWeek : Settings msg -> Month -> Maybe Posix -> Model -> List Parts -> Html msg
viewWeek settings currentMonth pickedTime model week =
    div [ class (classPrefix ++ "calendar-week") ]
        (List.map (viewDay settings model currentMonth pickedTime) week)


viewDay : Settings msg -> Model -> Month -> Maybe Posix -> Parts -> Html msg
viewDay settings model currentMonth pickedTime day =
    let
        isToday =
            Maybe.map (\tday -> Utilities.doDaysMatch day (Time.posixToParts Time.utc tday)) settings.today
                |> Maybe.withDefault False

        isPicked =
            case pickedTime of
                Just time ->
                    Utilities.doDaysMatch day (Time.posixToParts Time.utc time)

                Nothing ->
                    False

        isDisabled =
            settings.dayDisabled (Time.partsToPosix Time.utc day)

        dayClasses =
            DatePicker.Styles.singleDayClasses classPrefix (day.month /= currentMonth) isDisabled isPicked isToday

        attrs =
            if isDisabled then
                [ class dayClasses ]

            else
                [ class dayClasses, onClick <| settings.internalMsg (update (SetDay (Time.partsToPosix Time.utc day)) (DatePicker model)) ]
    in
    div
        attrs
        [ text (String.fromInt day.day) ]


viewFooter : Settings msg -> Model -> Maybe Posix -> Html msg
viewFooter settings model pickedTime =
    div
        [ class (classPrefix ++ "footer") ]
        [ viewTimePicker settings model pickedTime
        , div [ class (classPrefix ++ "confirm-button-container") ] [ viewConfirmButton settings pickedTime ]
        ]


viewConfirmButton : Settings msg -> Maybe Posix -> Html msg
viewConfirmButton settings pickedTime =
    let
        ( classStr, confirmAction ) =
            Maybe.map (\picked -> ( classPrefix ++ "confirm-button", [ onClick <| settings.selectedMsg picked ] )) pickedTime
                |> Maybe.withDefault ( classPrefix ++ "confirm-button " ++ classPrefix ++ "disabled", [] )

        confirmAttrs =
            [ class classStr, type_ "button", attribute "aria-label" "confirm" ] ++ confirmAction
    in
    button confirmAttrs
        [ Icons.check
            |> Icons.withSize 16
            |> Icons.toHtml []
        ]


viewTimePicker : Settings msg -> Model -> Maybe Posix -> Html msg
viewTimePicker settings model pickedTime =
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
            [ div [ class (classPrefix ++ "select") ] [ select [ on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update msg (DatePicker model)) (Decode.map SetHour targetValueIntParse))) ] (Utilities.generateHourOptions hour) ]
            , div [ class (classPrefix ++ "select-spacer") ] [ text ":" ]
            , div [ class (classPrefix ++ "select") ] [ select [ on "change" (Decode.map settings.internalMsg (Decode.map (\msg -> update msg (DatePicker model)) (Decode.map SetMinute targetValueIntParse))) ] (Utilities.generateMinuteOptions minute) ]
            ]
        ]
