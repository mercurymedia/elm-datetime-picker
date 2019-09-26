module SingleDatePicker exposing
    ( DatePicker, init, view, subscriptions
    , Settings, defaultSettings
    , getPickedTime, isOpen
    , open, close, setPickedTime
    )

{-| A date picker component for a single datetime.


# Architecture

@docs DatePicker, init, view, subscriptions


# Settings

@docs Settings, defaultSettings


# Query

@docs getPickedTime, isOpen


# Externally Triggered Actions

@docs open, close, setPickedTime

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


type alias Model msg =
    { settings : Settings msg
    , status : Status
    , viewOffset : Int
    , stagedTime : Maybe Posix
    , pickedTime : Maybe Posix
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


{-| Instantiates and returns a date picker. Takes in the configured picker
settings as well as the picked time, if any.
-}
init : Settings msg -> Maybe Posix -> DatePicker msg
init settings pickedTime =
    DatePicker
        { settings = settings
        , status = Closed <| Time.floor Day Time.utc settings.baseTime
        , viewOffset = 0
        , stagedTime = Nothing
        , pickedTime = pickedTime
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


{-| Retrieve the picked time, if any, from the provided date picker.
-}
getPickedTime : DatePicker msg -> Maybe Posix
getPickedTime (DatePicker model) =
    model.pickedTime


{-| Open the provided date picker and receive the updated picker instance.
-}
open : DatePicker msg -> DatePicker msg
open (DatePicker model) =
    DatePicker { model | status = Open model.settings.baseTime, stagedTime = model.pickedTime, viewOffset = calculatePickerOffset (Time.floor Day Time.utc model.settings.baseTime) model.pickedTime }


{-| Close the provided date picker and receive the updated picker instance.
-}
close : DatePicker msg -> DatePicker msg
close (DatePicker model) =
    DatePicker { model | status = Closed model.settings.baseTime, stagedTime = Nothing }


{-| It is possible that the picker is instantiated on your model before data representing
a previously picked datetime is loaded. Use this function to update the picked datetime in
the picker when the appropriate data is received. Use this method sparingly, if at all.
The picked state should ideally only change due to user input.
-}
setPickedTime : Posix -> DatePicker msg -> DatePicker msg
setPickedTime datetime (DatePicker model) =
    DatePicker { model | pickedTime = Just datetime }


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
    = NextMonth
    | PrevMonth
    | NextYear
    | PrevYear
    | SetDay Posix
    | SetHour Int
    | SetMinute Int
    | ConfirmDateTime Posix
    | Close


update : Msg -> Model msg -> DatePicker msg
update msg model =
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
                    DatePicker { model | stagedTime = Just <| Utilities.setDayNotTime time (Maybe.withDefault (Time.floor Day Time.utc time) model.stagedTime) }

                SetHour hour ->
                    DatePicker { model | stagedTime = Just <| Utilities.setHourNotDay hour (Maybe.withDefault (Time.floor Day Time.utc baseTime) model.stagedTime) }

                SetMinute minute ->
                    DatePicker { model | stagedTime = Just <| Utilities.setMinuteNotDay minute (Maybe.withDefault (Time.floor Day Time.utc baseTime) model.stagedTime) }

                ConfirmDateTime confirmed ->
                    DatePicker { model | pickedTime = Just confirmed } |> close

                Close ->
                    close <| DatePicker model

        Closed _ ->
            DatePicker model


classPrefix : String
classPrefix =
    "elm-datetimepicker-single--"


{-| The date picker view. Simply pass it the date picker instance you wish to view.
-}
view : DatePicker msg -> Html msg
view (DatePicker model) =
    case model.status of
        Open baseTime ->
            let
                offsetTime =
                    Time.add Month model.viewOffset Time.utc baseTime
            in
            div
                [ id datePickerId, class (classPrefix ++ "picker-container") ]
                [ div [ class (classPrefix ++ "calendar-container") ]
                    [ viewCalendarHeader model offsetTime
                    , viewMonth model offsetTime
                    ]
                , viewFooter model
                ]

        Closed _ ->
            text ""


viewCalendarHeader : Model msg -> Posix -> Html msg
viewCalendarHeader model time =
    let
        monthName =
            Time.toMonth Time.utc time |> model.settings.formattedMonth

        year =
            Time.toYear Time.utc time |> String.fromInt
    in
    div
        [ class (classPrefix ++ "calendar-header") ]
        [ div [ class (classPrefix ++ "calendar-header-row") ]
            [ div
                [ class (classPrefix ++ "calendar-header-chevron")
                , onClick <| model.settings.internalMsg <| update PrevMonth model
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
                , onClick <| model.settings.internalMsg <| update NextMonth model
                ]
                [ Icons.chevronRight
                    |> Icons.withSize 12
                    |> Icons.toHtml []
                ]
            ]
        , div [ class (classPrefix ++ "calendar-header-row") ]
            [ div
                [ class (classPrefix ++ "calendar-header-chevron")
                , onClick <| model.settings.internalMsg <| update PrevYear model
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
                , onClick <| model.settings.internalMsg <| update NextYear model
                ]
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
        [ class (classPrefix ++ "calendar-month") ]
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

        isPicked =
            case model.stagedTime of
                Just time ->
                    Utilities.doDaysMatch day (Time.posixToParts Time.utc time)

                Nothing ->
                    False

        isDisabled =
            model.settings.dayDisabled (Time.partsToPosix Time.utc day)

        dayClasses =
            DatePicker.Styles.singleDayClasses classPrefix (day.month /= currentMonth) isDisabled isPicked isToday

        attrs =
            if isDisabled then
                [ class dayClasses ]

            else
                [ class dayClasses, onClick <| model.settings.internalMsg (update (SetDay (Time.partsToPosix Time.utc day)) model) ]
    in
    div
        attrs
        [ text (String.fromInt day.day) ]


viewFooter : Model msg -> Html msg
viewFooter model =
    div
        [ class (classPrefix ++ "footer") ]
        [ viewTimePicker model
        , div [ class (classPrefix ++ "confirm-button-container") ] [ viewConfirmButton model ]
        ]


viewConfirmButton : Model msg -> Html msg
viewConfirmButton model =
    let
        ( classStr, confirmAction ) =
            Maybe.map (\staged -> ( classPrefix ++ "confirm-button", [ onClick <| model.settings.internalMsg (update (ConfirmDateTime staged) model) ] )) model.stagedTime
                |> Maybe.withDefault ( classPrefix ++ "confirm-button " ++ classPrefix ++ "disabled", [] )

        confirmAttrs =
            [ class classStr, type_ "button", attribute "aria-label" "confirm" ] ++ confirmAction
    in
    button confirmAttrs
        [ Icons.check
            |> Icons.withSize 16
            |> Icons.toHtml []
        ]


viewTimePicker : Model msg -> Html msg
viewTimePicker model =
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
                model.stagedTime
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
            [ div [ class (classPrefix ++ "select") ] [ select [ on "change" (Decode.map model.settings.internalMsg (Decode.map (\msg -> update msg model) (Decode.map SetHour targetValueIntParse))) ] (Utilities.generateHourOptions hour) ]
            , div [ class (classPrefix ++ "select-spacer") ] [ text ":" ]
            , div [ class (classPrefix ++ "select") ] [ select [ on "change" (Decode.map model.settings.internalMsg (Decode.map (\msg -> update msg model) (Decode.map SetMinute targetValueIntParse))) ] (Utilities.generateMinuteOptions minute) ]
            ]
        ]
