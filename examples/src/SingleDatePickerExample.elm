module SingleDatePickerExample exposing (Model, Msg, init, subscriptions, update, view)

import Css
import DatePicker.DateInput as DateInput
import DatePicker.Settings as Settings
    exposing
        ( Settings
        , TimePickerVisibility(..)
        , defaultSettings
        , defaultTimePickerSettings
        )
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import SingleDatePicker
import Task
import Time exposing (Month(..), Posix, Zone)
import Time.Extra as TimeExtra exposing (Interval(..))
import Utilities exposing (adjustAllowedTimesOfDayToClientZone, isDateBeforeToday, posixToDateString, posixToTimeString)


type Msg
    = OpenDetachedPicker String
    | UpdateDateInputPicker SingleDatePicker.Msg
    | UpdateDetachedPicker SingleDatePicker.Msg
    | AdjustTimeZone Zone
    | Tick Posix


type alias Model =
    { currentTime : Posix
    , zone : Zone
    , dateInputPickerTime : Maybe Posix
    , dateInputPicker : SingleDatePicker.DatePicker Msg
    , detachedPickerTime : Maybe Posix
    , detachedPicker : SingleDatePicker.DatePicker Msg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenDetachedPicker elementId ->
            let
                ( newPicker, cmd ) =
                    SingleDatePicker.openPicker elementId
                        (userDefinedDatePickerSettings model.zone model.currentTime)
                        model.currentTime
                        model.detachedPickerTime
                        model.detachedPicker
            in
            ( { model | detachedPicker = newPicker }, cmd )

        UpdateDetachedPicker subMsg ->
            let
                ( ( newPicker, maybeNewTime ), cmd ) =
                    SingleDatePicker.update (userDefinedDatePickerSettings model.zone model.currentTime) subMsg model.detachedPicker
            in
            ( { model | detachedPicker = newPicker, detachedPickerTime = maybeNewTime }, cmd )

        UpdateDateInputPicker subMsg ->
            let
                ( ( newPicker, maybeNewTime ), cmd ) =
                    SingleDatePicker.update (userDefinedDatePickerSettings model.zone model.currentTime) subMsg model.dateInputPicker
            in
            ( { model | dateInputPicker = newPicker, dateInputPickerTime = maybeNewTime }, cmd )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )


userDefinedDatePickerSettings : Zone -> Posix -> Settings
userDefinedDatePickerSettings zone today =
    let
        defaults =
            defaultSettings zone

        allowedTimesOfDay =
            \clientZone datetime -> adjustAllowedTimesOfDayToClientZone Time.utc clientZone today datetime

        dateFormat =
            DateInput.defaultDateFormat

        timeFormat =
            DateInput.defaultTimeFormat

        dateInputSettings =
            { format = DateInput.DateTime dateFormat { timeFormat | allowedTimesOfDay = allowedTimesOfDay }, getErrorMessage = getErrorMessage }
    in
    { defaults
        | isDayDisabled = \clientZone datetime -> isDateBeforeToday (TimeExtra.floor Day clientZone today) datetime
        , focusedDate = Just today
        , dateStringFn = posixToDateString
        , timePickerVisibility =
            Toggleable
                { defaultTimePickerSettings
                    | timeStringFn = posixToTimeString
                    , allowedTimesOfDay = \clientZone datetime -> adjustAllowedTimesOfDayToClientZone Time.utc clientZone today datetime
                }
        , showCalendarWeekNumbers = True
        , presets = [ Settings.PresetDate { title = "Preset", date = today } ]

        -- , presets = []
        , dateInputSettings = dateInputSettings
    }


getErrorMessage : DateInput.InputError -> String
getErrorMessage error =
    case error of
        DateInput.ValueInvalid ->
            "Invalid value. Make sure to use the correct format."

        DateInput.ValueNotAllowed ->
            "Date not allowed."


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "height" "100vh"
        , style "padding" "3rem"
        ]
        [ h1 [ style "margin-bottom" "1rem" ] [ text "SingleDatePicker Example" ]
        , div []
            [ div [ style "margin-bottom" "1rem" ]
                [ text "This is the picker rendered with a date input" ]
            , div [ style "position" "relative", style "margin-bottom" "1rem", style "width" "250px" ]
                [ SingleDatePicker.viewDateInput []
                    (userDefinedDatePickerSettings model.zone model.currentTime)
                    model.currentTime
                    model.dateInputPickerTime
                    model.dateInputPicker
                ]
            , div [ style "margin-bottom" "1rem" ]
                [ text "This is the detached picker rendered on click of a button" ]
            , div [ style "position" "relative", style "margin-bottom" "1rem", style "display" "flex", style "gap" "1rem", style "align-items" "center" ]
                [ button [ id "my-button", onClick (OpenDetachedPicker "my-button") ] [ text "Open the picker here" ]
                , SingleDatePicker.view
                    (userDefinedDatePickerSettings model.zone model.currentTime)
                    model.detachedPicker
                , div []
                    [ text "Picked time: "
                    , case model.detachedPickerTime of
                        Just t ->
                            text (Utilities.posixToDateString model.zone t ++ " " ++ Utilities.posixToTimeString model.zone t)

                        Nothing ->
                            text "No date selected yet!"
                    ]
                ]
            ]
        ]


init : ( Model, Cmd Msg )
init =
    ( { currentTime = Time.millisToPosix 0
      , zone = Time.utc
      , dateInputPickerTime = Nothing
      , dateInputPicker = SingleDatePicker.init UpdateDateInputPicker
      , detachedPickerTime = Nothing
      , detachedPicker = SingleDatePicker.init UpdateDetachedPicker
      }
    , Task.perform AdjustTimeZone Time.here
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ SingleDatePicker.subscriptions (userDefinedDatePickerSettings model.zone model.currentTime) model.dateInputPicker
        , SingleDatePicker.subscriptions (userDefinedDatePickerSettings model.zone model.currentTime) model.detachedPicker
        , Time.every 1000 Tick
        ]
