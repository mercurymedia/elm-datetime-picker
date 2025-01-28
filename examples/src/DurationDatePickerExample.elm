module DurationDatePickerExample exposing (Model, Msg, init, subscriptions, update, view)

import DatePicker.Settings exposing (Preset(..), Settings, TimePickerVisibility(..), defaultSettings, defaultTimePickerSettings)
import DurationDatePicker
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Task
import Time exposing (Month(..), Posix, Zone)
import Time.Extra as TimeExtra exposing (Interval(..))
import Utilities exposing (adjustAllowedTimesOfDayToClientZone, isDateBeforeToday, posixToDateString, posixToTimeString)


type Msg
    = OpenDetachedPicker String
    | UpdateDetachedPicker DurationDatePicker.Msg
    | UpdateDateInputPicker DurationDatePicker.Msg
    | AdjustTimeZone Zone
    | Tick Posix


type alias Model =
    { currentTime : Posix
    , zone : Zone
    , detachedPickerStart : Maybe Posix
    , detachedPickerEnd : Maybe Posix
    , detachedPicker : DurationDatePicker.DatePicker Msg
    , dateInputPickerStart : Maybe Posix
    , dateInputPickerEnd : Maybe Posix
    , dateInputPicker : DurationDatePicker.DatePicker Msg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenDetachedPicker elementId ->
            let
                ( newPicker, cmd ) =
                    DurationDatePicker.openPicker elementId (userDefinedDatePickerSettings model.zone model.currentTime) model.currentTime model.detachedPickerStart model.detachedPickerEnd model.detachedPicker
            in
            ( { model | detachedPicker = newPicker }, cmd )

        UpdateDetachedPicker subMsg ->
            let
                ( ( newPicker, maybeRuntime ), cmd ) =
                    DurationDatePicker.update (userDefinedDatePickerSettings model.zone model.currentTime) subMsg model.detachedPicker

                ( startTime, endTime ) =
                    Maybe.map (\( start, end ) -> ( Just start, Just end )) maybeRuntime |> Maybe.withDefault ( Nothing, Nothing )
            in
            ( { model | detachedPicker = newPicker, detachedPickerStart = startTime, detachedPickerEnd = endTime }, cmd )

        UpdateDateInputPicker subMsg ->
            let
                ( ( newPicker, maybeRuntime ), cmd ) =
                    DurationDatePicker.update (userDefinedDatePickerSettings model.zone model.currentTime) subMsg model.dateInputPicker

                ( startTime, endTime ) =
                    Maybe.map (\( start, end ) -> ( Just start, Just end )) maybeRuntime |> Maybe.withDefault ( Nothing, Nothing )
            in
            ( { model | dateInputPicker = newPicker, dateInputPickerStart = startTime, dateInputPickerEnd = endTime }, cmd )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )


userDefinedDatePickerSettings : Zone -> Posix -> Settings
userDefinedDatePickerSettings zone today =
    let
        defaults =
            defaultSettings zone
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
        , presets =
            [ PresetRange
                { title = "Today"
                , range =
                    { start = TimeExtra.floor Day zone today
                    , end = TimeExtra.floor Day zone today
                    }
                }
            , PresetRange
                { title = "This month"
                , range =
                    { start = TimeExtra.floor Month zone today
                    , end =
                        TimeExtra.floor Month zone today
                            |> TimeExtra.add Month 1 zone
                            |> TimeExtra.add Day -1 zone
                    }
                }
            , PresetRange
                { title = "Next month"
                , range =
                    { start =
                        TimeExtra.floor Month zone today
                            |> TimeExtra.add Month 1 zone
                    , end =
                        TimeExtra.floor Month zone today
                            |> TimeExtra.add Month 2 zone
                            |> TimeExtra.add Day -1 zone
                    }
                }
            , PresetRange
                { title = "Next 2 months"
                , range =
                    { start =
                        TimeExtra.floor Month zone today
                            |> TimeExtra.add Month 1 zone
                    , end =
                        TimeExtra.floor Month zone today
                            |> TimeExtra.add Month 3 zone
                            |> TimeExtra.add Day -1 zone
                    }
                }
            ]
    }


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "height" "100vh"
        , style "padding" "3rem"
        ]
        [ h1 [ style "margin-bottom" "1rem" ] [ text "DurationDatePicker Example" ]
        , div [ style "margin-bottom" "1rem" ]
            [ div [ style "margin-bottom" "1rem", style "position" "relative" ]
                [ div [ style "margin-bottom" "1rem" ]
                    [ text "This is a duration input picker" ]
                , DurationDatePicker.viewDurationInput []
                    (userDefinedDatePickerSettings model.zone model.currentTime)
                    model.currentTime
                    model.dateInputPickerStart
                    model.dateInputPickerEnd
                    model.dateInputPicker
                ]
            , div [ style "margin-bottom" "1rem" ]
                [ text "This is a duration picker" ]
            , div [ style "display" "flex", style "gap" "1rem", style "align-items" "center" ]
                [ div [ style "position" "relative" ]
                    [ button [ id "my-button", onClick <| OpenDetachedPicker "my-button" ]
                        [ text "Open Picker" ]
                    , DurationDatePicker.view (userDefinedDatePickerSettings model.zone model.currentTime) model.detachedPicker
                    ]
                , Maybe.map2
                    (\start end -> text (posixToDateString model.zone start ++ " " ++ posixToTimeString model.zone start ++ " - " ++ posixToDateString model.zone end ++ " " ++ posixToTimeString model.zone end))
                    model.detachedPickerStart
                    model.detachedPickerEnd
                    |> Maybe.withDefault (text "No date selected yet!")
                ]
            ]
        ]


init : ( Model, Cmd Msg )
init =
    ( { currentTime = Time.millisToPosix 0
      , zone = Time.utc
      , detachedPickerStart = Nothing
      , detachedPickerEnd = Nothing
      , detachedPicker = DurationDatePicker.init UpdateDetachedPicker
      , dateInputPickerStart = Nothing
      , dateInputPickerEnd = Nothing
      , dateInputPicker = DurationDatePicker.init UpdateDateInputPicker
      }
    , Task.perform AdjustTimeZone Time.here
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ DurationDatePicker.subscriptions (userDefinedDatePickerSettings model.zone model.currentTime) model.detachedPicker
        , DurationDatePicker.subscriptions (userDefinedDatePickerSettings model.zone model.currentTime) model.dateInputPicker
        , Time.every 1000 Tick
        ]
