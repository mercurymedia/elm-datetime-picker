module SingleDatePickerExample exposing (Model, Msg, init, subscriptions, update, view)

import Css
import DatePicker.Settings
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
import Utilities exposing (posixToDateString, posixToTimeString, isDateBeforeToday, adjustAllowedTimesOfDayToClientZone)


type Msg
    = OpenPicker
    | UpdatePicker SingleDatePicker.Msg
    | AdjustTimeZone Zone
    | Tick Posix


type alias Model =
    { currentTime : Posix
    , zone : Zone
    , pickedTime : Maybe Posix
    , picker : SingleDatePicker.DatePicker Msg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenPicker ->
            ( { model | picker = SingleDatePicker.openPicker (userDefinedDatePickerSettings model.zone model.currentTime) model.currentTime model.pickedTime model.picker }, Cmd.none )

        UpdatePicker subMsg ->
            let
                ( ( newPicker, maybeNewTime ), cmd ) =
                    SingleDatePicker.update (userDefinedDatePickerSettings model.zone model.currentTime) subMsg model.picker
            in
            ( { model | picker = newPicker, pickedTime = maybeNewTime }, cmd )

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
        , presets = []
    }


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
                [ text "This is a basic picker" ]
            , div [ style "position" "relative", style "margin-bottom" "1rem" ]
                [ case model.pickedTime of
                    Just date ->
                        text (posixToDateString model.zone date ++ " " ++ posixToTimeString model.zone date)

                    Nothing ->
                        text "No date selected yet!"
                , div []
                    [ SingleDatePicker.viewDateInput [ onClick <| OpenPicker ]
                        (userDefinedDatePickerSettings model.zone model.currentTime)
                        model.picker
                    , SingleDatePicker.view (userDefinedDatePickerSettings model.zone model.currentTime) model.picker
                    ]
                ]
            ]
        ]


init : ( Model, Cmd Msg )
init =
    ( { currentTime = Time.millisToPosix 0
      , zone = Time.utc
      , pickedTime = Nothing
      , picker = SingleDatePicker.init UpdatePicker
      }
    , Task.perform AdjustTimeZone Time.here
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ SingleDatePicker.subscriptions (userDefinedDatePickerSettings model.zone model.currentTime) model.picker
        , Time.every 1000 Tick
        ]
