module DatePickerExample.Duration.Main exposing (main)

import Browser
import DatePicker.Utilities as Utilities
import DurationDatePicker exposing (Settings, defaultSettings)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time exposing (Month(..), Posix)
import Time.Extra as Time exposing (Interval(..))


type Msg
    = OpenPicker
    | Selected Posix Posix
    | UpdatePicker DurationDatePicker.DatePicker


type alias Model =
    { today : Posix
    , pickedStartTime : Maybe Posix
    , pickedEndTime : Maybe Posix
    , picker : DurationDatePicker.DatePicker
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenPicker ->
            ( { model | picker = DurationDatePicker.openPicker model.today model.pickedStartTime model.pickedEndTime model.picker }, Cmd.none )

        Selected startTime endTime ->
            let
                -- Don't want to close after confirming picked date? No problem, just remove the picker update!
                newPicker =
                    DurationDatePicker.closePicker model.picker
            in
            ( { model | pickedStartTime = Just startTime, pickedEndTime = Just endTime, picker = newPicker }, Cmd.none )

        UpdatePicker newPicker ->
            ( { model | picker = newPicker }, Cmd.none )


isDateBeforeToday : Posix -> Posix -> Bool
isDateBeforeToday today datetime =
    Time.posixToMillis today > Time.posixToMillis datetime


userDefinedDatePickerSettings : Posix -> Settings Msg
userDefinedDatePickerSettings today =
    let
        defaults =
            defaultSettings { internalMsg = UpdatePicker, externalMsg = Selected }
    in
    { defaults | dayDisabled = \datetime -> isDateBeforeToday (Time.floor Day Time.utc today) datetime, today = Just today }


view : Model -> Html Msg
view model =
    div [ style "width" "100vw", style "height" "100vh", style "padding" "3rem" ]
        [ div [ style "padding-bottom" "1rem" ] [ text "This is a duration picker" ]
        , div
            [ style "width" "500px", style "display" "inline-flex" ]
            [ div []
                [ button [ style "margin-right" "10px", onClick <| OpenPicker ] [ text "Picker" ]
                , DurationDatePicker.view (userDefinedDatePickerSettings model.today) model.picker
                ]
            , Maybe.map2
                (\start end -> text (Utilities.toUtcDateTimeString start ++ " - " ++ Utilities.toUtcDateTimeString end))
                model.pickedStartTime
                model.pickedEndTime
                |> Maybe.withDefault (text "No date selected yet!")
            ]
        ]


init : Int -> ( Model, Cmd Msg )
init currentTime =
    let
        today =
            Time.millisToPosix currentTime
    in
    ( { today = today
      , pickedStartTime = Nothing
      , pickedEndTime = Nothing
      , picker = DurationDatePicker.init
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    DurationDatePicker.subscriptions UpdatePicker model.picker


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
