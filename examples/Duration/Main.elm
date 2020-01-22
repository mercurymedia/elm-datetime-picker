module DatePickerExample.Duration.Main exposing (main)

import Browser
import DurationDatePicker exposing (Settings, defaultSettings)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time exposing (Month(..), Posix)
import Time.Extra as Time exposing (Interval(..))


type Msg
    = OpenPicker
    | UpdatePicker ( DurationDatePicker.DatePicker, Maybe ( Posix, Posix ) )


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

        UpdatePicker ( newPicker, maybeRuntime ) ->
            let
                ( startTime, endTime ) =
                    Maybe.map (\( start, end ) -> ( Just start, Just end )) maybeRuntime |> Maybe.withDefault ( model.pickedStartTime, model.pickedEndTime )
            in
            ( { model | picker = newPicker, pickedStartTime = startTime, pickedEndTime = endTime }, Cmd.none )


isDateBeforeToday : Posix -> Posix -> Bool
isDateBeforeToday today datetime =
    Time.posixToMillis today > Time.posixToMillis datetime


userDefinedDatePickerSettings : Posix -> Settings Msg
userDefinedDatePickerSettings today =
    let
        defaults =
            defaultSettings UpdatePicker
    in
    { defaults
        | dateTimeProcessor =
            { isDayDisabled = \datetime -> isDateBeforeToday (Time.floor Day Time.utc today) datetime
            , allowedTimesOfDay =
                \datetime ->
                    let
                        processingParts =
                            Time.posixToParts Time.utc datetime

                        todayParts =
                            Time.posixToParts Time.utc today
                    in
                    if processingParts.day == todayParts.day && processingParts.month == todayParts.month && processingParts.year == todayParts.year then
                        { startHour = todayParts.hour, startMinute = todayParts.minute, endHour = 16, endMinute = 30 }

                    else
                        { startHour = 8, startMinute = 0, endHour = 16, endMinute = 30 }
            }
        , focusedDate = Just today
        , dateStringFn = posixToDateString
        , timeStringFn = posixToTimeString
    }


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
                (\start end -> text (posixToDateString start ++ " " ++ posixToTimeString start ++ " - " ++ posixToDateString end ++ " " ++ posixToTimeString end))
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
    DurationDatePicker.subscriptions (userDefinedDatePickerSettings model.today) UpdatePicker model.picker


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW UTILITIES - these are not required for the package to work, they are used here simply to format the selected dates


addLeadingZero : Int -> String
addLeadingZero value =
    let
        string =
            String.fromInt value
    in
    if String.length string == 1 then
        "0" ++ string

    else
        string


monthToNmbString : Month -> String
monthToNmbString month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


posixToDateString : Posix -> String
posixToDateString date =
    addLeadingZero (Time.toDay Time.utc date)
        ++ "."
        ++ monthToNmbString (Time.toMonth Time.utc date)
        ++ "."
        ++ addLeadingZero (Time.toYear Time.utc date)


posixToTimeString : Posix -> String
posixToTimeString datetime =
    addLeadingZero (Time.toHour Time.utc datetime)
        ++ ":"
        ++ addLeadingZero (Time.toMinute Time.utc datetime)
        ++ ":"
        ++ addLeadingZero (Time.toSecond Time.utc datetime)
        ++ " (UTC)"
