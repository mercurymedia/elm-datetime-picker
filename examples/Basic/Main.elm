module DatePickerExample.Basic.Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import SingleDatePicker exposing (Settings, defaultSettings)
import Task
import Time exposing (Month(..), Posix, Zone)
import Time.Extra as Time exposing (Interval(..))


type Msg
    = OpenPicker
    | UpdatePicker ( SingleDatePicker.DatePicker, Maybe Posix )
    | AdjustTimeZone Zone
    | Tick Posix


type alias Model =
    { currentTime : Posix
    , zone : Zone
    , pickedTime : Maybe Posix
    , picker : SingleDatePicker.DatePicker
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenPicker ->
            ( { model | picker = SingleDatePicker.openPicker model.zone model.currentTime model.pickedTime model.picker }, Cmd.none )

        UpdatePicker ( newPicker, maybeNewTime ) ->
            ( { model | picker = newPicker, pickedTime = Maybe.map (\t -> Just t) maybeNewTime |> Maybe.withDefault model.pickedTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )


isDateBeforeToday : Posix -> Posix -> Bool
isDateBeforeToday today datetime =
    Time.posixToMillis today > Time.posixToMillis datetime


userDefinedDatePickerSettings : Zone -> Posix -> Settings Msg
userDefinedDatePickerSettings zone today =
    let
        defaults =
            defaultSettings zone UpdatePicker
    in
    { defaults
        | dateTimeProcessor =
            { isDayDisabled = \clientZone datetime -> isDateBeforeToday (Time.floor Day clientZone today) datetime
            , allowedTimesOfDay =
                \clientZone datetime -> adjustAllowedTimesOfDayToClientZone Time.utc clientZone today datetime
            }
        , focusedDate = Just today
        , dateStringFn = posixToDateString
        , timeStringFn = posixToTimeString
        , isFooterDisabled = True
    }


view : Model -> Html Msg
view model =
    div [ style "width" "100vw", style "height" "100vh", style "padding" "3rem" ]
        [ div [ style "padding-bottom" "1rem" ] [ text "This is a basic picker" ]
        , div
            [ style "width" "500px", style "display" "inline-flex" ]
            [ div []
                [ button [ style "margin-right" "10px", onClick <| OpenPicker ] [ text "Picker" ]
                , SingleDatePicker.view (userDefinedDatePickerSettings model.zone model.currentTime) model.picker
                ]
            , case model.pickedTime of
                Just date ->
                    text (posixToDateString model.zone date ++ " " ++ posixToTimeString model.zone date)

                Nothing ->
                    text "No date selected yet!"
            ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentTime = Time.millisToPosix 0
      , zone = Time.utc
      , pickedTime = Nothing
      , picker = SingleDatePicker.init
      }
    , Task.perform AdjustTimeZone Time.here
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ SingleDatePicker.subscriptions (userDefinedDatePickerSettings model.zone model.currentTime) UpdatePicker model.picker
        , Time.every 1000 Tick
        ]


main : Program () Model Msg
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


{-| The goal of this naive function is to adjust
the allowed time boundaries within the baseZone
to the time zone in which the picker is running
(clientZone) for the current day being processed
(datetime).

For example, the allowed times of day could be
9am - 5pm EST. However, if someone is using the
picker in MST (2 hours behind EST), the allowed
times of day displayed in the picker should be
7am - 3pm.

There is likely a better way to do this, but it
is suitable as an example.

-}
adjustAllowedTimesOfDayToClientZone : Zone -> Zone -> Posix -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }
adjustAllowedTimesOfDayToClientZone baseZone clientZone today datetimeBeingProcessed =
    let
        processingPartsInClientZone =
            Time.posixToParts clientZone datetimeBeingProcessed

        todayPartsInClientZone =
            Time.posixToParts clientZone today

        startPartsAdjustedForBaseZone =
            Time.posixToParts baseZone datetimeBeingProcessed
                |> (\parts -> Time.partsToPosix baseZone { parts | hour = 8, minute = 0 })
                |> Time.posixToParts clientZone

        endPartsAdjustedForBaseZone =
            Time.posixToParts baseZone datetimeBeingProcessed
                |> (\parts -> Time.partsToPosix baseZone { parts | hour = 17, minute = 30 })
                |> Time.posixToParts clientZone

        bounds =
            { startHour = startPartsAdjustedForBaseZone.hour
            , startMinute = startPartsAdjustedForBaseZone.minute
            , endHour = endPartsAdjustedForBaseZone.hour
            , endMinute = endPartsAdjustedForBaseZone.minute
            }
    in
    if processingPartsInClientZone.day == todayPartsInClientZone.day && processingPartsInClientZone.month == todayPartsInClientZone.month && processingPartsInClientZone.year == todayPartsInClientZone.year then
        if todayPartsInClientZone.hour > bounds.startHour || (todayPartsInClientZone.hour == bounds.startHour && todayPartsInClientZone.minute > bounds.startMinute) then
            { startHour = todayPartsInClientZone.hour, startMinute = todayPartsInClientZone.minute, endHour = bounds.endHour, endMinute = bounds.endMinute }

        else
            bounds

    else
        bounds


posixToDateString : Zone -> Posix -> String
posixToDateString zone date =
    addLeadingZero (Time.toDay zone date)
        ++ "."
        ++ monthToNmbString (Time.toMonth zone date)
        ++ "."
        ++ addLeadingZero (Time.toYear zone date)


posixToTimeString : Zone -> Posix -> String
posixToTimeString zone datetime =
    addLeadingZero (Time.toHour zone datetime)
        ++ ":"
        ++ addLeadingZero (Time.toMinute zone datetime)
        ++ ":"
        ++ addLeadingZero (Time.toSecond zone datetime)
