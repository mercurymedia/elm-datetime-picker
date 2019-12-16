module DatePickerExample.Basic.Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import SingleDatePicker exposing (Settings, defaultSettings)
import Time exposing (Month(..), Posix)
import Time.Extra as Time exposing (Interval(..))


type Msg
    = OpenPicker
    | UpdatePicker ( SingleDatePicker.DatePicker, Maybe Posix )


type alias Model =
    { today : Posix
    , pickedTime : Maybe Posix
    , picker : SingleDatePicker.DatePicker
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenPicker ->
            ( { model | picker = SingleDatePicker.openPicker model.today model.pickedTime model.picker }, Cmd.none )

        UpdatePicker ( newPicker, maybeNewTime ) ->
            ( { model | picker = newPicker, pickedTime = Maybe.map (\t -> Just t) maybeNewTime |> Maybe.withDefault model.pickedTime }, Cmd.none )


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
        | dayDisabled = \datetime -> isDateBeforeToday (Time.floor Day Time.utc today) datetime
        , today = Just today
    }


view : Model -> Html Msg
view model =
    div [ style "width" "100vw", style "height" "100vh", style "padding" "3rem" ]
        [ div [ style "padding-bottom" "1rem" ] [ text "This is a basic picker" ]
        , div
            [ style "width" "500px", style "display" "inline-flex" ]
            [ div []
                [ button [ style "margin-right" "10px", onClick <| OpenPicker ] [ text "Picker" ]
                , SingleDatePicker.view (userDefinedDatePickerSettings model.today) model.picker
                ]
            , case model.pickedTime of
                Just date ->
                    text (posixToDateString date ++ " " ++ posixToTimeString date)

                Nothing ->
                    text "No date selected yet!"
            ]
        ]


init : Int -> ( Model, Cmd Msg )
init currentTime =
    let
        today =
            Time.millisToPosix currentTime
    in
    ( { today = today
      , pickedTime = Nothing
      , picker = SingleDatePicker.init
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    SingleDatePicker.subscriptions UpdatePicker model.picker


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
