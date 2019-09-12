module DatePickerExample.Basic.Main exposing (main)

import Browser
import DatePicker.Utilities as Utilities
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import SingleDatePicker exposing (Settings, defaultSettings)
import Time exposing (Month(..), Posix)
import Time.Extra as Time exposing (Interval(..))


type Msg
    = OpenPicker
    | Selected Posix
    | UpdatePicker SingleDatePicker.DatePicker


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

        Selected time ->
            let
                -- Don't want to close after confirming picked date? No problem, just remove the picker update!
                newPicker =
                    SingleDatePicker.closePicker model.picker
            in
            ( { model | pickedTime = Just time, picker = newPicker }, Cmd.none )

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
                    text (Utilities.toUtcDateTimeString date)

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
