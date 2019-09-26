module DatePickerExample.Duration.Main exposing (main)

import Browser
import DatePicker.Utilities as Utilities
import DurationDatePicker exposing (Settings)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time exposing (Month(..), Posix)
import Time.Extra as Time exposing (Interval(..))


type Msg
    = OpenPicker
    | UpdatePicker (DurationDatePicker.DatePicker Msg)


type alias Model =
    { today : Posix
    , picker : DurationDatePicker.DatePicker Msg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenPicker ->
            ( { model | picker = DurationDatePicker.open model.picker }, Cmd.none )

        UpdatePicker newPicker ->
            ( { model | picker = newPicker }, Cmd.none )


isDateBeforeToday : Posix -> Posix -> Bool
isDateBeforeToday today datetime =
    Time.posixToMillis today > Time.posixToMillis datetime


userDefinedDatePickerSettings : Posix -> Settings Msg
userDefinedDatePickerSettings today =
    let
        defaults =
            DurationDatePicker.defaultSettings today UpdatePicker
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
                , DurationDatePicker.view model.picker
                ]
            , Maybe.map
                (\( start, end ) -> text (Utilities.toUtcDateTimeString start ++ " - " ++ Utilities.toUtcDateTimeString end))
                (DurationDatePicker.getPickedTimes model.picker)
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
      , picker = DurationDatePicker.init (userDefinedDatePickerSettings today) Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    DurationDatePicker.subscriptions model.picker


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
