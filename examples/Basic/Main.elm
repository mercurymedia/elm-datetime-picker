module DatePickerExample.Basic.Main exposing (main)

import Browser
import DatePicker.Utilities as Utilities
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import SingleDatePicker exposing (Settings)
import Time exposing (Month(..), Posix)
import Time.Extra as Time exposing (Interval(..))


type Msg
    = OpenPicker
    | UpdatePicker (SingleDatePicker.DatePicker Msg)


type alias Model =
    { today : Posix
    , picker : SingleDatePicker.DatePicker Msg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenPicker ->
            ( { model | picker = SingleDatePicker.open model.picker }, Cmd.none )

        UpdatePicker newPicker ->
            ( { model | picker = newPicker }, Cmd.none )


isDateBeforeToday : Posix -> Posix -> Bool
isDateBeforeToday today datetime =
    Time.posixToMillis today > Time.posixToMillis datetime


userDefinedDatePickerSettings : Posix -> Settings Msg
userDefinedDatePickerSettings today =
    let
        defaults =
            SingleDatePicker.defaultSettings today UpdatePicker
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
                , SingleDatePicker.view model.picker
                ]
            , Maybe.map (\date -> text <| Utilities.toUtcDateTimeString date) (SingleDatePicker.getPickedTime model.picker) |> Maybe.withDefault (text "No date selected yet!")
            ]
        ]


init : Int -> ( Model, Cmd Msg )
init currentTime =
    let
        today =
            Time.millisToPosix currentTime
    in
    ( { today = today
      , picker = SingleDatePicker.init (userDefinedDatePickerSettings today) Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    SingleDatePicker.subscriptions model.picker


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
