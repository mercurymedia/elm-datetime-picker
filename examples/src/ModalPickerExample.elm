module ModalPickerExample exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import SingleDatePicker exposing (Settings, TimePickerVisibility(..), defaultSettings, defaultTimePickerSettings)
import Task
import Time exposing (Month(..), Posix, Zone)
import Time.Extra as TimeExtra exposing (Interval(..))


type Msg
    = OpenPicker
    | UpdatePicker SingleDatePicker.Msg
    | AdjustTimeZone Zone
    | Tick Posix
    | OnViewportChange
    | ToggleModal
    | NoOp


type alias Model =
    { currentTime : Posix
    , zone : Zone
    , pickedTime : Maybe Posix
    , picker : SingleDatePicker.DatePicker Msg
    , modalOpen : Bool
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        pickerSettings =
            userDefinedDatePickerSettings model.zone model.currentTime
    in
    case msg of
        OpenPicker ->
            let
                ( newPicker, cmd ) =
                    SingleDatePicker.openPickerOutsideHierarchy "my-button" pickerSettings model.currentTime model.pickedTime model.picker
            in
            ( { model | picker = newPicker }, cmd )

        UpdatePicker subMsg ->
            let
                ( newPicker, maybeNewTime ) =
                    SingleDatePicker.update pickerSettings subMsg model.picker
            in
            ( { model | picker = newPicker, pickedTime = Maybe.map (\t -> Just t) maybeNewTime |> Maybe.withDefault model.pickedTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )

        OnViewportChange ->
            ( model, SingleDatePicker.updatePickerPosition model.picker )

        ToggleModal ->
            ( { model | modalOpen = not model.modalOpen }, SingleDatePicker.updatePickerPosition model.picker )

        NoOp ->
            ( model, Cmd.none )


isDateBeforeToday : Posix -> Posix -> Bool
isDateBeforeToday today datetime =
    Time.posixToMillis today > Time.posixToMillis datetime


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
    }


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "height" "100vh"
        , style "padding" "3rem"
        , style "position" "relative"
        ]
        [ h1 [ style "margin-bottom" "1rem" ] [ text "Modal Example" ]
        , div []
            [ div [ style "margin-bottom" "1rem" ]
                [ text "This is a basic picker in a scrollable modal" ]
            , div [ style "margin-bottom" "1rem" ]
                [ button [ onClick <| ToggleModal ]
                    [ text "Open Modal" ]
                ]
            ]
        , if model.modalOpen then
            viewModal model

          else
            text ""
        ]


viewModal : Model -> Html Msg
viewModal model =
    div
        [ style "position" "fixed"
        , style "left" "0"
        , style "top" "0"
        , style "width" "100%"
        , style "height" "100%"
        , style "background-color" "rgba(0,0,0,0.25)"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ div
            [ style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            , style "width" "100%"
            , style "height" "100%"
            , onClick <| ToggleModal
            ]
            []
        , div
            [ style "width" "600px"
            , style "height" "auto"
            , style "max-height" "300px"
            , style "background-color" "white"
            , style "overflow" "auto"
            , style "border-radius" "5px"
            , style "position" "relative"
            , Html.Events.on "scroll" (Decode.succeed OnViewportChange)
            ]
            [ div [ style "padding" "3rem" ]
                [ button [ id "my-button", style "margin-bottom" "1rem", onClick <| OpenPicker ]
                    [ text "Open Picker" ]
                , div [ style "margin-bottom" "1rem" ]
                    [ case model.pickedTime of
                        Just date ->
                            text (posixToDateString model.zone date ++ " " ++ posixToTimeString model.zone date)

                        Nothing ->
                            text "No date selected yet!"
                    ]
                , div [ style "margin-bottom" "1rem" ]
                    [ div [] [ text "This is just some text indicating overflow:" ]
                    , div [] [ text "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet." ]
                    ]
                ]
            ]
        , SingleDatePicker.view (userDefinedDatePickerSettings model.zone model.currentTime) model.picker
        ]


init : ( Model, Cmd Msg )
init =
    ( { currentTime = Time.millisToPosix 0
      , zone = Time.utc
      , pickedTime = Nothing
      , picker = SingleDatePicker.init UpdatePicker
      , modalOpen = False
      }
    , Task.perform AdjustTimeZone Time.here
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ SingleDatePicker.subscriptions (userDefinedDatePickerSettings model.zone model.currentTime) model.picker
        , Time.every 1000 Tick
        , Browser.Events.onResize (\_ _ -> OnViewportChange)
        ]



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
            TimeExtra.posixToParts clientZone datetimeBeingProcessed

        todayPartsInClientZone =
            TimeExtra.posixToParts clientZone today

        startPartsAdjustedForBaseZone =
            TimeExtra.posixToParts baseZone datetimeBeingProcessed
                |> (\parts -> TimeExtra.partsToPosix baseZone { parts | hour = 8, minute = 0 })
                |> TimeExtra.posixToParts clientZone

        endPartsAdjustedForBaseZone =
            TimeExtra.posixToParts baseZone datetimeBeingProcessed
                |> (\parts -> TimeExtra.partsToPosix baseZone { parts | hour = 17, minute = 30 })
                |> TimeExtra.posixToParts clientZone

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
