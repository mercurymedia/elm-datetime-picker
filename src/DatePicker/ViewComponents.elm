module DatePicker.ViewComponents exposing (..)

import Date
import DatePicker.Icons as Icons
import DatePicker.Settings exposing (..)
import DatePicker.SingleUtilities as SingleUtilities
import DatePicker.Styles
import DatePicker.Utilities as Utilities exposing (DomLocation(..), PickerDay)
import Html exposing (Html, button, div, select, span, text)
import Html.Attributes exposing (class, classList, disabled, id, type_)
import Html.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Json.Decode as Decode
import List.Extra as List
import Svg.Attributes exposing (direction)
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra as Time exposing (Interval(..))


prefix : String
prefix =
    "elm-datetimepicker--"


classPrefix : String -> String
classPrefix class =
    prefix ++ class


{-| Container Component
-}
viewContainer : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewContainer attributes children =
    div (class (classPrefix "container") :: attributes)
        children


{-| Preset Components
-}
viewPresetsContainer : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewPresetsContainer attributes children =
    div (class (classPrefix "presets-container") :: attributes)
        children


viewPresetTab : List (Html.Attribute msg) -> { title : String, active : Bool, onClickMsg : msg } -> Html msg
viewPresetTab attributes { title, active, onClickMsg } =
    div
        ([ class (classPrefix "preset")
         , classList [ ( classPrefix "active", active ) ]
         , onClick onClickMsg
         ]
            ++ attributes
        )
        [ text title
        ]


{-| Picker Container Component
-}
viewPickerContainer : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewPickerContainer attributes children =
    div (class (classPrefix "picker-container") :: attributes)
        children


{-| Calendar Components
-}
viewCalendarContainer : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewCalendarContainer attributes children =
    div (class (classPrefix "calendar-container") :: attributes)
        children


{-| Calendar Header Components
-}
type NavDirection
    = PreviousNav
    | NextNav


type NavScale
    = YearScale
    | MonthScale


viewIconButton : List (Html.Attribute msg) -> Icons.Icon -> Html msg
viewIconButton attributes icon =
    div attributes
        [ icon
            |> Icons.withSize 15
            |> Icons.toHtml []
        ]


viewNavigationButton : { direction : NavDirection, scale : NavScale, onClickMsg : msg } -> Html msg
viewNavigationButton { direction, scale, onClickMsg } =
    let
        ( idDirection, idScale, icon ) =
            case ( direction, scale ) of
                ( PreviousNav, YearScale ) ->
                    ( "previous", "year", Icons.chevronsLeft )

                ( PreviousNav, MonthScale ) ->
                    ( "previous", "month", Icons.chevronLeft )

                ( NextNav, YearScale ) ->
                    ( "next", "year", Icons.chevronsRight )

                ( NextNav, MonthScale ) ->
                    ( "next", "month", Icons.chevronRight )
    in
    viewIconButton
        [ id (idDirection ++ "-" ++ idScale)
        , class (classPrefix "calendar-header-chevron")
        , onClick onClickMsg
        ]
        icon


viewCalendarHeaderNavigation : List (Html.Attribute msg) -> { direction : NavDirection, yearMsg : msg, monthMsg : msg } -> Html msg
viewCalendarHeaderNavigation attributes { direction, yearMsg, monthMsg } =
    let
        yearButton =
            viewNavigationButton { direction = direction, scale = YearScale, onClickMsg = yearMsg }

        monthButton =
            viewNavigationButton { direction = direction, scale = MonthScale, onClickMsg = monthMsg }

        ( classSuffix, children ) =
            case direction of
                PreviousNav ->
                    ( "previous", [ yearButton, monthButton ] )

                NextNav ->
                    ( "next", [ monthButton, yearButton ] )
    in
    div
        ([ class (classPrefix "calendar-header-navigation")
         , class (classPrefix "calendar-header-navigation--" ++ classSuffix)
         ]
            ++ attributes
        )
        children


type alias CalendarHeaderProps msg =
    { yearText : String
    , monthText : String
    , previousYearMsg : msg
    , previousMonthMsg : msg
    , nextYearMsg : msg
    , nextMonthMsg : msg
    , formattedDay : Weekday -> String
    , firstWeekDay : Weekday
    , showCalendarWeekNumbers : Bool
    }


viewCalendarHeader : CalendarHeaderProps msg -> Html msg
viewCalendarHeader { previousYearMsg, previousMonthMsg, nextYearMsg, nextMonthMsg, monthText, yearText, formattedDay, firstWeekDay, showCalendarWeekNumbers } =
    div
        [ class (classPrefix "calendar-header") ]
        [ div [ class (classPrefix "calendar-header-row") ]
            [ viewCalendarHeaderNavigation []
                { direction = PreviousNav, yearMsg = previousYearMsg, monthMsg = previousMonthMsg }
            , div
                [ class (classPrefix "calendar-header-text") ]
                [ div []
                    [ span [ id "month" ] [ text monthText ]
                    , span [] [ text " " ]
                    , span [ id "year" ] [ text yearText ]
                    ]
                ]
            , viewCalendarHeaderNavigation []
                { direction = NextNav, yearMsg = nextYearMsg, monthMsg = nextMonthMsg }
            ]
        , viewWeekHeader
            { formattedDay = formattedDay
            , firstWeekDay = firstWeekDay
            , showCalendarWeekNumbers = showCalendarWeekNumbers
            }
        ]


viewWeekHeader :
    { formattedDay : Weekday -> String
    , firstWeekDay : Weekday
    , showCalendarWeekNumbers : Bool
    }
    -> Html msg
viewWeekHeader { formattedDay, firstWeekDay, showCalendarWeekNumbers } =
    div
        [ class (classPrefix "calendar-header-week") ]
        ((if showCalendarWeekNumbers then
            div [ class (classPrefix "calendar-header-week-number") ] [ text "W" ]

          else
            text ""
         )
            :: List.map (viewHeaderDay formattedDay) (Utilities.generateListOfWeekDay firstWeekDay)
        )


viewHeaderDay : (Weekday -> String) -> Weekday -> Html msg
viewHeaderDay formatDay day =
    div
        [ class (classPrefix "calendar-header-day") ]
        [ text (formatDay day) ]


{-| Calendar Body Components
-}
type alias CalendarBodyProps msg =
    { zone : Zone
    , currentMonth : Month
    , showCalendarWeekNumbers : Bool
    , focusedDay : Maybe PickerDay
    , pickedDay : Maybe PickerDay
    , onDayClickMsg : PickerDay -> msg
    , onDayMouseOverMsg : PickerDay -> msg
    }


type alias MonthProps msg =
    { weeks : List (List PickerDay)
    , onMouseOutMsg : msg
    , calendarProps : CalendarBodyProps msg
    }


viewCalendarMonth : MonthProps msg -> Html msg
viewCalendarMonth { weeks, onMouseOutMsg, calendarProps } =
    div
        [ class (classPrefix "calendar-month"), onMouseOut onMouseOutMsg ]
        [ div []
            (List.map
                (\week ->
                    viewWeek
                        { week = week
                        , calendarProps = calendarProps
                        }
                )
                weeks
            )
        ]


type alias WeekProps msg =
    { week : List PickerDay
    , calendarProps : CalendarBodyProps msg
    }


viewWeek : WeekProps msg -> Html msg
viewWeek { week, calendarProps } =
    let
        firstDateOfWeek =
            Maybe.map
                (\day ->
                    Date.fromPosix calendarProps.zone day.start
                )
                (List.head week)

        dateWeekNumber =
            case firstDateOfWeek of
                Just date ->
                    String.fromInt (Date.weekNumber date)

                Nothing ->
                    ""
    in
    div [ class (classPrefix "calendar-week") ]
        ((if calendarProps.showCalendarWeekNumbers then
            div [ class (classPrefix "calendar-week-number") ]
                [ text dateWeekNumber ]

          else
            text ""
         )
            :: List.map
                (\day ->
                    viewDay { day = day, calendarProps = calendarProps }
                )
                week
        )


type alias DayProps msg =
    { day : PickerDay
    , calendarProps : CalendarBodyProps msg
    }


viewDay : DayProps msg -> Html msg
viewDay { day, calendarProps } =
    let
        dayParts =
            Time.posixToParts calendarProps.zone day.start

        isFocused =
            Maybe.map (\fday -> fday == day) calendarProps.focusedDay
                |> Maybe.withDefault False

        isPicked =
            Maybe.map (\pday -> pday == day) calendarProps.pickedDay
                |> Maybe.withDefault False

        dayClasses =
            DatePicker.Styles.singleDayClasses prefix
                (dayParts.month /= calendarProps.currentMonth)
                day.disabled
                isPicked
                isFocused
    in
    button
        [ type_ "button"
        , disabled day.disabled
        , class dayClasses
        , onClick (calendarProps.onDayClickMsg day)
        , onMouseOver (calendarProps.onDayMouseOverMsg day)
        ]
        [ text (String.fromInt dayParts.day) ]


{-| Footer Components
-}
viewFooterContainer : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewFooterContainer attributes children =
    div (class (classPrefix "footer") :: attributes)
        children


viewEmpty : Html msg
viewEmpty =
    div [ class (classPrefix "footer-empty") ]
        [ text "––.––.––––" ]


viewDate : String -> Html msg
viewDate dateTimeString =
    span [ class (classPrefix "selection-date") ]
        [ Icons.calendar
            |> Icons.withSize 16
            |> Icons.toHtml []
        , text dateTimeString
        ]


type alias FooterBodyProps msg =
    { timePickerProps : TimePickerProps msg
    , isTimePickerVisible : Bool
    , timePickerVisibility : TimePickerVisibility
    , selection : Posix
    , onTimePickerToggleMsg : msg
    , dateTimeString : String
    }


viewFooterBody : FooterBodyProps msg -> Html msg
viewFooterBody { timePickerProps, isTimePickerVisible, timePickerVisibility, selection, onTimePickerToggleMsg, dateTimeString } =
    div [ class (classPrefix "footer-datetime-container") ]
        [ viewDate dateTimeString
        , case timePickerVisibility of
            NeverVisible ->
                text ""

            Toggleable timePickerSettings ->
                let
                    ( viewToggleView, toggleIcon ) =
                        if isTimePickerVisible then
                            ( viewTimePicker timePickerProps
                            , Icons.check
                            )

                        else
                            ( text (timePickerSettings.timeStringFn timePickerProps.zone selection), Icons.edit )
                in
                div [ class (classPrefix "selection-time") ]
                    [ Icons.clock
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    , viewToggleView
                    , div [ class (classPrefix "time-picker-toggle"), onClick onTimePickerToggleMsg ]
                        [ toggleIcon
                            |> Icons.withSize 16
                            |> Icons.toHtml []
                        ]
                    ]

            AlwaysVisible _ ->
                div [ class (classPrefix "selection-time") ]
                    [ Icons.clock
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                    , viewTimePicker timePickerProps
                    ]
        ]


type alias TimePickerProps msg =
    { zone : Zone
    , baseDay : PickerDay
    , selectionTuple : Maybe ( PickerDay, Posix )
    , onHourChangeDecoder : Decode.Decoder msg
    , onMinuteChangeDecoder : Decode.Decoder msg
    }


viewTimePicker : TimePickerProps msg -> Html msg
viewTimePicker { zone, baseDay, selectionTuple, onHourChangeDecoder, onMinuteChangeDecoder } =
    let
        { selectableHours, selectableMinutes } =
            SingleUtilities.filterSelectableTimes zone baseDay selectionTuple
    in
    div
        [ class (classPrefix "time-picker") ]
        [ div [ class (classPrefix "select-container") ]
            -- Eventually we would like to use onInput instead of a custom on "change".
            --
            -- It will be easier to reason through. However, at the moment, a few browsers are not compatible
            -- with that behaviour. See: https://caniuse.com/#search=oninput
            [ viewSelect [ id "hour-select", on "change" onHourChangeDecoder ]
                (Utilities.generateHourOptions zone selectionTuple selectableHours)
            , div [ class (classPrefix "select-spacer") ] [ text ":" ]
            , viewSelect
                [ id "minute-select", on "change" onMinuteChangeDecoder ]
                (Utilities.generateMinuteOptions zone selectionTuple selectableMinutes)
            ]
        ]


viewSelect : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewSelect attributes content =
    div [ class (classPrefix "select") ]
        [ select attributes content
        , Icons.chevronDown
            |> Icons.withSize 16
            |> Icons.toHtml []
        ]
