module DatePicker.ViewComponents exposing (..)

import Css
import Date
import DatePicker.Icons as Icons
import DatePicker.Settings exposing (Theme, TimePickerVisibility(..))
import DatePicker.Utilities as Utilities exposing (DomLocation(..), PickerDay)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, classList, css, disabled, id, type_)
import Html.Styled.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Json.Decode as Decode
import List.Extra as List
import Svg.Attributes exposing (direction, display, overflow)
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
viewContainer : Theme -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
viewContainer theme attributes children =
    Html.Styled.div
        (css
            [ Css.backgroundColor theme.color.background.container
            , Css.zIndex (Css.int theme.zIndex)
            , Css.color theme.color.text.primary
            , Css.fontSize theme.fontSize.base
            , Css.boxShadow5
                theme.boxShadow.offsetX
                theme.boxShadow.offsetY
                theme.boxShadow.blurRadius
                theme.boxShadow.spreadRadius
                theme.boxShadow.color
            , Css.overflow Css.hidden
            , Css.displayFlex
            , Css.borderRadius (Css.px 4)
            , Css.position Css.absolute
            ]
            :: attributes
        )
        children


{-| Preset Components
-}
viewPresetsContainer : Theme -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
viewPresetsContainer theme attributes children =
    div (class (classPrefix "presets-container") :: attributes)
        children


viewPresetTab : Theme -> List (Html.Styled.Attribute msg) -> { title : String, active : Bool, onClickMsg : msg } -> Html msg
viewPresetTab theme attributes { title, active, onClickMsg } =
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
viewPickerContainer : Theme -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
viewPickerContainer theme attributes children =
    div (class (classPrefix "picker-container") :: attributes)
        children


{-| Calendar Components
-}
viewCalendarContainer : Theme -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
viewCalendarContainer theme attributes children =
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


viewIconButton : Theme -> List (Html.Styled.Attribute msg) -> Icons.Icon -> Html msg
viewIconButton theme attributes icon =
    div attributes
        [ icon
            |> Icons.withSize 15
            |> Icons.toHtml []
            |> fromUnstyled
        ]


viewNavigationButton : Theme -> { direction : NavDirection, scale : NavScale, onClickMsg : msg } -> Html msg
viewNavigationButton theme { direction, scale, onClickMsg } =
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
    viewIconButton theme
        [ id (idDirection ++ "-" ++ idScale)
        , class (classPrefix "calendar-header-chevron")
        , onClick onClickMsg
        ]
        icon


viewCalendarHeaderNavigation : Theme -> List (Html.Styled.Attribute msg) -> { direction : NavDirection, yearMsg : msg, monthMsg : msg } -> Html msg
viewCalendarHeaderNavigation theme attributes { direction, yearMsg, monthMsg } =
    let
        yearButton =
            viewNavigationButton theme { direction = direction, scale = YearScale, onClickMsg = yearMsg }

        monthButton =
            viewNavigationButton theme { direction = direction, scale = MonthScale, onClickMsg = monthMsg }

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


viewCalendarHeader : Theme -> CalendarHeaderProps msg -> Html msg
viewCalendarHeader theme { previousYearMsg, previousMonthMsg, nextYearMsg, nextMonthMsg, monthText, yearText, formattedDay, firstWeekDay, showCalendarWeekNumbers } =
    div
        [ class (classPrefix "calendar-header") ]
        [ div [ class (classPrefix "calendar-header-row") ]
            [ viewCalendarHeaderNavigation theme
                []
                { direction = PreviousNav, yearMsg = previousYearMsg, monthMsg = previousMonthMsg }
            , div
                [ class (classPrefix "calendar-header-text") ]
                [ div []
                    [ span [ id "month" ] [ text monthText ]
                    , span [] [ text " " ]
                    , span [ id "year" ] [ text yearText ]
                    ]
                ]
            , viewCalendarHeaderNavigation theme
                []
                { direction = NextNav, yearMsg = nextYearMsg, monthMsg = nextMonthMsg }
            ]
        , viewWeekHeader theme
            { formattedDay = formattedDay
            , firstWeekDay = firstWeekDay
            , showCalendarWeekNumbers = showCalendarWeekNumbers
            }
        ]


viewWeekHeader :
    Theme
    ->
        { formattedDay : Weekday -> String
        , firstWeekDay : Weekday
        , showCalendarWeekNumbers : Bool
        }
    -> Html msg
viewWeekHeader theme { formattedDay, firstWeekDay, showCalendarWeekNumbers } =
    div
        [ class (classPrefix "calendar-header-week") ]
        ((if showCalendarWeekNumbers then
            div [ class (classPrefix "calendar-header-week-number") ] [ text "W" ]

          else
            text ""
         )
            :: List.map (viewHeaderDay theme formattedDay) (Utilities.generateListOfWeekDay firstWeekDay)
        )


viewHeaderDay : Theme -> (Weekday -> String) -> Weekday -> Html msg
viewHeaderDay theme formatDay day =
    div
        [ class (classPrefix "calendar-header-day") ]
        [ text (formatDay day) ]


{-| Calendar Body Components
-}
type alias CalendarMonthProps msg =
    { zone : Zone
    , showCalendarWeekNumbers : Bool
    , weeks : List (List PickerDay)
    , onMouseOutMsg : msg
    , dayProps : DayProps msg
    }


type alias CalendarWeekProps msg =
    { zone : Zone
    , showCalendarWeekNumbers : Bool
    , dayProps : DayProps msg
    , week : List PickerDay
    }


type alias CalendarDayProps msg =
    { zone : Zone
    , day : PickerDay
    , dayProps : DayProps msg
    }


type alias DayProps msg =
    { onDayClickMsg : PickerDay -> msg
    , onDayMouseOverMsg : PickerDay -> msg
    , dayClassesFn : PickerDay -> String
    }


viewCalendarMonth : Theme -> CalendarMonthProps msg -> Html msg
viewCalendarMonth theme { weeks, onMouseOutMsg, dayProps, showCalendarWeekNumbers, zone } =
    div
        [ class (classPrefix "calendar-month"), onMouseOut onMouseOutMsg ]
        [ div []
            (List.map
                (\week ->
                    viewCalendarWeek theme
                        { week = week
                        , dayProps = dayProps
                        , showCalendarWeekNumbers = showCalendarWeekNumbers
                        , zone = zone
                        }
                )
                weeks
            )
        ]


viewCalendarWeek : Theme -> CalendarWeekProps msg -> Html msg
viewCalendarWeek theme { week, zone, showCalendarWeekNumbers, dayProps } =
    let
        firstDateOfWeek =
            Maybe.map
                (\day ->
                    Date.fromPosix zone day.start
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
        ((if showCalendarWeekNumbers then
            div [ class (classPrefix "calendar-week-number") ]
                [ text dateWeekNumber ]

          else
            text ""
         )
            :: List.map
                (\day ->
                    viewDay theme { day = day, zone = zone, dayProps = dayProps }
                )
                week
        )


viewDay : Theme -> CalendarDayProps msg -> Html msg
viewDay theme { zone, dayProps, day } =
    let
        dayParts =
            Time.posixToParts zone day.start
    in
    button
        [ type_ "button"
        , disabled day.disabled
        , class (dayProps.dayClassesFn day)
        , onClick (dayProps.onDayClickMsg day)
        , onMouseOver (dayProps.onDayMouseOverMsg day)
        ]
        [ text (String.fromInt dayParts.day) ]


{-| Footer Components
-}
viewFooterContainer : Theme -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
viewFooterContainer theme attributes children =
    div (class (classPrefix "footer") :: attributes)
        children


viewEmpty : Theme -> Html msg
viewEmpty theme =
    div [ class (classPrefix "footer-empty") ]
        [ text "––.––.––––" ]


viewDate : Theme -> String -> Html msg
viewDate theme dateTimeString =
    span [ class (classPrefix "selection-date") ]
        [ Icons.calendar
            |> Icons.withSize 16
            |> Icons.toHtml []
            |> fromUnstyled
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


viewFooterBody : Theme -> FooterBodyProps msg -> Html msg
viewFooterBody theme { timePickerProps, isTimePickerVisible, timePickerVisibility, selection, onTimePickerToggleMsg, dateTimeString } =
    div [ class (classPrefix "footer-datetime-container") ]
        [ viewDate theme dateTimeString
        , case timePickerVisibility of
            NeverVisible ->
                text ""

            Toggleable timePickerSettings ->
                let
                    ( viewToggleView, toggleIcon ) =
                        if isTimePickerVisible then
                            ( viewTimePicker theme timePickerProps
                            , Icons.check
                            )

                        else
                            ( text (timePickerSettings.timeStringFn timePickerProps.zone selection), Icons.edit )
                in
                div [ class (classPrefix "selection-time") ]
                    [ Icons.clock
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                        |> fromUnstyled
                    , viewToggleView
                    , div [ class (classPrefix "time-picker-toggle"), onClick onTimePickerToggleMsg ]
                        [ toggleIcon
                            |> Icons.withSize 16
                            |> Icons.toHtml []
                            |> fromUnstyled
                        ]
                    ]

            AlwaysVisible _ ->
                div [ class (classPrefix "selection-time") ]
                    [ Icons.clock
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                        |> fromUnstyled
                    , viewTimePicker theme timePickerProps
                    ]
        ]


type alias TimePickerProps msg =
    { zone : Zone
    , selectionTuple : Maybe ( PickerDay, Posix )
    , onHourChangeDecoder : Decode.Decoder msg
    , onMinuteChangeDecoder : Decode.Decoder msg
    , selectableHours : List Int
    , selectableMinutes : List Int
    }


viewTimePicker : Theme -> TimePickerProps msg -> Html msg
viewTimePicker theme { zone, selectionTuple, onHourChangeDecoder, onMinuteChangeDecoder, selectableHours, selectableMinutes } =
    div
        [ class (classPrefix "time-picker") ]
        [ div [ class (classPrefix "select-container") ]
            -- Eventually we would like to use onInput instead of a custom on "change".
            --
            -- It will be easier to reason through. However, at the moment, a few browsers are not compatible
            -- with that behaviour. See: https://caniuse.com/#search=oninput
            [ viewSelect theme
                [ id "hour-select", on "change" onHourChangeDecoder ]
                (Utilities.generateHourOptions zone selectionTuple selectableHours)
            , div [ class (classPrefix "select-spacer") ] [ text ":" ]
            , viewSelect theme
                [ id "minute-select", on "change" onMinuteChangeDecoder ]
                (Utilities.generateMinuteOptions zone selectionTuple selectableMinutes)
            ]
        ]


viewSelect : Theme -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
viewSelect theme attributes content =
    div [ class (classPrefix "select") ]
        [ select attributes content
        , Icons.chevronDown
            |> Icons.withSize 16
            |> Icons.toHtml []
            |> fromUnstyled
        ]
