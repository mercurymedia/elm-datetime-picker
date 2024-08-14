module DatePicker.ViewComponents exposing (durationDayStyles, durationStartOrEndStyles, singleDayStyles, viewCalendarContainer, viewCalendarHeader, viewCalendarMonth, viewContainer, viewEmpty, viewFooterBody, viewFooterContainer, viewPickerContainer, viewPresetTab, viewPresetsContainer)

import Css
import Css.Transitions
import Date
import DatePicker.Icons as Icons
import DatePicker.Settings exposing (Theme, TimePickerVisibility(..))
import DatePicker.Utilities as Utilities exposing (DomLocation(..), PickerDay)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, disabled, id, type_)
import Html.Styled.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Json.Decode as Decode
import List.Extra as List
import Svg.Attributes exposing (direction)
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra as Time exposing (Interval(..))


{-| ViewComponent functions for both Pickers.


# View Components

@docs durationDayStyles, durationStartOrEndStyles, singleDayStyles, viewCalendarContainer, viewCalendarHeader, viewCalendarMonth, viewContainer, viewEmpty, viewFooterBody, viewFooterContainer, viewPickerContainer, viewPresetTab, viewPresetsContainer

-}
styleList : List ( List Css.Style, Bool ) -> Css.Style
styleList list =
    list
        |> List.filter Tuple.second
        |> List.concatMap Tuple.first
        |> Css.batch


colorsTransition : Theme -> Css.Style
colorsTransition theme =
    Css.Transitions.transition
        [ Css.Transitions.backgroundColor3 theme.transition.duration 0 (Css.Transitions.cubicBezier 0.4 0 0.2 1)
        , Css.Transitions.color3 theme.transition.duration 0 (Css.Transitions.cubicBezier 0.4 0 0.2 1)
        ]


dayStyles :
    Theme
    ->
        { common : List Css.Style
        , hidden : List Css.Style
        , disabled : List Css.Style
        , picked : List Css.Style
        , today : List Css.Style
        }
dayStyles theme =
    { common =
        [ Css.height theme.size.day
        , Css.width theme.size.day
        , Css.position Css.relative
        , Css.fontSize theme.fontSize.xs
        , Css.display Css.inlineFlex
        , Css.justifyContent Css.center
        , Css.alignItems Css.center
        , Css.cursor Css.pointer
        , Css.outline Css.none
        , Css.padding (Css.px 0)
        , colorsTransition theme
        , Css.border3 theme.borderWidth Css.solid Css.transparent
        , Css.hover [ Css.backgroundColor theme.color.action.hover ]
        , Css.borderRadius theme.borderRadius.lg
        , Css.backgroundColor theme.color.background.container
        ]
    , hidden = [ Css.visibility Css.hidden ]
    , disabled =
        [ Css.pointerEvents Css.none
        , Css.property "filter" "grayscale(1)"
        , Css.color theme.color.text.disabled
        , Css.after
            [ Css.position Css.absolute
            , Css.width theme.fontSize.xs
            , Css.height (Css.px 1)
            , Css.backgroundColor Css.currentColor
            , Css.property "content" "''"
            , Css.display Css.block
            ]
        ]
    , picked =
        [ Css.backgroundColor theme.color.primary.main
        , Css.color theme.color.primary.contrastText
        , Css.borderRadius theme.borderRadius.lg
        , Css.hover
            [ Css.backgroundColor theme.color.primary.main
            , Css.color theme.color.primary.contrastText
            ]
        ]
    , today = [ Css.border3 theme.borderWidth Css.solid theme.color.text.disabled ]
    }


{-| Utilities
-}
durationDayStyles : Theme -> Bool -> Bool -> Bool -> Bool -> Bool -> Css.Style
durationDayStyles theme isHidden isDisabled isPicked isToday isBetween =
    let
        { common, today, picked, disabled, hidden } =
            dayStyles theme

        between =
            [ Css.backgroundColor theme.color.primary.light
            , Css.borderRadius (Css.px 0)
            ]
    in
    Css.batch
        [ Css.batch common
        , styleList
            [ ( today, isToday )
            , ( between, isBetween )
            , ( picked, isPicked )
            , ( disabled, isDisabled )
            , ( hidden, isHidden )
            ]
        ]


durationStartOrEndStyles : Theme -> Bool -> Bool -> Css.Style
durationStartOrEndStyles theme isStart isEnd =
    let
        startStyles =
            Css.batch
                [ Css.borderTopRightRadius (Css.px 0)
                , Css.borderBottomRightRadius (Css.px 0)
                ]

        endStyles =
            Css.batch
                [ Css.borderTopLeftRadius (Css.px 0)
                , Css.borderBottomLeftRadius (Css.px 0)
                ]
    in
    if isStart && not isEnd then
        startStyles

    else if isEnd && not isStart then
        endStyles

    else
        Css.batch []


singleDayStyles : Theme -> Bool -> Bool -> Bool -> Bool -> Css.Style
singleDayStyles theme isHidden isDisabled isPicked isToday =
    let
        { common, today, picked, disabled, hidden } =
            dayStyles theme
    in
    Css.batch
        [ Css.batch common
        , styleList
            [ ( today, isToday )
            , ( picked, isPicked )
            , ( disabled, isDisabled )
            , ( hidden, isHidden )
            ]
        ]


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
            , Css.borderRadius theme.borderRadius.lg
            , Css.overflow Css.hidden
            , Css.displayFlex
            , Css.position Css.absolute
            ]
            :: attributes
        )
        children


{-| Preset Components
-}
viewPresetsContainer : Theme -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
viewPresetsContainer theme attributes children =
    div
        (css
            [ Css.padding (Css.rem 0.75)
            , Css.borderRight3 theme.borderWidth Css.solid theme.color.border
            , Css.backgroundColor theme.color.background.presets
            , Css.width theme.size.presetsContainer
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.flexShrink (Css.int 0)
            ]
            :: attributes
        )
        children


viewPresetTab : Theme -> List (Html.Styled.Attribute msg) -> { title : String, active : Bool, onClickMsg : msg } -> Html msg
viewPresetTab theme attributes { title, active, onClickMsg } =
    let
        defaultStyles =
            [ Css.padding2 (Css.rem 0.65) (Css.rem 0.75)
            , Css.lineHeight (Css.num 1.25)
            , Css.cursor Css.pointer
            , Css.borderRadius theme.borderRadius.base
            , Css.fontSize theme.fontSize.sm
            , colorsTransition theme
            , Css.hover [ Css.backgroundColor theme.color.action.hover ]
            ]

        activeStyles =
            [ Css.color theme.color.primary.contrastText
            , Css.backgroundColor theme.color.primary.main
            , Css.hover [ Css.backgroundColor theme.color.primary.main ]
            , Css.cursor Css.default
            ]

        styles =
            if active then
                defaultStyles ++ activeStyles

            else
                defaultStyles
    in
    div
        ([ css styles
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
    div
        (css
            [ Css.flexShrink (Css.num 0)
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceBetween
            ]
            :: attributes
        )
        children


{-| Calendar Components
-}
viewCalendarContainer : Theme -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
viewCalendarContainer theme attributes children =
    div (css [ Css.padding (Css.rem 1) ] :: attributes)
        children


{-| Calendar Header Components
-}
type NavDirection
    = PreviousNav
    | NextNav


type NavScale
    = YearScale
    | MonthScale


type IconButtonVariant
    = PrimaryIconButton
    | SecondaryIconButton


viewIconButton : Theme -> List (Html.Styled.Attribute msg) -> { icon : Icons.Icon, variant : IconButtonVariant } -> Html msg
viewIconButton theme attributes { icon, variant } =
    let
        variantStyles =
            case variant of
                PrimaryIconButton ->
                    [ Css.color theme.color.primary.main
                    , Css.hover [ Css.backgroundColor theme.color.action.hover ]
                    ]

                SecondaryIconButton ->
                    [ Css.color theme.color.text.secondary
                    , Css.hover [ Css.backgroundColor theme.color.action.hover ]
                    ]
    in
    div
        (css
            [ Css.display Css.inlineFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.width theme.size.iconButton
            , Css.height theme.size.iconButton
            , Css.borderRadius (Css.pct 50)
            , Css.cursor Css.pointer
            , colorsTransition theme
            , Css.batch variantStyles
            ]
            :: attributes
        )
        [ icon
            |> Icons.withSize 16
            |> Icons.toHtml []
            |> fromUnstyled
        ]


viewNavigationButton : Theme -> { direction : NavDirection, scale : NavScale, onClickMsg : Maybe msg } -> Html msg
viewNavigationButton theme { direction, scale, onClickMsg } =
    let
        icon =
            case ( direction, scale ) of
                ( PreviousNav, YearScale ) ->
                    Icons.chevronsLeft

                ( PreviousNav, MonthScale ) ->
                    Icons.chevronLeft

                ( NextNav, YearScale ) ->
                    Icons.chevronsRight

                ( NextNav, MonthScale ) ->
                    Icons.chevronRight

        attrs =
            case onClickMsg of
                Just msg ->
                    [ onClick msg ]

                Nothing ->
                    [ css [ Css.visibility Css.hidden ] ]
    in
    viewIconButton theme
        attrs
        { icon = icon, variant = SecondaryIconButton }


viewCalendarHeaderNavigation : Theme -> List (Html.Styled.Attribute msg) -> { direction : NavDirection, yearMsg : Maybe msg, monthMsg : Maybe msg } -> Html msg
viewCalendarHeaderNavigation theme attributes { direction, yearMsg, monthMsg } =
    let
        yearButton =
            viewNavigationButton theme { direction = direction, scale = YearScale, onClickMsg = yearMsg }

        monthButton =
            viewNavigationButton theme { direction = direction, scale = MonthScale, onClickMsg = monthMsg }

        children =
            case direction of
                PreviousNav ->
                    [ yearButton, monthButton ]

                NextNav ->
                    [ monthButton, yearButton ]
    in
    div ([] ++ attributes)
        children


type alias CalendarHeaderProps msg =
    { yearText : String
    , monthText : String
    , previousYearMsg : Maybe msg
    , previousMonthMsg : Maybe msg
    , nextYearMsg : Maybe msg
    , nextMonthMsg : Maybe msg
    , formattedDay : Weekday -> String
    , firstWeekDay : Weekday
    , showCalendarWeekNumbers : Bool
    }


viewCalendarHeader : Theme -> CalendarHeaderProps msg -> Html msg
viewCalendarHeader theme { previousYearMsg, previousMonthMsg, nextYearMsg, nextMonthMsg, monthText, yearText, formattedDay, firstWeekDay, showCalendarWeekNumbers } =
    div
        [ css
            [ Css.fontWeight Css.bold
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "0.5rem"
            ]
        ]
        [ div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.spaceBetween
                ]
            ]
            [ viewCalendarHeaderNavigation theme
                []
                { direction = PreviousNav, yearMsg = previousYearMsg, monthMsg = previousMonthMsg }
            , div
                [ css
                    [ Css.display Css.inlineFlex
                    , Css.alignItems Css.center
                    , Css.property "user-select" "none"
                    ]
                ]
                [ div []
                    [ span [] [ text monthText ]
                    , span [] [ text " " ]
                    , span [] [ text yearText ]
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
        [ css
            [ Css.displayFlex
            , Css.color theme.color.text.secondary
            , Css.fontSize theme.fontSize.xxs
            , Css.fontWeight Css.normal
            ]
        ]
        ((if showCalendarWeekNumbers then
            viewHeaderDay theme [ text "#" ]

          else
            text ""
         )
            :: List.map
                (\day ->
                    viewHeaderDay theme
                        [ text (formattedDay day) ]
                )
                (Utilities.generateListOfWeekDay firstWeekDay)
        )


viewHeaderDay : Theme -> List (Html msg) -> Html msg
viewHeaderDay theme children =
    div
        [ css
            [ Css.display Css.inlineFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.width theme.size.day
            , Css.height theme.size.day
            ]
        ]
        children


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
    , dayStylesFn : PickerDay -> Css.Style
    }


viewCalendarMonth : Theme -> CalendarMonthProps msg -> Html msg
viewCalendarMonth theme { weeks, onMouseOutMsg, dayProps, showCalendarWeekNumbers, zone } =
    div
        [ onMouseOut onMouseOutMsg ]
        [ div [ css [ Css.displayFlex, Css.flexDirection Css.column, Css.property "gap" "2px" ] ]
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
    div [ css [ Css.displayFlex ] ]
        ((if showCalendarWeekNumbers then
            div
                [ css
                    [ Css.width theme.size.day
                    , Css.height theme.size.day
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.justifyContent Css.center
                    , Css.fontSize theme.fontSize.xs
                    , Css.color theme.color.text.secondary
                    , Css.opacity (Css.num 0.5)
                    ]
                ]
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
        , css [ dayProps.dayStylesFn day ]
        , onClick (dayProps.onDayClickMsg day)
        , onMouseOver (dayProps.onDayMouseOverMsg day)
        ]
        [ text (String.fromInt dayParts.day) ]


{-| Footer Components
-}
viewFooterContainer : Theme -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
viewFooterContainer theme attributes children =
    div
        (css
            [ Css.width (Css.pct 100)
            , Css.backgroundColor theme.color.background.footer
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.fontSize theme.fontSize.sm
            , Css.borderTop3 theme.borderWidth Css.solid theme.color.border
            , Css.padding (Css.rem 1)
            ]
            :: attributes
        )
        children


viewEmpty : Theme -> Html msg
viewEmpty theme =
    div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.letterSpacing (Css.px 3)
            , Css.color theme.color.text.disabled
            , Css.flexGrow (Css.num 1)
            , Css.flexBasis (Css.px 0)
            , Css.minHeight theme.size.iconButton
            ]
        ]
        [ text "––.––.––––" ]


viewDate : Theme -> String -> Html msg
viewDate theme dateTimeString =
    span
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.property "gap" "0.5rem"
            , Css.minHeight theme.size.iconButton
            ]
        ]
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
    div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.property "gap" "1rem"
            , Css.flexGrow (Css.num 1)
            , Css.flexBasis (Css.px 0)
            ]
        ]
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
                div
                    [ css
                        [ Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.property "gap" "0.5rem"
                        ]
                    ]
                    [ Icons.clock
                        |> Icons.withSize 16
                        |> Icons.toHtml []
                        |> fromUnstyled
                    , viewToggleView
                    , viewIconButton theme [ onClick onTimePickerToggleMsg ] { icon = toggleIcon, variant = PrimaryIconButton }
                    ]

            AlwaysVisible _ ->
                div
                    [ css
                        [ Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.property "gap" "0.5rem"
                        ]
                    ]
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
        [ css [ Css.displayFlex, Css.justifyContent Css.spaceBetween ] ]
        [ div [ css [ Css.display Css.inlineFlex ] ]
            -- Eventually we would like to use onInput instead of a custom on "change".
            --
            -- It will be easier to reason through. However, at the moment, a few browsers are not compatible
            -- with that behaviour. See: https://caniuse.com/#search=oninput
            [ viewSelect theme
                [ id "hour-select", on "change" onHourChangeDecoder ]
                (Utilities.generateHourOptions zone selectionTuple selectableHours)
            , div
                [ css
                    [ Css.display Css.inlineFlex
                    , Css.alignItems Css.center
                    , Css.padding2 (Css.px 0) (Css.rem 0.25)
                    , Css.property "user-select" "none"
                    ]
                ]
                [ text ":" ]
            , viewSelect theme
                [ id "minute-select", on "change" onMinuteChangeDecoder ]
                (Utilities.generateMinuteOptions zone selectionTuple selectableMinutes)
            ]
        ]


viewSelect : Theme -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
viewSelect theme attributes content =
    div
        [ css
            [ Css.position Css.relative
            , Css.display Css.inlineFlex
            , Css.alignItems Css.center
            ]
        ]
        [ select
            (css
                [ Css.borderRadius theme.borderRadius.base
                , Css.height theme.size.iconButton
                , Css.outline Css.zero
                , Css.fontWeight Css.normal
                , Css.border3 theme.borderWidth Css.solid theme.color.border
                , Css.property "appearance" "none"
                , Css.padding4 (Css.px 0) (Css.px 24) (Css.px 0) (Css.px 4)
                ]
                :: attributes
            )
            content
        , span
            [ css
                [ Css.position Css.absolute
                , Css.right (Css.px 4)
                , Css.pointerEvents Css.none
                , Css.display Css.inlineFlex
                , Css.alignItems Css.center
                ]
            ]
            [ Icons.chevronDown
                |> Icons.withSize 16
                |> Icons.toHtml []
                |> fromUnstyled
            ]
        ]
