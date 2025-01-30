module DatePicker.Alignment exposing
    ( Alignment
    , init, update
    , dateInputStylesFromAlignment, calcDateInputWidth, calcDurationDateInputWidth
    , pickerStylesFromAlignment, applyPickerStyles, pickerPositionFromAlignment, pickerTranslationFromAlignment
    , gridAreaPresets, gridAreaDateInput, gridAreaCalendar
    , pickerGridLayoutFromAlignment
    )

{-| This module provides utilities for determining the alignment and layout
of a date picker component, ensuring proper positioning relative to
its trigger element and viewport constraints.

@docs Alignment
@docs init, update
@docs dateInputStylesFromAlignment, calcDateInputWidth, calcDurationDateInputWidth
@docs pickerStylesFromAlignment, applyPickerStyles, pickerPositionFromAlignment, pickerTranslationFromAlignment
@docs gridAreaPresets, gridAreaDateInput, gridAreaCalendar
@docs pickerGridLayoutFromAlignment

-}

import Browser.Dom as Dom
import Css
import DatePicker.Theme as Theme exposing (Theme)
import Task exposing (Task)
import Task.Extra as TaskExtra


{-| Represents the alignment of a date picker relative to a trigger element.
It includes information about the placement and the elements involved.
-}
type Alignment
    = Alignment
        { placement : ( PlacementX, PlacementY )
        , trigger : Element
        , picker : Element
        }


{-| Represents an HTML element with position and size details.
-}
type alias Element =
    { id : String, x : Float, y : Float, width : Float, height : Float }


{-| Horizontal placement options for the date picker.
-}
type PlacementX
    = Left
    | Right
    | Center


{-| Vertical placement options for the date picker.
-}
type PlacementY
    = Top
    | Bottom


{-| Determines the alignment of a date picker popover based on the position of the trigger element,
the picker itself, and the viewport constraints.
-}
fromElements :
    { trigger : Element
    , picker : Element
    , viewport : Element
    }
    -> Alignment
fromElements { trigger, picker, viewport } =
    let
        minOffset =
            10

        triggerLeft =
            trigger.x

        triggerRight =
            trigger.x + trigger.width

        triggerCenter =
            trigger.x + trigger.width / 2

        triggerBottom =
            trigger.y + trigger.height

        pickerWidth =
            picker.width

        pickerHeight =
            picker.height

        viewPortWidth =
            viewport.width

        viewPortHeight =
            viewport.height

        alignX =
            if (triggerLeft + pickerWidth) <= (viewPortWidth - minOffset) then
                Left

            else if (triggerRight - pickerWidth) >= minOffset then
                Right

            else if
                (triggerCenter - pickerWidth / 2)
                    >= minOffset
                    && (triggerCenter + pickerWidth / 2)
                    <= (viewPortWidth - minOffset)
            then
                Center

            else
                Left

        alignY =
            if (triggerBottom + pickerHeight) > viewPortHeight then
                Top

            else
                Bottom
    in
    Alignment
        { placement = ( alignX, alignY )
        , trigger = trigger
        , picker = picker
        }


{-| Initializes the alignment by fetching the positions of the trigger and picker elements
from the DOM. Calls `handleResponse` with the result.
-}
init : { triggerId : String, pickerId : String } -> (Result Dom.Error Alignment -> msg) -> Cmd msg
init { triggerId, pickerId } handleResponse =
    Task.attempt handleResponse
        (getElements triggerId pickerId)


{-| Updates the alignment by re-fetching the positions of the elements and recalculating alignment.
-}
update : (Result Dom.Error Alignment -> msg) -> Alignment -> Cmd msg
update handleResponse (Alignment { trigger, picker }) =
    init { pickerId = picker.id, triggerId = trigger.id } handleResponse


{-| Retrieves the DOM elements by their IDs and constructs an `Alignment` instance.
-}
getElements : String -> String -> Task Dom.Error Alignment
getElements pickerId triggerId =
    let
        elementFromDomElement : String -> { x : Float, y : Float, width : Float, height : Float } -> Element
        elementFromDomElement id domElement =
            { id = id
            , x = domElement.x
            , y = domElement.y
            , width = domElement.width
            , height = domElement.height
            }
    in
    Task.succeed
        (\trigger picker viewport ->
            fromElements
                { trigger = elementFromDomElement triggerId trigger.element
                , picker = elementFromDomElement pickerId picker.element
                , viewport = elementFromDomElement "" viewport.viewport
                }
        )
        |> TaskExtra.andMap (Dom.getElement pickerId)
        |> TaskExtra.andMap (Dom.getElement triggerId)
        |> TaskExtra.andMap Dom.getViewport


{-| Computes the styles for the date input view based on the alignment and visibility of the picker popover.
When the picker is opened, the date input view is positioned fixed on top of the picker popover, manually
integrating itself into the popover's layout.
When the picker is closed, the date input view is positioned absolute to it's container element.

The date input's width needs to be passed in order to scale it correctly into the picker's layout (when the
picker is opened, the date input should be the same width as the calendar).

-}
dateInputStylesFromAlignment : Theme.Theme -> Bool -> Float -> Maybe Alignment -> List Css.Style
dateInputStylesFromAlignment theme isPickerOpen width maybeAlignment =
    let
        closedStyles =
            [ Css.position Css.absolute
            , Css.top (Css.px 0)
            , Css.left (Css.px 0)
            , Css.zIndex (Css.int (theme.zIndex + 10))
            , Css.width (Css.pct 100)
            ]
    in
    case ( maybeAlignment, isPickerOpen ) of
        ( Just alignment, True ) ->
            let
                { x, y } =
                    fixedDateInputCoorinatesFromAlignment width alignment
            in
            [ Css.position Css.fixed
            , Css.zIndex (Css.int (theme.zIndex + 10))
            , Css.left (Css.px x)
            , Css.top (Css.px y)
            , Css.width (Css.px width)
            ]

        ( _, _ ) ->
            closedStyles


{-| Calculates the width of a single date input view element.
When the picker is opened, the date input should be the same width as the calendar
-}
calcDateInputWidth : Theme.Theme -> Bool -> Float
calcDateInputWidth theme showCalendarWeekNumbers =
    calendarWidth theme showCalendarWeekNumbers


{-| Calculates the width of two date input elements for duration picking.
When the picker is opened, each date input should be the same width as one calendar month.
So the total width is twice the width of one calendar month (plus spacing in between).
-}
calcDurationDateInputWidth : Theme.Theme -> Bool -> Float
calcDurationDateInputWidth theme showCalendarWeekNumbers =
    2 * calendarWidth theme showCalendarWeekNumbers + 2 * theme.spacing.base


{-| Computes the calendar width based on whether the week numbers are shown.
-}
calendarWidth : Theme -> Bool -> Float
calendarWidth theme showCalendarWeekNumbers =
    let
        factor =
            if showCalendarWeekNumbers then
                8

            else
                7
    in
    factor * theme.size.day


{-| Computes the fixed coordinates for the date input field based on alignment.
-}
fixedDateInputCoorinatesFromAlignment : Float -> Alignment -> { x : Float, y : Float }
fixedDateInputCoorinatesFromAlignment dateInputWidth (Alignment { placement, trigger, picker }) =
    let
        ( placementX, _ ) =
            placement

        x =
            case placementX of
                Left ->
                    trigger.x

                Center ->
                    trigger.x + trigger.width / 2 - picker.width / 2

                Right ->
                    trigger.x + trigger.width - dateInputWidth

        y =
            trigger.y
    in
    { x = x, y = y }


{-| Computes the styles for positioning the picker based on the given alignment.
-}
pickerStylesFromAlignment : Theme -> Maybe Alignment -> List Css.Style
pickerStylesFromAlignment theme maybeAlignment =
    case maybeAlignment of
        Just alignment ->
            let
                { x, y } =
                    fixedPickerCoorinatesFromAlignment alignment
            in
            [ Css.position Css.fixed
            , Css.zIndex (Css.int theme.zIndex)
            , Css.left (Css.px x)
            , Css.top (Css.px y)
            ]

        _ ->
            -- hide picker element until the DOM elements have been found
            [ Css.visibility Css.hidden ]


{-| Applies the provided styling function to an `Alignment` if available.
Otherwise, hides the picker until alignment is determined.
-}
applyPickerStyles : (Alignment -> List Css.Style) -> Maybe Alignment -> List Css.Style
applyPickerStyles stylingFn maybeAlignment =
    case maybeAlignment of
        Just alignment ->
            stylingFn alignment

        Nothing ->
            -- hide picker element until the DOM elements have been found
            [ Css.visibility Css.hidden
            , defaultGridLayout
            ]


{-| Determines the CSS fixed position styles for the picker popover based on alignment.
-}
pickerPositionFromAlignment : Theme -> Alignment -> Css.Style
pickerPositionFromAlignment theme alignment =
    let
        { x, y } =
            fixedPickerCoorinatesFromAlignment alignment
    in
    Css.batch
        [ Css.position Css.fixed
        , Css.zIndex (Css.int theme.zIndex)
        , Css.left (Css.px x)
        , Css.top (Css.px y)
        ]


{-| Computes the translation transformation for the picker container,
adjusting its position based on alignment to frame the date input element.
-}
pickerTranslationFromAlignment : Theme -> Alignment -> Css.Style
pickerTranslationFromAlignment theme (Alignment { placement }) =
    let
        ( placementX, placementY ) =
            placement

        inputHeight =
            String.fromFloat theme.size.inputElement ++ "px"

        translate : { x : String, y : String } -> Css.Style
        translate { x, y } =
            Css.property "transform" ("translateX(" ++ x ++ ") translateY(" ++ y ++ ")")
    in
    -- container translation to frame date input
    case ( placementX, placementY ) of
        ( Left, Bottom ) ->
            translate { x = "-1rem", y = "calc(-1rem - " ++ inputHeight ++ ")" }

        ( Left, Top ) ->
            translate { x = "-1rem", y = "calc(1rem + " ++ inputHeight ++ ")" }

        ( Right, Bottom ) ->
            translate { x = "1rem", y = "calc(-1rem - " ++ inputHeight ++ ")" }

        ( Right, Top ) ->
            translate { x = "1rem", y = "calc(2rem + " ++ inputHeight ++ ")" }

        ( Center, Bottom ) ->
            translate { x = "-1rem", y = "calc(-1rem - " ++ inputHeight ++ ")" }

        ( Center, Top ) ->
            translate { x = "-1rem", y = "calc(2rem + " ++ inputHeight ++ ")" }


{-| Computes the fixed coordinates for the picker based on its alignment
relative to the trigger element.
-}
fixedPickerCoorinatesFromAlignment : Alignment -> { x : Float, y : Float }
fixedPickerCoorinatesFromAlignment (Alignment { placement, trigger, picker }) =
    let
        ( placementX, placementY ) =
            placement

        x =
            case placementX of
                Left ->
                    trigger.x

                Center ->
                    trigger.x + trigger.width / 2 - picker.width / 2

                Right ->
                    trigger.x + trigger.width - picker.width

        y =
            case placementY of
                Top ->
                    trigger.y - picker.height

                Bottom ->
                    trigger.y + trigger.height
    in
    { x = x, y = y }


{-| CSS grid area name for the preset buttons section.
-}
gridAreaPresets : String
gridAreaPresets =
    "presets"


{-| CSS grid area name for the date input field.
-}
gridAreaDateInput : String
gridAreaDateInput =
    "input"


{-| CSS grid area name for the calendar section.
-}
gridAreaCalendar : String
gridAreaCalendar =
    "calendar"


{-| Generates a `grid-template` CSS style from a list of row definitions.
Each row is represented as a list of strings corresponding to grid areas.
-}
gridTemplateFromList : List (List String) -> Css.Style
gridTemplateFromList listTemplate =
    let
        template =
            String.join " "
                (List.map
                    (\row -> "'" ++ String.join " " row ++ "'")
                    listTemplate
                )
    in
    Css.property "grid-template" template


{-| Computes the grid layout for the picker based on its alignment.
Adjusts the order of the date input, calendar, and preset sections
based on whether the picker appears above or below, left or right.
-}
pickerGridLayoutFromAlignment : Alignment -> Css.Style
pickerGridLayoutFromAlignment (Alignment { placement }) =
    let
        template =
            case ( Tuple.first placement, Tuple.second placement ) of
                ( Left, Bottom ) ->
                    [ [ gridAreaDateInput, gridAreaPresets ]
                    , [ gridAreaCalendar, gridAreaPresets ]
                    ]

                ( Left, Top ) ->
                    [ [ gridAreaCalendar, gridAreaPresets ]
                    , [ gridAreaDateInput, gridAreaPresets ]
                    ]

                ( Right, Bottom ) ->
                    [ [ gridAreaPresets, gridAreaDateInput ]
                    , [ gridAreaPresets, gridAreaCalendar ]
                    ]

                ( Right, Top ) ->
                    [ [ gridAreaPresets, gridAreaCalendar ]
                    , [ gridAreaPresets, gridAreaDateInput ]
                    ]

                ( Center, Bottom ) ->
                    [ [ gridAreaDateInput, gridAreaPresets ]
                    , [ gridAreaCalendar, gridAreaPresets ]
                    ]

                ( Center, Top ) ->
                    [ [ gridAreaCalendar, gridAreaPresets ]
                    , [ gridAreaDateInput, gridAreaPresets ]
                    ]
    in
    Css.batch
        [ Css.property "display" "grid"
        , gridTemplateFromList template
        ]


{-| Default grid layout used when alignment is not yet determined.
-}
defaultGridLayout : Css.Style
defaultGridLayout =
    let
        template =
            [ [ gridAreaDateInput, gridAreaPresets ]
            , [ gridAreaCalendar, gridAreaPresets ]
            ]
    in
    Css.batch
        [ Css.property "display" "grid"
        , gridTemplateFromList template
        ]
