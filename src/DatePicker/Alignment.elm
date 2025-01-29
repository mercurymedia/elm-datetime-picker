module DatePicker.Alignment exposing (..)

import Browser.Dom as Dom
import Css
import DatePicker.Theme as Theme exposing (Theme)
import Task exposing (Task)
import Task.Extra as TaskExtra


type Alignment
    = Alignment
        { placement : ( PlacementX, PlacementY )
        , trigger : Element
        , picker : Element
        }


type alias Position =
    { x : Float, y : Float }


type alias Element =
    { id : String, x : Float, y : Float, width : Float, height : Float }


type PlacementX
    = Left
    | Right
    | Center


type PlacementY
    = Top
    | Bottom


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


init : { triggerId : String, pickerId : String } -> (Result Dom.Error Alignment -> msg) -> Cmd msg
init { triggerId, pickerId } handleResponse =
    Task.attempt handleResponse
        (getElements triggerId pickerId)


update : (Result Dom.Error Alignment -> msg) -> Alignment -> Cmd msg
update handleResponse (Alignment { trigger, picker }) =
    init { pickerId = picker.id, triggerId = trigger.id } handleResponse


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


calcDateInputWidth : Theme.Theme -> Bool -> Float
calcDateInputWidth theme showCalendarWeekNumbers =
    calendarWidth theme showCalendarWeekNumbers


calcDurationDateInputWidth : Theme.Theme -> Bool -> Float
calcDurationDateInputWidth theme showCalendarWeekNumbers =
    2 * calendarWidth theme showCalendarWeekNumbers + 2 * theme.spacing.base


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


gridAreaPresets : String
gridAreaPresets =
    "presets"


gridAreaDateInput : String
gridAreaDateInput =
    "input"


gridAreaCalendar : String
gridAreaCalendar =
    "calendar"


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
