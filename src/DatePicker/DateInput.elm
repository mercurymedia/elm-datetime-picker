module DatePicker.DateInput exposing
    ( DateInput, InputError(..), Msg
    , Config, Format(..), Settings
    , defaultConfig, defaultDateFormat, defaultFormat, defaultSettings, defaultTimeFormat
    , clear, init, toPosix, update, updateFromPosix
    , hasDurationError, hasError
    , containerId, durationEndId, durationStartId
    , view, viewContainer, viewDurationInputs, viewPlaceholder
    )

{-| A configurable DateInput component, formatting and validating text input for dates and times


# Model & Msg

@docs DateInput, InputError, Msg


# Configuration & Settings

@docs Config, Format, Settings


# Defaults

@docs defaultConfig, defaultDateFormat, defaultFormat, defaultSettings, defaultTimeFormat


# Init & Updates

@docs clear, init, toPosix, update, updateFromPosix


# Query

@docs hasDurationError, hasError


# IDs

@docs containerId, durationEndId, durationStartId


# View

@docs view, viewContainer, viewDurationInputs, viewPlaceholder

-}

import Css
import Date exposing (format, numberToMonth)
import DatePicker.Icons as Icons
import DatePicker.Theme as Theme
import DatePicker.Utilities as Utilities
import DatePicker.ViewComponents as ViewComponents
import Html.Styled exposing (div, input, span, text)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onBlur, onInput)
import Time exposing (Month(..), Posix, Zone)
import Time.Extra



-- MODEL & MSG


{-| The opaque type representing a particular DateInput instance.
-}
type DateInput msg
    = DateInput (Model msg)


{-| The internal model for the DateInput.
-}
type alias Model msg =
    { inputValue : String
    , error : Maybe InputError
    , isTouched : Bool
    , internalMsg : Msg -> msg
    , dateParts : DateParts (Maybe Int)
    , timeParts : TimeParts (Maybe Int)
    }


{-| Represents date parts (day, month, year)
-}
type alias DateParts a =
    { day : a
    , month : a
    , year : a
    }


{-| Represents time parts (hour, minute)
-}
type alias TimeParts a =
    { hour : a
    , minute : a
    }


{-| Messages that can be sent through user interaction to update the DateInput model.
-}
type Msg
    = NoOp
    | UpdateValue String
    | OnBlur


{-| Possible errors in the DateInput field.
-}
type InputError
    = ValueInvalid
    | ValueNotAllowed
    | DurationInvalid



-- CONFIGURATION & SETTINGS


{-| Configuration settings for DateInput.
-}
type alias Config =
    { dateInputSettings : Settings
    , isDayDisabled : Zone -> Posix -> Bool
    , zone : Zone
    , theme : Theme.Theme
    , id : String
    }


{-| Settings for the DateInput component.
-}
type alias Settings =
    { format : Format
    , getErrorMessage : InputError -> String
    }


{-| Supported date and time formats.
-}
type Format
    = Date DateFormat
    | DateTime DateFormat TimeFormat


{-| Configuration for date formats.
-}
type alias DateFormat =
    { pattern : DatePattern
    , separator : Char
    , placeholders : DateParts Char
    }


{-| Configuration for time formats.
-}
type alias TimeFormat =
    { separator : Char
    , placeholders : TimeParts Char
    , allowedTimesOfDay : Zone -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }
    }


{-| Available date format patterns.
-}
type DatePattern
    = DDMMYYYY
    | MMDDYYYY
    | YYYYMMDD
    | YYYYDDMM



-- DEFAULTS


{-| Default configuration for DateInput.
-}
defaultConfig : Zone -> Config
defaultConfig zone =
    { dateInputSettings = defaultSettings
    , isDayDisabled = \_ _ -> False
    , zone = zone
    , theme = Theme.defaultTheme
    , id = "date-picker-component--date-input"
    }


{-| Default settings for DateInput.
-}
defaultSettings : Settings
defaultSettings =
    { format = Date defaultDateFormat
    , getErrorMessage = getDefaultErrorMessage
    }


{-| Default format used for DateInput.
-}
defaultFormat : Format
defaultFormat =
    Date defaultDateFormat


{-| Default date format pattern.
-}
defaultDateFormat : DateFormat
defaultDateFormat =
    { pattern = DDMMYYYY
    , separator = '/'
    , placeholders = { day = 'd', month = 'm', year = 'y' }
    }


{-| Default time format settings.
-}
defaultTimeFormat : TimeFormat
defaultTimeFormat =
    { separator = ':'
    , placeholders = { hour = 'h', minute = 'm' }
    , allowedTimesOfDay = \_ _ -> { startHour = 0, startMinute = 0, endHour = 23, endMinute = 59 }
    }


{-| Default error messages for input validation.
-}
getDefaultErrorMessage : InputError -> String
getDefaultErrorMessage error =
    case error of
        ValueInvalid ->
            "Invalid value. Make sure to use the correct format."

        ValueNotAllowed ->
            "Date input is not allowed."

        DurationInvalid ->
            "The given duration is invalid, the end date is before the start date."



-- INIT & UPDATES


{-| Returns a DateInput instance initialized with default values.
-}
init : (Msg -> msg) -> DateInput msg
init internalMsg =
    DateInput
        { inputValue = ""
        , error = Nothing
        , isTouched = False
        , internalMsg = internalMsg
        , dateParts = { day = Nothing, month = Nothing, year = Nothing }
        , timeParts = { hour = Nothing, minute = Nothing }
        }


{-| Clears the DateInput field.
-}
clear : DateInput msg -> DateInput msg
clear (DateInput model) =
    init model.internalMsg


{-| Update the DateInput based on user interaction according to the given internal msg.

Returns the updated DateInput and a Cmd msg.

-}
update : Config -> Msg -> DateInput msg -> ( DateInput msg, Cmd msg )
update ({ dateInputSettings } as config) msg (DateInput model) =
    case msg of
        NoOp ->
            ( DateInput model, Cmd.none )

        UpdateValue value ->
            let
                sanitizedInputValue =
                    sanitizeInputValue dateInputSettings.format value

                ( dateParts, timeParts ) =
                    inputValueToParts dateInputSettings.format sanitizedInputValue

                error =
                    catchError config sanitizedInputValue dateParts timeParts

                updatedModel =
                    { model
                        | inputValue = sanitizedInputValue
                        , dateParts = dateParts
                        , timeParts = timeParts
                        , error = error
                    }
            in
            ( DateInput updatedModel
            , Cmd.none
            )

        OnBlur ->
            let
                isTouched =
                    not (String.isEmpty model.inputValue)

                error =
                    catchError config model.inputValue model.dateParts model.timeParts

                formattedInputValue =
                    partsToInputValue dateInputSettings.format model.dateParts model.timeParts
                        |> Maybe.withDefault model.inputValue
            in
            ( DateInput
                { model
                    | inputValue = formattedInputValue
                    , isTouched = isTouched
                    , error = error
                }
            , Cmd.none
            )


{-| Updates the DateInput model from a given Posix time.

Converts the time into date and time parts and updates the model accordingly.

-}
updateFromPosix : Config -> Zone -> Posix -> DateInput msg -> DateInput msg
updateFromPosix ({ dateInputSettings } as config) zone time (DateInput model) =
    let
        posixToParts =
            Time.Extra.posixToParts zone time

        dateParts =
            { day = Just posixToParts.day
            , month = Just (Utilities.monthToNumber posixToParts.month)
            , year = Just posixToParts.year
            }

        timeParts =
            { hour = Just posixToParts.hour
            , minute = Just posixToParts.minute
            }

        inputValue =
            partsToInputValue dateInputSettings.format dateParts timeParts
                |> Maybe.withDefault model.inputValue

        error =
            catchError config inputValue dateParts timeParts
    in
    DateInput
        { model
            | dateParts = dateParts
            , timeParts = timeParts
            , inputValue = inputValue
            , error = error
        }



-- CONVERTING TEXT INPUT VALUE TO DATE/TIME PARTS


{-| Converts a DateInput model into a Posix time, if possible.
-}
toPosix : Zone -> DateInput msg -> Maybe Posix
toPosix zone (DateInput model) =
    partsToPosix zone model.dateParts model.timeParts


{-| Converts date and time parts into a Posix time.

Returns Nothing if the parts do not form a valid date/time.

-}
partsToPosix : Zone -> DateParts (Maybe Int) -> TimeParts (Maybe Int) -> Maybe Posix
partsToPosix zone dateParts timeParts =
    let
        maybeDate =
            case ( dateParts.day, dateParts.month, dateParts.year ) of
                ( Just day, Just month, Just year ) ->
                    Just ( year, Date.numberToMonth month, day )

                ( _, _, _ ) ->
                    Nothing

        maybeTime =
            case ( timeParts.hour, timeParts.minute ) of
                ( Just hour, Just minute ) ->
                    Just ( hour, minute )

                ( _, _ ) ->
                    Nothing
    in
    case ( maybeDate, maybeTime ) of
        ( Just ( year, month, day ), Nothing ) ->
            Just (Time.Extra.partsToPosix zone (Time.Extra.Parts year month day 0 0 0 0))

        ( Just ( year, month, day ), Just ( hour, minute ) ) ->
            Just (Time.Extra.partsToPosix zone (Time.Extra.Parts year month day hour minute 0 0))

        ( _, _ ) ->
            Nothing


{-| Cleans and truncates the input value based on the given format.
-}
sanitizeInputValue : Format -> String -> String
sanitizeInputValue format value =
    let
        maxLength =
            buildPlaceholderFromFormat format |> String.length

        sanitizedValue =
            case format of
                Date dateFormat ->
                    String.filter (\ch -> Char.isDigit ch || ch == dateFormat.separator) value

                DateTime dateFormat timeFormat ->
                    String.filter (\ch -> Char.isDigit ch || ch == dateFormat.separator || ch == timeFormat.separator || ch == ' ') value

        trimmedValue =
            if String.length sanitizedValue > maxLength then
                String.left maxLength sanitizedValue

            else
                sanitizedValue
    in
    trimmedValue


{-| Converts an input string into date and time parts.
-}
inputValueToParts : Format -> String -> ( DateParts (Maybe Int), TimeParts (Maybe Int) )
inputValueToParts format value =
    case format of
        Date dateFormat ->
            ( toDateParts dateFormat value, { hour = Nothing, minute = Nothing } )

        DateTime dateFormat timeFormat ->
            let
                segments =
                    String.split " " value
            in
            case segments of
                [ dateString ] ->
                    ( toDateParts dateFormat dateString, { hour = Nothing, minute = Nothing } )

                [ dateString, timeString ] ->
                    ( toDateParts dateFormat dateString, toTimeParts timeFormat timeString )

                _ ->
                    ( { day = Nothing, month = Nothing, year = Nothing }, { hour = Nothing, minute = Nothing } )


{-| Converts a date string into a DateParts record.
-}
toDateParts : DateFormat -> String -> DateParts (Maybe Int)
toDateParts dateFormat value =
    let
        segments =
            String.split (String.fromChar dateFormat.separator) value

        segmentsInParts =
            case ( dateFormat.pattern, segments ) of
                ( DDMMYYYY, [ dd, mm, yyyy ] ) ->
                    { day = String.toInt dd, month = String.toInt mm, year = String.toInt yyyy }

                ( MMDDYYYY, [ mm, dd, yyyy ] ) ->
                    { day = String.toInt dd, month = String.toInt mm, year = String.toInt yyyy }

                ( YYYYMMDD, [ yyyy, mm, dd ] ) ->
                    { day = String.toInt dd, month = String.toInt mm, year = String.toInt yyyy }

                ( YYYYDDMM, [ yyyy, dd, mm ] ) ->
                    { day = String.toInt dd, month = String.toInt mm, year = String.toInt yyyy }

                _ ->
                    { day = Nothing, month = Nothing, year = Nothing }
    in
    segmentsInParts


{-| Converts a time string into a TimeParts record.
-}
toTimeParts : TimeFormat -> String -> TimeParts (Maybe Int)
toTimeParts timeFormat value =
    let
        segments =
            String.split (String.fromChar timeFormat.separator) value

        segmentsInParts =
            case segments of
                [ hh, mm ] ->
                    { hour = String.toInt hh, minute = String.toInt mm }

                _ ->
                    { hour = Nothing, minute = Nothing }
    in
    segmentsInParts



-- CONVERTING DATE/TIME PARTS TO INPUT VALUE


{-| Converts date and time parts into an input string value.
-}
partsToInputValue : Format -> DateParts (Maybe Int) -> TimeParts (Maybe Int) -> Maybe String
partsToInputValue format dateParts timeParts =
    case format of
        Date dateFormat ->
            datePartsToInputValue dateFormat dateParts

        DateTime dateFormat timeFormat ->
            let
                dateValue =
                    datePartsToInputValue dateFormat dateParts

                timeValue =
                    timePartsToInputValue timeFormat timeParts
            in
            case ( dateValue, timeValue ) of
                ( Just d, Just t ) ->
                    [ d, " ", t ] |> String.concat |> Just

                _ ->
                    Nothing


datePartsToInputValue : DateFormat -> DateParts (Maybe Int) -> Maybe String
datePartsToInputValue dateFormat { day, month, year } =
    case ( day, month, year ) of
        ( Just d, Just m, Just y ) ->
            let
                ( dd, mm, yyyy ) =
                    ( addLeadingZeros 2 (String.fromInt d)
                    , addLeadingZeros 2 (String.fromInt m)
                    , addLeadingZeros 4 (String.fromInt y)
                    )

                s =
                    String.fromChar dateFormat.separator
            in
            case dateFormat.pattern of
                DDMMYYYY ->
                    [ dd, s, mm, s, yyyy ] |> String.concat |> Just

                MMDDYYYY ->
                    [ mm, s, dd, s, yyyy ] |> String.concat |> Just

                YYYYMMDD ->
                    [ yyyy, s, mm, s, dd ] |> String.concat |> Just

                YYYYDDMM ->
                    [ yyyy, s, dd, s, mm ] |> String.concat |> Just

        _ ->
            Nothing


timePartsToInputValue : TimeFormat -> TimeParts (Maybe Int) -> Maybe String
timePartsToInputValue timeFormat { hour, minute } =
    case ( hour, minute ) of
        ( Just h, Just m ) ->
            let
                ( hh, mm ) =
                    ( addLeadingZeros 2 (String.fromInt h)
                    , addLeadingZeros 2 (String.fromInt m)
                    )

                s =
                    String.fromChar timeFormat.separator
            in
            [ hh, s, mm ] |> String.concat |> Just

        _ ->
            Nothing



-- DETERMINE VALIDATION ERRORS


{-| Checks if the DateInput has an error.
-}
hasError : DateInput msg -> Bool
hasError (DateInput { isTouched, error }) =
    case error of
        Just _ ->
            isTouched

        Nothing ->
            False


{-| Checks if one of the two DateInputs of a duration input has an error, returning a boolean and an optional error.
-}
hasDurationError : Zone -> ( DateInput msg, DateInput msg ) -> ( Bool, Maybe InputError )
hasDurationError zone ( DateInput startModel, DateInput endModel ) =
    let
        hasError_ =
            hasError (DateInput startModel) || hasError (DateInput endModel)

        isDurationTouched =
            startModel.isTouched || endModel.isTouched

        invalidDuration =
            case ( toPosix zone (DateInput startModel), toPosix zone (DateInput endModel) ) of
                ( Just start, Just end ) ->
                    Time.posixToMillis start > Time.posixToMillis end

                ( _, _ ) ->
                    False

        error =
            case ( startModel.error, endModel.error ) of
                ( Just e, _ ) ->
                    Just e

                ( _, Just e ) ->
                    Just e

                _ ->
                    if invalidDuration then
                        Just DurationInvalid

                    else
                        Nothing
    in
    ( hasError_ || (isDurationTouched && invalidDuration)
    , error
    )


{-| Checks if the input value, date parts, and time parts are valid
and allowed based on the provided configuration, returning an optional error.
-}
catchError : Config -> String -> DateParts (Maybe Int) -> TimeParts (Maybe Int) -> Maybe InputError
catchError { dateInputSettings, isDayDisabled, zone } inputValue dateParts timeParts =
    let
        isValid =
            case dateInputSettings.format of
                Date _ ->
                    isValidDate dateParts

                DateTime _ _ ->
                    isValidDate dateParts && isValidTime timeParts

        hasValidationError =
            not (String.isEmpty inputValue)
                && not isValid

        hasNotAllowedError =
            case ( partsToPosix zone dateParts timeParts, dateInputSettings.format ) of
                ( Just posix, Date _ ) ->
                    isDayDisabled zone posix

                ( Just posix, DateTime _ timeFormat ) ->
                    let
                        timeBoundaries =
                            timeFormat.allowedTimesOfDay zone posix

                        isAllowedTime =
                            Utilities.posixWithinTimeBoundaries zone timeBoundaries posix
                    in
                    isDayDisabled zone posix || not isAllowedTime

                ( _, _ ) ->
                    False
    in
    case ( hasValidationError, hasNotAllowedError ) of
        ( True, _ ) ->
            Just ValueInvalid

        ( False, True ) ->
            Just ValueNotAllowed

        ( _, _ ) ->
            Nothing



-- VIEW IDs


{-| Generates the ID for the container element based on the provided configuration.
-}
containerId : Config -> String
containerId { id } =
    id ++ "--container"


{-| Generates the ID for a duration's start input element based on the provided configuration.
-}
durationStartId : String -> String
durationStartId id =
    id ++ "--start"


{-| Generates the ID for a duration's end input element based on the provided configuration.
-}
durationEndId : String -> String
durationEndId id =
    id ++ "--end"



-- VIEWS


{-| Renders a single date input element view with error handling and theme styling.
-}
view : List (Html.Styled.Attribute msg) -> Config -> DateInput msg -> Html.Styled.Html msg
view attrs { dateInputSettings, theme, id } (DateInput model) =
    let
        placeholder =
            buildPlaceholderFromFormat dateInputSettings.format

        hasError_ =
            hasError (DateInput model)

        ( textColor, iconColor ) =
            if hasError_ then
                ( theme.color.text.error, theme.color.text.error )

            else
                ( theme.color.text.primary, theme.color.text.secondary )
    in
    viewTextField theme
        (Attrs.css [ Css.color textColor ] :: attrs)
        [ viewInput theme
            [ Attrs.value model.inputValue
            , Attrs.placeholder placeholder
            , Attrs.id id
            , onInput (model.internalMsg << UpdateValue)
            , onBlur (model.internalMsg OnBlur)
            ]
        , viewCalendarIcon [ Attrs.css [ Css.color iconColor ] ]
        , viewError theme hasError_ dateInputSettings.getErrorMessage model.error
        ]


{-| Renders two date input fields (start and end) for duration picking with error handling and theme styling.
-}
viewDurationInputs : List (Html.Styled.Attribute msg) -> Config -> ( DateInput msg, DateInput msg ) -> Html.Styled.Html msg
viewDurationInputs attrs { dateInputSettings, theme, id, zone } ( DateInput startModel, DateInput endModel ) =
    let
        placeholder =
            buildPlaceholderFromFormat dateInputSettings.format

        ( hasDurationError_, durationError ) =
            hasDurationError zone ( DateInput startModel, DateInput endModel )

        ( textColor, iconColor ) =
            if hasDurationError_ then
                ( theme.color.text.error, theme.color.text.error )

            else
                ( theme.color.text.primary, theme.color.text.secondary )
    in
    div
        (Attrs.css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.width (Css.pct 100)
            , Css.color textColor
            ]
            :: attrs
        )
        [ viewTextField theme
            []
            [ viewInput theme
                [ Attrs.value startModel.inputValue
                , Attrs.placeholder placeholder
                , Attrs.id (durationStartId id)
                , onInput (startModel.internalMsg << UpdateValue)
                , onBlur (startModel.internalMsg OnBlur)
                ]
            , viewCalendarIcon [ Attrs.css [ Css.color iconColor ] ]
            ]
        , span
            [ Attrs.css
                [ Css.width (Css.rem 2)
                , Css.display Css.inlineFlex
                , Css.justifyContent Css.center
                , Css.flexShrink (Css.int 0)
                ]
            ]
            [ text "–" ]
        , viewTextField theme
            []
            [ viewInput theme
                [ Attrs.value endModel.inputValue
                , Attrs.placeholder placeholder
                , Attrs.id (durationEndId id)
                , onInput (endModel.internalMsg << UpdateValue)
                , onBlur (endModel.internalMsg OnBlur)
                ]
            , viewCalendarIcon [ Attrs.css [ Css.color iconColor ] ]
            ]
        , viewError theme hasDurationError_ dateInputSettings.getErrorMessage durationError
        ]


{-| Renders a styled text field container to pass in the input and error elements.
-}
viewTextField : Theme.Theme -> List (Html.Styled.Attribute msg) -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewTextField theme attrs children =
    div
        (Attrs.css
            [ Css.displayFlex
            , Css.width (Css.pct 100)
            , Css.alignItems Css.center
            , Css.borderRadius (Css.px theme.borderRadius.base)
            , Css.cursor Css.text_
            , Css.position Css.relative
            , Css.backgroundColor theme.color.background.input
            , Css.border3 (Css.px theme.borderWidth) Css.solid theme.color.border
            , ViewComponents.colorsTransition theme
            , Css.hover [ Css.backgroundColor theme.color.action.hover ]
            , Css.pseudoClass "focus-within"
                [ Css.borderColor theme.color.primary.main
                , Css.backgroundColor theme.color.background.input
                ]
            ]
            :: attrs
        )
        children


{-| Renders an input field with theme-based styling.
-}
viewInput : Theme.Theme -> List (Html.Styled.Attribute msg) -> Html.Styled.Html msg
viewInput theme attrs =
    input
        (Attrs.css
            [ Css.flexGrow (Css.num 1)
            , Css.width (Css.px 0)
            , Css.minWidth (Css.px 40)
            , Css.outline Css.none
            , Css.padding (Css.rem 0.5)
            , Css.borderWidth (Css.px 0)
            , Css.borderRadius (Css.px 0)
            , Css.backgroundColor Css.transparent
            , Css.minHeight (Css.px theme.size.inputElement)
            , Css.color Css.currentColor
            , Css.pseudoClass ":placeholder"
                [ Css.textOverflow Css.ellipsis
                , Css.color theme.color.text.disabled
                ]
            , Css.pseudoClass "placeholder-shown"
                [ Css.textOverflow Css.ellipsis
                , Css.color theme.color.text.disabled
                ]
            ]
            :: Attrs.autocomplete False
            :: Attrs.type_ "text"
            :: attrs
        )
        []


{-| Renders a calendar icon to be used in the textField.
-}
viewCalendarIcon : List (Html.Styled.Attribute msg) -> Html.Styled.Html msg
viewCalendarIcon attrs =
    span
        (Attrs.css
            [ Css.position Css.absolute
            , Css.right (Css.rem 0.5)
            , Css.pointerEvents Css.none
            ]
            :: attrs
        )
        [ Icons.calendar
            |> Icons.withSize 16
            |> Icons.toHtml []
            |> Html.Styled.fromUnstyled
        ]


{-| Displays an error message if the error is visible.
-}
viewError : Theme.Theme -> Bool -> (InputError -> String) -> Maybe InputError -> Html.Styled.Html msg
viewError theme isVisible errorMessageFn error =
    case ( error, isVisible ) of
        ( Just err, True ) ->
            span
                [ Attrs.css
                    [ Css.position Css.absolute
                    , Css.top (Css.pct 100)
                    , Css.fontSize (Css.px theme.fontSize.xs)
                    , Css.paddingTop (Css.px 3)
                    ]
                ]
                [ text <| errorMessageFn err ]

        ( _, _ ) ->
            text ""


{-| Renders a container element for the date input element.
This element is scaled to the date input element's size and is used to position the picker popover.
-}
viewContainer : Theme.Theme -> List (Html.Styled.Attribute msg) -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
viewContainer theme attrs children =
    div
        (Attrs.css
            [ Css.position Css.relative
            , Css.display Css.inlineFlex
            , Css.width (Css.pct 100)
            , Css.height (Css.px theme.size.inputElement)
            ]
            :: attrs
        )
        children


{-| Renders a placeholder view based on the configuration.
This element is empty and scaled to the date input element's size. It is used in combination with
the picker popover working as a placeholder to arrange the picker popover's layout. The real date
input element will be positioned fixed above this element when the picker popover is opened.
-}
viewPlaceholder : Config -> Html.Styled.Html msg
viewPlaceholder { theme } =
    div
        [ Attrs.css [ Css.displayFlex, Css.width (Css.pct 100), Css.height (Css.px theme.size.inputElement) ]
        , Attrs.attribute "aria-hidden" "true"
        ]
        []



-- UTILS


{-| Builds a placeholder string based on the given date/time format.
-}
buildPlaceholderFromFormat : Format -> String
buildPlaceholderFromFormat format =
    case format of
        Date dateFormat ->
            buildDatePlaceholder dateFormat

        DateTime dateFormat timeFormat ->
            buildDatePlaceholder dateFormat ++ " " ++ buildTimePlaceholder timeFormat


{-| Builds a date placeholder string based on the given date format.
-}
buildDatePlaceholder : DateFormat -> String
buildDatePlaceholder dateFormat =
    let
        { day, month, year } =
            dateFormat.placeholders

        ( dd, mm, yyyy ) =
            ( repeatCharToString day 2
            , repeatCharToString month 2
            , repeatCharToString year 4
            )

        s =
            String.fromChar dateFormat.separator
    in
    case dateFormat.pattern of
        DDMMYYYY ->
            [ dd, s, mm, s, yyyy ] |> String.concat

        MMDDYYYY ->
            [ mm, s, dd, s, yyyy ] |> String.concat

        YYYYMMDD ->
            [ yyyy, s, mm, s, dd ] |> String.concat

        YYYYDDMM ->
            [ yyyy, s, dd, s, mm ] |> String.concat


{-| Builds a time placeholder string based on the given time format.
-}
buildTimePlaceholder : TimeFormat -> String
buildTimePlaceholder timeFormat =
    let
        { hour, minute } =
            timeFormat.placeholders

        ( hh, mm ) =
            ( repeatCharToString hour 2
            , repeatCharToString minute 2
            )

        s =
            String.fromChar timeFormat.separator
    in
    [ hh, s, mm ] |> String.concat


{-| Checks if the given date parts are valid (i.e. within the correct range for days, months and years).
-}
isValidDate : DateParts (Maybe Int) -> Bool
isValidDate { day, month, year } =
    case ( day, month, year ) of
        ( Just d, Just m, Just y ) ->
            case numberToMonth m of
                Just validMonth ->
                    let
                        daysInCurrentMonth =
                            daysInMonth y validMonth
                    in
                    d > 0 && d <= daysInCurrentMonth

                Nothing ->
                    False

        ( _, _, _ ) ->
            False


{-| Checks if a given time is valid (i.e., within the correct range for hours and minutes).
-}
isValidTime : TimeParts (Maybe Int) -> Bool
isValidTime { hour, minute } =
    case ( hour, minute ) of
        ( Just h, Just m ) ->
            h >= 0 && h < 24 && m >= 0 && m < 60

        ( _, _ ) ->
            False


{-| Repeats a character `count` times and returns the resulting string.
-}
repeatCharToString : Char -> Int -> String
repeatCharToString char count =
    if count <= 0 then
        ""

    else
        String.fromChar char ++ repeatCharToString char (count - 1)


{-| Adds leading zeros to a string value to match a specified size.
-}
addLeadingZeros : Int -> String -> String
addLeadingZeros size value =
    let
        currentLength =
            String.length value

        zerosToAdd =
            size - currentLength

        leadingZeros =
            String.repeat zerosToAdd "0"
    in
    if zerosToAdd > 0 && value /= "" then
        leadingZeros ++ value

    else
        value


{-| Determines if a given year is a leap year.
-}
isLeapYear : Int -> Bool
isLeapYear year =
    modBy 400 year == 0 || modBy 100 year /= 0 && modBy 4 year == 0


{-| Returns the number of days in a given month of a specific year.
-}
daysInMonth : Int -> Month -> Int
daysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


{-| Converts a month number to its corresponding Month type, if valid.
-}
numberToMonth : Int -> Maybe Month
numberToMonth month =
    case month of
        1 ->
            Just Jan

        2 ->
            Just Feb

        3 ->
            Just Mar

        4 ->
            Just Apr

        5 ->
            Just May

        6 ->
            Just Jun

        7 ->
            Just Jul

        8 ->
            Just Aug

        9 ->
            Just Sep

        10 ->
            Just Oct

        11 ->
            Just Nov

        12 ->
            Just Dec

        _ ->
            Nothing
