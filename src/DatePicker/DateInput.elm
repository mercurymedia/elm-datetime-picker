module DatePicker.DateInput exposing (..)

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


type DateInput msg
    = DateInput (Model msg)


type alias Model msg =
    { inputValue : String
    , error : Maybe InputError
    , isTouched : Bool
    , internalMsg : Msg -> msg
    , dateParts : DateParts (Maybe Int)
    , timeParts : TimeParts (Maybe Int)
    }


type alias DateParts a =
    { day : a
    , month : a
    , year : a
    }


type alias TimeParts a =
    { hour : a
    , minute : a
    }


type Format
    = Date DateFormat
    | DateTime DateFormat TimeFormat


type alias DateFormat =
    { pattern : DatePattern
    , separator : Char
    , placeholders : DateParts Char
    }


type alias TimeFormat =
    { separator : Char
    , placeholders : TimeParts Char
    , allowedTimesOfDay : Zone -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }
    }


type DatePattern
    = DDMMYYYY
    | MMDDYYYY
    | YYYYMMDD
    | YYYYDDMM


type Msg
    = NoOp
    | UpdateValue String
    | OnBlur


type alias Config =
    { dateInputSettings : Settings
    , isDayDisabled : Zone -> Posix -> Bool
    , zone : Zone
    , theme : Theme.Theme
    , id : String
    }


type alias Settings =
    { format : Format
    , getErrorMessage : InputError -> String
    }


type InputError
    = ValueInvalid
    | ValueNotAllowed


defaultConfig : Zone -> Config
defaultConfig zone =
    { dateInputSettings = defaultSettings
    , isDayDisabled = \_ _ -> False
    , zone = zone
    , theme = Theme.defaultTheme
    , id = "date-picker-component--date-input"
    }


defaultSettings : Settings
defaultSettings =
    { format = Date defaultDateFormat
    , getErrorMessage = getDefaultErrorMessage
    }


defaultFormat : Format
defaultFormat =
    Date defaultDateFormat


defaultDateFormat : DateFormat
defaultDateFormat =
    { pattern = DDMMYYYY
    , separator = '/'
    , placeholders = { day = 'd', month = 'm', year = 'y' }
    }


defaultTimeFormat : TimeFormat
defaultTimeFormat =
    { separator = ':'
    , placeholders = { hour = 'h', minute = 'm' }
    , allowedTimesOfDay = \_ _ -> { startHour = 0, startMinute = 0, endHour = 23, endMinute = 59 }
    }


getDefaultErrorMessage : InputError -> String
getDefaultErrorMessage error =
    case error of
        ValueInvalid ->
            "Invalid value. Make sure to use the correct format."

        ValueNotAllowed ->
            "Date not allowed."


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


clear : DateInput msg -> DateInput msg
clear (DateInput model) =
    init model.internalMsg


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


toPosix : Zone -> DateInput msg -> Maybe Posix
toPosix zone (DateInput model) =
    partsToPosix zone model.dateParts model.timeParts


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


hasError : Bool -> Maybe InputError -> Bool
hasError isTouched error =
    case error of
        Just _ ->
            isTouched

        Nothing ->
            False


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


view : List (Html.Styled.Attribute msg) -> Config -> DateInput msg -> Html.Styled.Html msg
view attrs { dateInputSettings, theme, id } (DateInput model) =
    let
        placeholder =
            buildPlaceholderFromFormat dateInputSettings.format

        hasError_ =
            hasError model.isTouched model.error

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
        , viewError theme hasError_ model.error
        ]


viewDurationInput : List (Html.Styled.Attribute msg) -> Config -> ( DateInput msg, DateInput msg ) -> Html.Styled.Html msg
viewDurationInput attrs { dateInputSettings, theme, id } ( DateInput startModel, DateInput endModel ) =
    let
        placeholder =
            buildPlaceholderFromFormat dateInputSettings.format

        hasError_ =
            hasError startModel.isTouched startModel.error || hasError endModel.isTouched endModel.error

        ( textColor, iconColor ) =
            if hasError_ then
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
        , viewError theme hasError_ startModel.error
        ]


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


viewError : Theme.Theme -> Bool -> Maybe InputError -> Html.Styled.Html msg
viewError theme isVisible error =
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
                [ text <| getDefaultErrorMessage err ]

        ( _, _ ) ->
            text ""


viewPlaceholder : Config -> Html.Styled.Html msg
viewPlaceholder { theme } =
    div
        [ Attrs.css [ Css.displayFlex, Css.width (Css.pct 100), Css.height (Css.px theme.size.inputElement) ]
        , Attrs.attribute "aria-hidden" "true"
        ]
        []


containerId : Config -> String
containerId { id } =
    id ++ "--container"


durationStartId : String -> String
durationStartId id =
    id ++ "--start"


durationEndId : String -> String
durationEndId id =
    id ++ "--end"


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


buildPlaceholderFromFormat : Format -> String
buildPlaceholderFromFormat format =
    case format of
        Date dateFormat ->
            buildDatePlaceholder dateFormat

        DateTime dateFormat timeFormat ->
            buildDatePlaceholder dateFormat ++ " " ++ buildTimePlaceholder timeFormat


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


isValidTime : TimeParts (Maybe Int) -> Bool
isValidTime { hour, minute } =
    case ( hour, minute ) of
        ( Just h, Just m ) ->
            h >= 0 && h < 24 && m >= 0 && m < 60

        ( _, _ ) ->
            False


repeatCharToString : Char -> Int -> String
repeatCharToString char count =
    if count <= 0 then
        ""

    else
        String.fromChar char ++ repeatCharToString char (count - 1)


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


isLeapYear : Int -> Bool
isLeapYear year =
    modBy 400 year == 0 || modBy 100 year /= 0 && modBy 4 year == 0


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
