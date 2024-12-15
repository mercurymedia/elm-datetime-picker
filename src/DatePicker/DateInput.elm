module DatePicker.DateInput exposing (..)

import Css
import Date exposing (format, numberToMonth)
import DatePicker.Icons as Icons
import DatePicker.Utilities as Utilities
import Html exposing (Html, time)
import Html.Styled exposing (div, input, span)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onBlur, onInput)
import Time exposing (Month(..), Posix, Zone)
import Time.Extra


type DateInput msg
    = DateInput (Model msg)


type alias Model msg =
    { parts : DateParts (Maybe Int)
    , inputValue : String
    , hasErrors : Bool
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
    { format : Format
    }


defaultConfig : Config
defaultConfig =
    { format = DateTime defaultDateFormat defaultTimeFormat }


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
    }


init : (Msg -> msg) -> DateInput msg
init internalMsg =
    DateInput
        { parts = { day = Nothing, month = Nothing, year = Nothing }
        , inputValue = ""
        , hasErrors = False
        , isTouched = False
        , internalMsg = internalMsg
        , dateParts = { day = Nothing, month = Nothing, year = Nothing }
        , timeParts = { hour = Nothing, minute = Nothing }
        }


update : Config -> Msg -> DateInput msg -> ( DateInput msg, Cmd msg )
update config msg (DateInput model) =
    case msg of
        NoOp ->
            ( DateInput model, Cmd.none )

        UpdateValue value ->
            let
                sanitizedInputValue =
                    sanitizeInputValue config.format value

                ( dateParts, timeParts ) =
                    inputValueToParts config.format sanitizedInputValue

                hasErrors_ =
                    hasErrors config.format model.isTouched sanitizedInputValue dateParts timeParts
            in
            ( DateInput
                { model
                    | inputValue = sanitizedInputValue
                    , parts = dateParts
                    , dateParts = dateParts
                    , timeParts = timeParts
                    , hasErrors = hasErrors_
                }
            , Cmd.none
            )

        OnBlur ->
            let
                isTouched =
                    not (String.isEmpty model.inputValue)

                hasErrors_ =
                    hasErrors config.format isTouched model.inputValue model.dateParts model.timeParts

                formattedInputValue =
                    partsToInputValue config.format model.dateParts model.timeParts
                        |> Maybe.withDefault model.inputValue
            in
            ( DateInput
                { model
                    | inputValue = formattedInputValue
                    , hasErrors = hasErrors_
                    , isTouched = isTouched
                }
            , Cmd.none
            )


updateFromPosix : Config -> Zone -> Posix -> DateInput msg -> DateInput msg
updateFromPosix config zone time (DateInput model) =
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
            partsToInputValue config.format dateParts timeParts
                |> Maybe.withDefault model.inputValue

        hasErrors_ =
            hasErrors config.format model.isTouched inputValue dateParts timeParts
    in
    DateInput
        { model
            | parts = dateParts
            , dateParts = dateParts
            , timeParts = timeParts
            , inputValue = inputValue
            , hasErrors = hasErrors_
        }


toPosix : Zone -> DateInput msg -> Maybe Posix
toPosix zone (DateInput model) =
    case ( model.parts.day, model.parts.month, model.parts.year ) of
        ( Just day, Just month, Just year ) ->
            Just
                (Time.Extra.partsToPosix zone (Time.Extra.Parts year (Date.numberToMonth month) day 0 0 0 0))

        ( _, _, _ ) ->
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


hasErrors : Format -> Bool -> String -> DateParts (Maybe Int) -> TimeParts (Maybe Int) -> Bool
hasErrors format isTouched inputValue dateParts timeParts =
    let
        isValid =
            case format of
                Date _ ->
                    isValidDate dateParts

                DateTime _ _ ->
                    isValidDate dateParts && isValidTime timeParts
    in
    isTouched
        && not (String.isEmpty inputValue)
        && not isValid


classPrefix : String -> String
classPrefix class =
    "elm-datetimepicker--" ++ class


view : List (Html.Attribute msg) -> Config -> DateInput msg -> Html msg
view attrs config (DateInput model) =
    viewStyled (Utilities.toStyledAttrs attrs) config (DateInput model)
        |> Html.Styled.toUnstyled


viewStyled : List (Html.Styled.Attribute msg) -> Config -> DateInput msg -> Html.Styled.Html msg
viewStyled attrs config (DateInput model) =
    let
        placeholder =
            buildPlaceholderFromFormat config.format

        errorStyles =
            if model.hasErrors then
                [ Css.color (Css.hex "#c00") ]

            else
                []
    in
    div
        (Attrs.css
            [ Css.backgroundColor (Css.hex "#fff")
            , Css.display Css.inlineFlex
            , Css.alignItems Css.center
            , Css.border3 (Css.px 1) Css.solid (Css.hex "#ccc")
            , Css.borderRadius (Css.px 3)
            , Css.batch errorStyles
            ]
            :: attrs
        )
        [ input
            [ Attrs.css [ Css.border (Css.px 0), Css.color Css.currentColor ]
            , Attrs.type_ "text"
            , Attrs.value model.inputValue
            , Attrs.placeholder placeholder
            , onInput (model.internalMsg << UpdateValue)
            , onBlur (model.internalMsg OnBlur)
            ]
            []
        , span [ Attrs.class (classPrefix "input-icon") ]
            [ Icons.calendar
                |> Icons.withSize 16
                |> Icons.toHtml []
                |> Html.Styled.fromUnstyled
            ]
        ]


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
