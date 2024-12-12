module DatePicker.DateInput exposing (..)

import Date exposing (format, numberToMonth)
import DatePicker.Icons as Icons
import DatePicker.Utilities as Utilities
import Html exposing (Html)
import Html.Styled exposing (div, input, span)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onBlur, onInput)
import Time exposing (Month(..), Posix, Zone)
import Time.Extra


type DateInput msg
    = DateInput (Model msg)


type alias Model msg =
    { parts : Parts (Maybe Int)
    , inputValue : String
    , hasErrors : Bool
    , isTouched : Bool
    , internalMsg : Msg -> msg
    }


type alias Parts a =
    { day : a
    , month : a
    , year : a
    }


type alias Format =
    { pattern : Pattern
    , separator : Char
    , placeholders : Parts Char
    }


type Pattern
    = DDMMYYYY
    | MMDDYYYY
    | YYYYMMDD
    | YYYYDDMM


type Msg
    = NoOp
    | UpdateValue String
    | OnBlur


init : (Msg -> msg) -> DateInput msg
init internalMsg =
    DateInput
        { parts = { day = Nothing, month = Nothing, year = Nothing }
        , inputValue = ""
        , hasErrors = False
        , isTouched = False
        , internalMsg = internalMsg
        }


defaultFormat : Format
defaultFormat =
    { pattern = DDMMYYYY
    , separator = '/'
    , placeholders = { day = 'd', month = 'm', year = 'y' }
    }


update : Msg -> DateInput msg -> ( DateInput msg, Cmd msg )
update msg (DateInput model) =
    case msg of
        NoOp ->
            ( DateInput model, Cmd.none )

        UpdateValue value ->
            let
                sanitizedInputValue =
                    sanitizeInputValue defaultFormat value

                parts =
                    inputValueToParts defaultFormat sanitizedInputValue

                hasErrors_ =
                    hasErrors model.isTouched sanitizedInputValue parts
            in
            ( DateInput
                { model
                    | inputValue = sanitizedInputValue
                    , parts = parts
                    , hasErrors = hasErrors_
                }
            , Cmd.none
            )

        OnBlur ->
            let
                isTouched =
                    not (String.isEmpty model.inputValue)

                hasErrors_ =
                    hasErrors isTouched model.inputValue model.parts

                formattedInputValue =
                    partsToInputValue defaultFormat model.parts
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


updateFromPosix : Zone -> Posix -> DateInput msg -> DateInput msg
updateFromPosix zone time (DateInput model) =
    let
        posixToParts =
            Time.Extra.posixToParts zone time

        parts =
            { day = Just posixToParts.day
            , month = Just (Utilities.monthToNumber posixToParts.month)
            , year = Just posixToParts.year
            }

        inputValue =
            partsToInputValue defaultFormat parts
                |> Maybe.withDefault model.inputValue

        hasErrors_ =
            hasErrors model.isTouched inputValue parts
    in
    DateInput
        { model
            | parts = parts
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
            String.filter (\ch -> Char.isDigit ch || ch == format.separator) value

        trimmedValue =
            if String.length sanitizedValue > maxLength then
                String.left maxLength sanitizedValue

            else
                sanitizedValue
    in
    trimmedValue


inputValueToParts : Format -> String -> Parts (Maybe Int)
inputValueToParts format value =
    let
        segments =
            String.split (String.fromChar format.separator) value

        segmentsInParts =
            case ( format.pattern, segments ) of
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


partsToInputValue : Format -> Parts (Maybe Int) -> Maybe String
partsToInputValue format { day, month, year } =
    case ( day, month, year ) of
        ( Just d, Just m, Just y ) ->
            let
                ( dd, mm, yyyy ) =
                    ( addLeadingZeros 2 (String.fromInt d)
                    , addLeadingZeros 2 (String.fromInt m)
                    , addLeadingZeros 4 (String.fromInt y)
                    )

                s =
                    String.fromChar format.separator
            in
            case format.pattern of
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


hasErrors : Bool -> String -> Parts (Maybe Int) -> Bool
hasErrors isTouched inputValue parts =
    isTouched
        && not (String.isEmpty inputValue)
        && not (isValidDate parts)


classPrefix : String -> String
classPrefix class =
    "elm-datetimepicker--" ++ class


view : List (Html.Attribute msg) -> DateInput msg -> Html msg
view attrs (DateInput model) =
    viewStyled (Utilities.toStyledAttrs attrs) (DateInput model)
        |> Html.Styled.toUnstyled


viewStyled : List (Html.Styled.Attribute msg) -> DateInput msg -> Html.Styled.Html msg
viewStyled attrs (DateInput model) =
    let
        placeholder =
            buildPlaceholderFromFormat defaultFormat
    in
    div
        (Attrs.classList
            [ ( classPrefix "input-container", True )
            , ( classPrefix "error", model.hasErrors )
            ]
            :: attrs
        )
        [ input
            [ Attrs.class (classPrefix "input")
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
    let
        { day, month, year } =
            format.placeholders

        ( dd, mm, yyyy ) =
            ( repeatCharToString day 2
            , repeatCharToString month 2
            , repeatCharToString year 4
            )

        s =
            String.fromChar format.separator
    in
    case format.pattern of
        DDMMYYYY ->
            [ dd, s, mm, s, yyyy ] |> String.concat

        MMDDYYYY ->
            [ mm, s, dd, s, yyyy ] |> String.concat

        YYYYMMDD ->
            [ yyyy, s, mm, s, dd ] |> String.concat

        YYYYDDMM ->
            [ yyyy, s, dd, s, mm ] |> String.concat


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


isValidDate : Parts (Maybe Int) -> Bool
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
