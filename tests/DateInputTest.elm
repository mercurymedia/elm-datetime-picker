module DateInputTest exposing (suite)

import DatePicker.DateInput as DateInput
import Expect
import Html exposing (time)
import Test exposing (..)
import Time exposing (Month(..))
import Time.Extra as TimeExtra


suite : Test
suite =
    let
        timeZone =
            Time.utc

        dateFormat =
            { pattern = DateInput.DDMMYYYY
            , separator = '/'
            , placeholders = { day = 'd', month = 'm', year = 'y' }
            }

        timeFormat =
            { separator = ':'
            , placeholders = { hour = 'h', minute = 'm' }
            , allowedTimesOfDay = \_ _ -> { startHour = 10, startMinute = 0, endHour = 15, endMinute = 59 }
            }
    in
    describe "DatePicker.DateInput"
        [ describe "partsToPosix"
            [ test "date and time parts return a valid posix" <|
                \_ ->
                    let
                        dateParts =
                            { day = Just 30, month = Just 1, year = Just 2025 }

                        timeParts =
                            { hour = Just 12, minute = Just 12 }

                        expectedPosix =
                            TimeExtra.partsToPosix timeZone
                                { year = 2025
                                , month = Time.Jan
                                , day = 30
                                , hour = 12
                                , minute = 12
                                , second = 0
                                , millisecond = 0
                                }
                                |> Just
                    in
                    Expect.equal (DateInput.partsToPosix timeZone dateParts timeParts) expectedPosix
            , test "partially empty date or time parts return `Nothing`" <|
                \_ ->
                    let
                        dateParts =
                            { day = Just 30, month = Just 1, year = Nothing }

                        timeParts =
                            { hour = Just 12, minute = Just 12 }

                        expectedPosix =
                            Nothing
                    in
                    Expect.equal (DateInput.partsToPosix timeZone dateParts timeParts) expectedPosix
            ]
        , describe "sanitizeInputValue (Date format)"
            [ test "ignores other characters than digits or separators" <|
                \_ ->
                    Expect.equal (DateInput.sanitizeInputValue (DateInput.Date dateFormat) "30/a") "30/"
            , test "trims value to maximum length" <|
                \_ ->
                    Expect.equal (DateInput.sanitizeInputValue (DateInput.Date dateFormat) "30/01/20255") "30/01/2025"
            ]
        , describe "sanitizeInputValue (DateTime format)"
            [ test "ignores other characters than digits or separators or spaces" <|
                \_ ->
                    Expect.equal (DateInput.sanitizeInputValue (DateInput.DateTime dateFormat timeFormat) "30/01/2025 12:b") "30/01/2025 12:"
            , test "trims value to maximum length" <|
                \_ ->
                    Expect.equal (DateInput.sanitizeInputValue (DateInput.DateTime dateFormat timeFormat) "30/01/2025 12:122") "30/01/2025 12:12"
            ]
        , describe "inputValueToParts"
            [ test "returns all date and time parts for correct dateTime string" <|
                \_ ->
                    let
                        expectedDateParts =
                            { day = Just 30, month = Just 1, year = Just 2025 }

                        expectedTimeParts =
                            { hour = Just 12, minute = Just 12 }
                    in
                    Expect.equal (DateInput.inputValueToParts (DateInput.DateTime dateFormat timeFormat) "30/01/2025 12:12")
                        ( expectedDateParts, expectedTimeParts )
            , test "returns only partial date and time parts for parially correct dateTime string" <|
                \_ ->
                    let
                        expectedDateParts =
                            { day = Just 30, month = Just 1, year = Nothing }

                        expectedTimeParts =
                            { hour = Just 12, minute = Nothing }
                    in
                    Expect.equal (DateInput.inputValueToParts (DateInput.DateTime dateFormat timeFormat) "30/01/ 12:")
                        ( expectedDateParts, expectedTimeParts )
            ]
        , describe "partsToInputValue"
            [ test "returns formatted dateTimestring for complete date and time parts" <|
                \_ ->
                    let
                        dateParts =
                            { day = Just 30, month = Just 1, year = Just 2025 }

                        timeParts =
                            { hour = Just 12, minute = Just 12 }
                    in
                    Expect.equal (DateInput.partsToInputValue (DateInput.DateTime dateFormat timeFormat) dateParts timeParts)
                        (Just "30/01/2025 12:12")
            ]
        , describe "catchError"
            [ test "returns no error (`Nothing`) with valid date and time parts" <|
                \_ ->
                    let
                        settings =
                            { format = DateInput.DateTime dateFormat timeFormat
                            , getErrorMessage = \_ -> ""
                            }

                        defaultConfig =
                            DateInput.defaultConfig timeZone

                        config =
                            { defaultConfig
                                | dateInputSettings = settings
                                , isDayDisabled = \_ _ -> False
                            }

                        inputValue =
                            "30/01/2025 12:12"

                        dateParts =
                            { day = Just 30, month = Just 1, year = Just 2025 }

                        timeParts =
                            { hour = Just 12, minute = Just 12 }
                    in
                    Expect.equal (DateInput.catchError config inputValue dateParts timeParts)
                        Nothing
            , test "returns `ValueInvalid` error with invalid date parts" <|
                \_ ->
                    let
                        settings =
                            { format = DateInput.DateTime dateFormat timeFormat
                            , getErrorMessage = \_ -> ""
                            }

                        defaultConfig =
                            DateInput.defaultConfig timeZone

                        config =
                            { defaultConfig
                                | dateInputSettings = settings
                                , isDayDisabled = \_ _ -> False
                            }

                        inputValue =
                            "33/01/2025 12:12"

                        dateParts =
                            { day = Just 33, month = Just 1, year = Just 2025 }

                        timeParts =
                            { hour = Just 12, minute = Just 12 }
                    in
                    Expect.equal (DateInput.catchError config inputValue dateParts timeParts)
                        (Just DateInput.ValueInvalid)
            , test "returns `ValueInvalid` error with invalid time parts" <|
                \_ ->
                    let
                        settings =
                            { format = DateInput.DateTime dateFormat timeFormat
                            , getErrorMessage = \_ -> ""
                            }

                        defaultConfig =
                            DateInput.defaultConfig timeZone

                        config =
                            { defaultConfig
                                | dateInputSettings = settings
                                , isDayDisabled = \_ _ -> False
                            }

                        inputValue =
                            "30/01/2025 12:72"

                        dateParts =
                            { day = Just 30, month = Just 1, year = Just 2025 }

                        timeParts =
                            { hour = Just 12, minute = Just 72 }
                    in
                    Expect.equal (DateInput.catchError config inputValue dateParts timeParts)
                        (Just DateInput.ValueInvalid)
            , test "returns `ValueNotAllowed` error when day is disabled" <|
                \_ ->
                    let
                        settings =
                            { format = DateInput.DateTime dateFormat timeFormat
                            , getErrorMessage = \_ -> ""
                            }

                        defaultConfig =
                            DateInput.defaultConfig timeZone

                        config =
                            { defaultConfig
                                | dateInputSettings = settings
                                , isDayDisabled = \_ _ -> True
                            }

                        inputValue =
                            "30/01/2025 12:12"

                        dateParts =
                            { day = Just 30, month = Just 1, year = Just 2025 }

                        timeParts =
                            { hour = Just 12, minute = Just 12 }
                    in
                    Expect.equal (DateInput.catchError config inputValue dateParts timeParts)
                        (Just DateInput.ValueNotAllowed)
            , test "returns `ValueNotAllowed` error when time is out of allowed bounds" <|
                \_ ->
                    let
                        settings =
                            { format = DateInput.DateTime dateFormat timeFormat
                            , getErrorMessage = \_ -> ""
                            }

                        defaultConfig =
                            DateInput.defaultConfig timeZone

                        config =
                            { defaultConfig
                                | dateInputSettings = settings
                                , isDayDisabled = \_ _ -> False
                            }

                        inputValue =
                            "30/01/2025 16:12"

                        dateParts =
                            { day = Just 30, month = Just 1, year = Just 2025 }

                        timeParts =
                            { hour = Just 16, minute = Just 12 }
                    in
                    Expect.equal (DateInput.catchError config inputValue dateParts timeParts)
                        (Just DateInput.ValueNotAllowed)
            ]
        ]
