module UtilitiesTest exposing (suite)

import DatePicker.Utilities as Utilities exposing (timeOfDayFromPosix)
import Expect
import Html exposing (option, text)
import Html.Attributes exposing (selected, value)
import Test exposing (..)
import Time exposing (Month(..), Posix)
import Time.Extra as Time exposing (Parts, partsToPosix)


suite : Test
suite =
    let
        timeZone =
            Time.utc
    in
    describe "DatePicker.Utilities"
        [ describe "monthData"
            [ test "generates data for parent month of provided posix split by week" <|
                \_ ->
                    let
                        result =
                            Utilities.monthData timeZone (\zone posix -> False) Nothing (Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0))

                        generatedDaysCount =
                            List.concat result
                                |> List.foldl
                                    (\day dayCounter ->
                                        if Time.toMonth timeZone day.start == Dec && Time.toYear timeZone day.start == 2020 then
                                            let
                                                decDays =
                                                    dayCounter.decDays
                                            in
                                            { dayCounter | decDays = decDays + 1 }

                                        else if Time.toMonth timeZone day.start == Jan && Time.toYear timeZone day.start == 2021 then
                                            let
                                                janDays =
                                                    dayCounter.janDays
                                            in
                                            { dayCounter | janDays = janDays + 1 }

                                        else if Time.toMonth timeZone day.start == Feb && Time.toYear timeZone day.start == 2021 then
                                            let
                                                febDays =
                                                    dayCounter.febDays
                                            in
                                            { dayCounter | febDays = febDays + 1 }

                                        else
                                            dayCounter
                                    )
                                    { decDays = 0, janDays = 0, febDays = 0 }
                    in
                    Expect.equal
                        generatedDaysCount
                        { decDays = 5, janDays = 31, febDays = 6 }
            , test "takes disabled function into account" <|
                \_ ->
                    let
                        result =
                            Utilities.monthData timeZone (\_ _ -> True) Nothing (Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0))

                        areAllDaysDisabled =
                            List.concat result
                                |> List.map (\day -> day.disabled)
                                |> List.all ((==) True)
                    in
                    Expect.equal
                        True
                        areAllDaysDisabled
            , test "takes allowable times function into account" <|
                \_ ->
                    let
                        result =
                            Utilities.monthData
                                timeZone
                                (\_ _ -> False)
                                (Just (\_ _ -> { startHour = 9, startMinute = 30, endHour = 17, endMinute = 30 }))
                                (Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0))

                        areAllowableTimesAsExpected =
                            List.concat result
                                |> List.map
                                    (\day ->
                                        let
                                            startTime =
                                                timeOfDayFromPosix timeZone day.start

                                            endTime =
                                                timeOfDayFromPosix timeZone day.end
                                        in
                                        startTime == ( 9, 30 ) && endTime == ( 17, 30 )
                                    )
                                |> List.all ((==) True)
                    in
                    Expect.equal
                        True
                        areAllowableTimesAsExpected
            ]
        , describe "generateHourOptions"
            [ test "generates a list of html options for every hour in the provided list." <|
                \_ ->
                    Expect.equal
                        (Utilities.generateHourOptions timeZone Nothing (List.range 0 3))
                        [ option [ value "0", selected False ] [ text "00" ]
                        , option [ value "1", selected False ] [ text "01" ]
                        , option [ value "2", selected False ] [ text "02" ]
                        , option [ value "3", selected False ] [ text "03" ]
                        ]
            , test "if there is a selection and an hour option matches the selected hour, option is marked as selected." <|
                \_ ->
                    let
                        selectionTuple =
                            ( { start = Time.partsToPosix Time.utc (Parts 2021 Jan 1 0 0 0 0)
                              , end = Time.partsToPosix Time.utc (Parts 2021 Jan 1 23 59 0 0)
                              , disabled = False
                              }
                            , Time.partsToPosix Time.utc (Parts 2021 Jan 1 0 0 0 0)
                            )
                    in
                    Expect.equal
                        (Utilities.generateHourOptions
                            timeZone
                            (Just selectionTuple)
                            (List.range 0 3)
                        )
                        [ option [ value "0", selected True ] [ text "00" ]
                        , option [ value "1", selected False ] [ text "01" ]
                        , option [ value "2", selected False ] [ text "02" ]
                        , option [ value "3", selected False ] [ text "03" ]
                        ]
            ]
        , describe "generateMinuteOptions"
            [ test "generates a list of html options for every minute in the provided list." <|
                \_ ->
                    Expect.equal
                        (Utilities.generateMinuteOptions timeZone Nothing (List.range 0 3))
                        [ option [ value "0", selected False ] [ text "00" ]
                        , option [ value "1", selected False ] [ text "01" ]
                        , option [ value "2", selected False ] [ text "02" ]
                        , option [ value "3", selected False ] [ text "03" ]
                        ]
            , test "if there is a selection and a minute option matches the selected minute, option is marked as selected." <|
                \_ ->
                    let
                        selectionTuple =
                            ( { start = Time.partsToPosix Time.utc (Parts 2021 Jan 1 0 0 0 0)
                              , end = Time.partsToPosix Time.utc (Parts 2021 Jan 1 23 59 0 0)
                              , disabled = False
                              }
                            , Time.partsToPosix Time.utc (Parts 2021 Jan 1 0 0 0 0)
                            )
                    in
                    Expect.equal
                        (Utilities.generateMinuteOptions
                            timeZone
                            (Just selectionTuple)
                            (List.range 0 3)
                        )
                        [ option [ value "0", selected True ] [ text "00" ]
                        , option [ value "1", selected False ] [ text "01" ]
                        , option [ value "2", selected False ] [ text "02" ]
                        , option [ value "3", selected False ] [ text "03" ]
                        ]
            ]
        , describe "pickerDayFromPosix"
            [ test "takes disabled function into account" <|
                \_ ->
                    Expect.equal
                        (Utilities.pickerDayFromPosix timeZone (\_ _ -> True) Nothing (Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0)))
                        { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0)
                        , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 23 59 0 0)
                        , disabled = True
                        }
            , test "takes allowable times function into account or defaults to start and end of posix's parent day" <|
                \_ ->
                    let
                        allowableTimesFn =
                            \_ _ -> { startHour = 9, startMinute = 30, endHour = 17, endMinute = 30 }
                    in
                    Expect.equal
                        [ Utilities.pickerDayFromPosix timeZone (\_ _ -> False) Nothing (Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0))
                        , Utilities.pickerDayFromPosix timeZone (\_ _ -> False) (Just allowableTimesFn) (Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0))
                        ]
                        [ { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0)
                          , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 23 59 0 0)
                          , disabled = False
                          }
                        , { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                          , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                          , disabled = False
                          }
                        ]
            ]
        , describe "timeOfDayFromPosix"
            [ test "returns a tuple containing the hour and minute of the provided posix" <|
                \_ ->
                    Expect.equal
                        (Utilities.timeOfDayFromPosix timeZone (Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)))
                        ( 9, 30 )
            ]
        , describe "setTimeOfDay"
            [ test "updates the hour and minute of the provided Posix" <|
                \_ ->
                    let
                        dateTimeToUpdate =
                            Time.partsToPosix timeZone (Parts 2019 Sep 19 0 0 0 0)

                        expectedResult =
                            Time.partsToPosix timeZone (Parts 2019 Sep 19 12 30 0 0)
                    in
                    Expect.equal (Utilities.setTimeOfDay timeZone 12 30 dateTimeToUpdate) expectedResult
            ]
        , describe "setHourNotDay"
            [ test "updates only the hour of the provided Posix" <|
                \_ ->
                    let
                        dateTimeToUpdate =
                            Time.partsToPosix timeZone (Parts 2019 Sep 19 0 0 0 0)

                        expectedResult =
                            Time.partsToPosix timeZone (Parts 2019 Sep 19 12 0 0 0)
                    in
                    Expect.equal (Utilities.setHourNotDay timeZone 12 dateTimeToUpdate) expectedResult
            ]
        , describe "setMinuteNotDay"
            [ test "updates only the minute of the provided Posix" <|
                \_ ->
                    let
                        dateTimeToUpdate =
                            Time.partsToPosix timeZone (Parts 2019 Sep 19 0 0 0 0)

                        expectedResult =
                            Time.partsToPosix timeZone (Parts 2019 Sep 19 0 30 0 0)
                    in
                    Expect.equal (Utilities.setMinuteNotDay timeZone 30 dateTimeToUpdate) expectedResult
            ]
        , describe "calculateViewOffset"
            [ test "returns the offset/difference in months between the reference/base time and the subject/selected time when subject is provided or defaults to 0." <|
                \_ ->
                    Expect.equal
                        [ Utilities.calculateViewOffset
                            timeZone
                            (Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0))
                            (Just (Time.partsToPosix timeZone (Parts 2021 Feb 1 0 0 0 0)))
                        , Utilities.calculateViewOffset
                            timeZone
                            (Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0))
                            (Just (Time.partsToPosix timeZone (Parts 2020 Dec 1 0 0 0 0)))
                        , Utilities.calculateViewOffset
                            timeZone
                            (Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0))
                            (Just (Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0)))
                        , Utilities.calculateViewOffset
                            timeZone
                            (Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0))
                            Nothing
                        ]
                        [ 1, -1, 0, 0 ]
            ]
        , describe "hourBoundsForSelectedDay"
            [ test "returns the earliest and latest selectable hour for the given picker day" <|
                \_ ->
                    let
                        pickerDay1 =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 23 59 0 0)
                            , disabled = False
                            }

                        pickerDay2 =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                            , disabled = False
                            }
                    in
                    Expect.equal
                        [ Utilities.hourBoundsForSelectedDay timeZone pickerDay1
                        , Utilities.hourBoundsForSelectedDay timeZone pickerDay2
                        ]
                        [ ( 0, 23 )
                        , ( 9, 17 )
                        ]
            ]
        , describe "minuteBoundsForSelectedHour"
            [ describe "when hour is day start bound"
                [ test "returns the specified picker day start minute to end of hour" <|
                    \_ ->
                        let
                            pickerDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (Utilities.minuteBoundsForSelectedHour timeZone ( pickerDay, pickerDay.start ))
                            ( 30, 59 )
                ]
            , describe "when hour is day end bound"
                [ test "returns the start of the hour to the specified picker day end minute" <|
                    \_ ->
                        let
                            pickerDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (Utilities.minuteBoundsForSelectedHour timeZone ( pickerDay, pickerDay.end ))
                            ( 0, 30 )
                ]
            , describe "when hour is both start and end bound"
                [ test "returns the specified picker day start minute to the specified picker day end minute" <|
                    \_ ->
                        let
                            pickerDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 12 15 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 12 45 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (Utilities.minuteBoundsForSelectedHour timeZone ( pickerDay, pickerDay.start ))
                            ( 15, 45 )
                ]
            , describe "when hour is not a boundary"
                [ test "returns the whole hour" <|
                    \_ ->
                        let
                            pickerDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (Utilities.minuteBoundsForSelectedHour timeZone ( pickerDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 10 0 0 0) ))
                            ( 0, 59 )
                ]
            ]
        , describe "posixWithinPickerDayBoundaries"
            [ test "returns True when posix time of day is within picker day allowable time boundaries, otherwise returns False" <|
                \_ ->
                    let
                        pickerDay =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                            , disabled = False
                            }
                    in
                    Expect.equal
                        [ Utilities.posixWithinPickerDayBoundaries timeZone pickerDay (Time.partsToPosix timeZone (Parts 2021 Jan 1 8 0 0 0))
                        , Utilities.posixWithinPickerDayBoundaries timeZone pickerDay pickerDay.start
                        , Utilities.posixWithinPickerDayBoundaries timeZone pickerDay (Time.partsToPosix timeZone (Parts 2021 Jan 1 12 0 0 0))
                        , Utilities.posixWithinPickerDayBoundaries timeZone pickerDay pickerDay.end
                        , Utilities.posixWithinPickerDayBoundaries timeZone pickerDay (Time.partsToPosix timeZone (Parts 2021 Jan 1 18 0 0 0))
                        ]
                        [ False, True, True, True, False ]
            , test "returns True when posix time of day is within picker day allowable time boundaries even if posix day and picker day are different" <|
                \_ ->
                    let
                        pickerDay =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                            , disabled = False
                            }
                    in
                    Expect.equal
                        (Utilities.posixWithinPickerDayBoundaries timeZone pickerDay (Time.partsToPosix timeZone (Parts 2021 Jan 2 12 0 0 0)))
                        True
            ]
        , describe "validSelectionOrDefault"
            [ test "returns provided default if selection time is not within day bounds" <|
                \_ ->
                    let
                        pickerDay =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                            , disabled = False
                            }

                        invalidSelection =
                            ( pickerDay
                            , Time.partsToPosix timeZone (Parts 2021 Jan 1 8 30 0 0)
                            )
                    in
                    Expect.equal
                        (Utilities.validSelectionOrDefault timeZone Nothing invalidSelection)
                        Nothing
            , test "returns provided default if selection day is disabled" <|
                \_ ->
                    let
                        pickerDay =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                            , disabled = True
                            }

                        invalidSelection =
                            ( pickerDay
                            , Time.partsToPosix timeZone (Parts 2021 Jan 1 8 30 0 0)
                            )
                    in
                    Expect.equal
                        (Utilities.validSelectionOrDefault timeZone Nothing invalidSelection)
                        Nothing
            , test "returns provided default if pickerDay of selection tuple is not same as parent day of selection" <|
                \_ ->
                    let
                        pickerDay =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                            , disabled = False
                            }

                        invalidSelection =
                            ( pickerDay
                            , Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                            )
                    in
                    Expect.equal
                        (Utilities.validSelectionOrDefault timeZone Nothing invalidSelection)
                        Nothing
            , test "returns valid selection" <|
                \_ ->
                    let
                        pickerDay =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                            , disabled = False
                            }

                        validSelection =
                            ( pickerDay
                            , Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                            )
                    in
                    Expect.equal
                        (Utilities.validSelectionOrDefault timeZone Nothing validSelection)
                        (Just validSelection)
            ]
        ]
