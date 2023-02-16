module DurationUtilitiesTest exposing (suite)

import DatePicker.DurationUtilities as DurationUtilities
import Expect
import Test exposing (..)
import Time exposing (Month(..))
import Time.Extra as Time exposing (Parts, partsToPosix)


suite : Test
suite =
    let
        timeZone =
            Time.utc
    in
    describe "DatePicker.DurationUtilities"
        [ describe "selectDay"
            [ describe "when provided day is disabled"
                [ test "if provided day is disabled: returns prior selections" <|
                    \_ ->
                        let
                            pickerDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                , disabled = True
                                }

                            startEndDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( startEndDay
                                , startEndDay.start
                                )

                            priorEndSelection =
                                ( startEndDay
                                , startEndDay.end
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectDay timeZone (Just priorStartSelection) (Just priorEndSelection) pickerDay)
                            ( Just priorStartSelection, Just priorEndSelection )
                ]
            , describe "with prior start and end selection"
                [ test "if selected start & end day are the same and provided day matches them: deselects selected end" <|
                    \_ ->
                        let
                            pickerDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( pickerDay
                                , pickerDay.start
                                )

                            priorEndSelection =
                                ( pickerDay
                                , pickerDay.end
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectDay timeZone (Just priorStartSelection) (Just priorEndSelection) pickerDay)
                            ( Just priorStartSelection, Nothing )
                , test "if provided day matches selected start day: deselects selected start" <|
                    \_ ->
                        let
                            pickerDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            endDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( pickerDay
                                , pickerDay.start
                                )

                            priorEndSelection =
                                ( endDay
                                , endDay.end
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectDay timeZone (Just priorStartSelection) (Just priorEndSelection) pickerDay)
                            ( Nothing, Just priorEndSelection )
                , test "if provided day matches selected end day: deselects selected end" <|
                    \_ ->
                        let
                            pickerDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                , disabled = False
                                }

                            startDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( startDay
                                , startDay.start
                                )

                            priorEndSelection =
                                ( pickerDay
                                , pickerDay.end
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectDay timeZone (Just priorStartSelection) (Just priorEndSelection) pickerDay)
                            ( Just priorStartSelection, Nothing )
                , test "if provided day matches neither start nor end: sets provided day as start and deselects end" <|
                    \_ ->
                        let
                            pickerDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                , disabled = False
                                }

                            startEndDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( startEndDay
                                , startEndDay.start
                                )

                            priorEndSelection =
                                ( startEndDay
                                , startEndDay.end
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectDay timeZone (Just priorStartSelection) (Just priorEndSelection) pickerDay)
                            ( Just ( pickerDay, pickerDay.start ), Nothing )
                ]
            , describe "with only prior start selection"
                [ describe "when provided day is less than selected start day"
                    [ test "provided day becomes new start and prior start time transferred to new start if prior start time selectable in provided day." <|
                        \_ ->
                            let
                                pickerDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                    , disabled = False
                                    }

                                startDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                    , disabled = False
                                    }

                                priorStartSelection =
                                    ( startDay
                                    , Time.partsToPosix timeZone (Parts 2021 Jan 2 10 0 0 0)
                                    )
                            in
                            Expect.equal
                                (DurationUtilities.selectDay timeZone (Just priorStartSelection) Nothing pickerDay)
                                ( Just ( pickerDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 10 0 0 0) ), Just ( startDay, startDay.end ) )
                    , test "provided day becomes new start and start time is provided day start if prior start time not selectable in provided day." <|
                        \_ ->
                            let
                                pickerDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                    , disabled = False
                                    }

                                startDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 0 0 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 23 59 0 0)
                                    , disabled = False
                                    }

                                priorStartSelection =
                                    ( startDay
                                    , startDay.start
                                    )
                            in
                            Expect.equal
                                (DurationUtilities.selectDay timeZone (Just priorStartSelection) Nothing pickerDay)
                                ( Just ( pickerDay, pickerDay.start ), Just ( startDay, startDay.end ) )
                    ]
                , describe "when provided day is greater than or equal to selected start day"
                    [ test "provided day becomes end selection." <|
                        \_ ->
                            let
                                pickerDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                    , disabled = False
                                    }

                                startDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                    , disabled = False
                                    }

                                priorStartSelection =
                                    ( startDay
                                    , startDay.start
                                    )
                            in
                            Expect.equal
                                [ DurationUtilities.selectDay timeZone (Just priorStartSelection) Nothing startDay
                                , DurationUtilities.selectDay timeZone (Just priorStartSelection) Nothing pickerDay
                                ]
                                [ ( Just ( startDay, startDay.start ), Just ( startDay, startDay.end ) )
                                , ( Just ( startDay, startDay.start ), Just ( pickerDay, pickerDay.end ) )
                                ]
                    ]
                ]
            , describe "with only prior end selection"
                [ describe "when provided day is greater than selected end day"
                    [ test "provided day becomes new end and prior end time transferred to new end if prior end time selectable in provided day." <|
                        \_ ->
                            let
                                pickerDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                    , disabled = False
                                    }

                                endDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                    , disabled = False
                                    }

                                priorEndSelection =
                                    ( endDay
                                    , Time.partsToPosix timeZone (Parts 2021 Jan 2 16 0 0 0)
                                    )
                            in
                            Expect.equal
                                (DurationUtilities.selectDay timeZone Nothing (Just priorEndSelection) pickerDay)
                                ( Just ( endDay, endDay.start ), Just ( pickerDay, Time.partsToPosix timeZone (Parts 2021 Jan 2 16 0 59 0) ) )
                    , test "provided day becomes new end and end time is provided day end if prior end time not selectable in provided day." <|
                        \_ ->
                            let
                                pickerDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                    , disabled = False
                                    }

                                endDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 0 0 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 23 59 0 0)
                                    , disabled = False
                                    }

                                priorEndSelection =
                                    ( endDay
                                    , endDay.end
                                    )
                            in
                            Expect.equal
                                (DurationUtilities.selectDay timeZone Nothing (Just priorEndSelection) pickerDay)
                                ( Just ( endDay, endDay.start ), Just ( pickerDay, pickerDay.end ) )
                    ]
                , describe "when provided day is less than or equal to selected end day"
                    [ test "provided day becomes start selection." <|
                        \_ ->
                            let
                                pickerDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                    , disabled = False
                                    }

                                endDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                    , disabled = False
                                    }

                                priorEndSelection =
                                    ( endDay
                                    , endDay.end
                                    )
                            in
                            Expect.equal
                                [ DurationUtilities.selectDay timeZone Nothing (Just priorEndSelection) endDay
                                , DurationUtilities.selectDay timeZone Nothing (Just priorEndSelection) pickerDay
                                ]
                                [ ( Just ( endDay, endDay.start )
                                  , Just ( endDay, endDay.end )
                                  )
                                , ( Just ( pickerDay, pickerDay.start )
                                  , Just ( endDay, endDay.end )
                                  )
                                ]
                    ]
                ]
            , describe "without prior selection"
                [ test "provided day becomes start selection." <|
                    \_ ->
                        let
                            pickerDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (DurationUtilities.selectDay timeZone Nothing Nothing pickerDay)
                            ( Just ( pickerDay, pickerDay.start ), Nothing )
                ]
            ]
        , describe "selectStartHour"
            [ describe "with a prior selected start"
                [ test "sets: start hour -> provided hour, start minute -> prior or earliest selectable minute for provided hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 10 15 0 0)
                                )
                        in
                        Expect.equal
                            [ DurationUtilities.selectStartHour timeZone baseDay (Just priorStartSelection) Nothing 9
                            , DurationUtilities.selectStartHour timeZone baseDay (Just priorStartSelection) Nothing 11
                            ]
                            [ ( Just ( baseDay, baseDay.start ), Nothing )
                            , ( Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 11 15 0 0) ), Nothing )
                            ]
                , test "returns prior selections if provided hour is outside selectable range of start day" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( baseDay
                                , baseDay.start
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectStartHour timeZone baseDay (Just priorStartSelection) Nothing 8)
                            ( Just priorStartSelection, Nothing )
                ]
            , describe "with only a prior selected end"
                [ test "sets: start day -> selected end day, start hour -> provided hour, start minute -> earliest selectable minute for provided hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorEndSelection =
                                ( baseDay
                                , baseDay.end
                                )
                        in
                        Expect.equal
                            [ DurationUtilities.selectStartHour timeZone baseDay Nothing (Just priorEndSelection) 9
                            , DurationUtilities.selectStartHour timeZone baseDay Nothing (Just priorEndSelection) 11
                            ]
                            [ ( Just ( baseDay, baseDay.start ), Just priorEndSelection )
                            , ( Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 11 0 0 0) ), Just priorEndSelection )
                            ]
                , test "returns prior selections if provided hour is outside selectable range of end day" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorEndSelection =
                                ( baseDay
                                , baseDay.end
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectStartHour timeZone baseDay Nothing (Just priorEndSelection) 8)
                            ( Nothing, Just priorEndSelection )
                ]
            , describe "with no prior selection"
                [ test "sets: start day -> base day, start hour -> provided hour, start minute -> earliest selectable minute for provided hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            [ DurationUtilities.selectStartHour timeZone baseDay Nothing Nothing 9
                            , DurationUtilities.selectStartHour timeZone baseDay Nothing Nothing 11
                            ]
                            [ ( Just ( baseDay, baseDay.start ), Nothing )
                            , ( Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 11 0 0 0) ), Nothing )
                            ]
                , test "returns prior selections if provided hour is outside selectable range of base day" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (DurationUtilities.selectStartHour timeZone baseDay Nothing Nothing 8)
                            ( Nothing, Nothing )
                ]
            ]
        , describe "selectEndHour"
            [ describe "with a prior selected end"
                [ test "sets: end hour -> provided hour, end minute -> prior or latest selectable minute for provided hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorEndSelection =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 16 45 0 0)
                                )
                        in
                        Expect.equal
                            [ DurationUtilities.selectEndHour timeZone baseDay Nothing (Just priorEndSelection) 17
                            , DurationUtilities.selectEndHour timeZone baseDay Nothing (Just priorEndSelection) 15
                            ]
                            [ ( Nothing, Just ( baseDay, baseDay.end ) )
                            , ( Nothing, Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 15 45 0 0) ) )
                            ]
                , test "returns prior selections if provided hour is outside selectable range of end day" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorEndSelection =
                                ( baseDay
                                , baseDay.end
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectEndHour timeZone baseDay Nothing (Just priorEndSelection) 18)
                            ( Nothing, Just priorEndSelection )
                ]
            , describe "with only a prior selected start"
                [ test "sets: end day -> selected start day, end hour -> provided hour, end minute -> latest selectable minute for provided hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( baseDay
                                , baseDay.start
                                )
                        in
                        Expect.equal
                            [ DurationUtilities.selectEndHour timeZone baseDay (Just priorStartSelection) Nothing 17
                            , DurationUtilities.selectEndHour timeZone baseDay (Just priorStartSelection) Nothing 16
                            ]
                            [ ( Just priorStartSelection, Just ( baseDay, baseDay.end ) )
                            , ( Just priorStartSelection, Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 16 59 0 0) ) )
                            ]
                , test "returns prior selections if provided hour is outside selectable range of start day" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( baseDay
                                , baseDay.start
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectEndHour timeZone baseDay (Just priorStartSelection) Nothing 18)
                            ( Just priorStartSelection, Nothing )
                ]
            , describe "with no prior selection"
                [ test "sets: end day -> base day, end hour -> provided hour, end minute -> latest selectable minute for provided hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            [ DurationUtilities.selectEndHour timeZone baseDay Nothing Nothing 17
                            , DurationUtilities.selectEndHour timeZone baseDay Nothing Nothing 16
                            ]
                            [ ( Nothing, Just ( baseDay, baseDay.end ) )
                            , ( Nothing, Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 16 59 0 0) ) )
                            ]
                , test "returns prior selections if provided hour is outside selectable range of base day" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (DurationUtilities.selectEndHour timeZone baseDay Nothing Nothing 18)
                            ( Nothing, Nothing )
                ]
            ]
        , describe "selectStartMinute"
            [ describe "with a prior selected start"
                [ test "sets: start minute -> provided minute" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 10 15 0 0)
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectStartMinute timeZone baseDay (Just priorStartSelection) Nothing 30)
                            ( Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 10 30 0 0) ), Nothing )
                , test "returns prior selections if provided minute is outside selectable range of start hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( baseDay
                                , baseDay.start
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectStartMinute timeZone baseDay (Just priorStartSelection) Nothing 15)
                            ( Just priorStartSelection, Nothing )
                ]
            , describe "with only a prior selected end"
                [ test "sets: start day -> end day, start hour -> end day start hour, start minute -> provided minute" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorEndSelection =
                                ( baseDay
                                , baseDay.end
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectStartMinute timeZone baseDay Nothing (Just priorEndSelection) 45)
                            ( Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 9 45 0 0) ), Just priorEndSelection )
                , test "returns prior selections if provided minute is outside selectable range of end day start hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorEndSelection =
                                ( baseDay
                                , baseDay.end
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectStartMinute timeZone baseDay Nothing (Just priorEndSelection) 15)
                            ( Nothing, Just priorEndSelection )
                ]
            , describe "with no prior selection"
                [ test "sets: start day -> base day, start hour -> base day start hour, start minute -> provided minute" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (DurationUtilities.selectStartMinute timeZone baseDay Nothing Nothing 45)
                            ( Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 9 45 0 0) ), Nothing )
                , test "returns prior selections if provided minute is outside selectable range of base day start hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (DurationUtilities.selectStartMinute timeZone baseDay Nothing Nothing 15)
                            ( Nothing, Nothing )
                ]
            ]
        , describe "selectEndMinute"
            [ describe "with a prior selected end"
                [ test "sets: end minute -> provided minute" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorEndSelection =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 16 45 0 0)
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectEndMinute timeZone baseDay Nothing (Just priorEndSelection) 59)
                            ( Nothing, Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 16 59 0 0) ) )
                , test "returns prior selections if provided minute is outside selectable range of end hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorEndSelection =
                                ( baseDay
                                , baseDay.end
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectEndMinute timeZone baseDay Nothing (Just priorEndSelection) 45)
                            ( Nothing, Just priorEndSelection )
                ]
            , describe "with only a prior selected start"
                [ test "sets: end day -> start day, end hour -> start day end hour, end minute -> provided minute" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( baseDay
                                , baseDay.start
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectEndMinute timeZone baseDay (Just priorStartSelection) Nothing 15)
                            ( Just priorStartSelection, Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 17 15 0 0) ) )
                , test "returns prior selections if provided minute is outside selectable range of start day end hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorStartSelection =
                                ( baseDay
                                , baseDay.start
                                )
                        in
                        Expect.equal
                            (DurationUtilities.selectEndMinute timeZone baseDay (Just priorStartSelection) Nothing 45)
                            ( Just priorStartSelection, Nothing )
                ]
            , describe "with no prior selection"
                [ test "sets: end day -> base day, end hour -> base day end hour, end minute -> provided minute" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (DurationUtilities.selectEndMinute timeZone baseDay Nothing Nothing 15)
                            ( Nothing, Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 17 15 0 0) ) )
                , test "returns prior selections if provided hour is outside selectable range of base day end hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (DurationUtilities.selectEndMinute timeZone baseDay Nothing Nothing 45)
                            ( Nothing, Nothing )
                ]
            ]
        , describe "filterSelectableTimes"
            [ describe "with prior start and end selection"
                [ describe "when start and end are same day"
                    [ describe "when start and end are same hour"
                        [ test "start hours: earliest selectable hour - selected end hour, start mins: hour start bound - selected end min, end hours: selected start hour - latest selectable hour, end mins: selected start min - hour end bound" <|
                            \_ ->
                                let
                                    baseDay =
                                        { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                        , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                        , disabled = False
                                        }

                                    -- because the selected minute is less than 30, 9 will not be selectable
                                    -- start hour
                                    startSelection =
                                        ( baseDay
                                        , Time.partsToPosix timeZone (Parts 2021 Jan 1 13 15 0 0)
                                        )

                                    endSelection =
                                        ( baseDay
                                        , Time.partsToPosix timeZone (Parts 2021 Jan 1 13 30 0 0)
                                        )
                                in
                                Expect.equal
                                    (DurationUtilities.filterSelectableTimes timeZone baseDay (Just startSelection) (Just endSelection))
                                    { selectableStartHours = List.range 10 13
                                    , selectableStartMinutes = List.range 0 30
                                    , selectableEndHours = List.range 13 17
                                    , selectableEndMinutes = List.range 15 59
                                    }
                        ]
                    , describe "when start and end are different hours"
                        [ test "start hours: earliest selectable hour - selected end hour, start mins: start hour start bound - start hour end bound, end hours: selected start hour - latest selectable hour, end mins: end hour start bound - end hour end bound" <|
                            \_ ->
                                let
                                    baseDay =
                                        { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                        , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                        , disabled = False
                                        }

                                    -- because the selected minute is less than 30, 9 will not be selectable
                                    -- start hour
                                    startSelection =
                                        ( baseDay
                                        , Time.partsToPosix timeZone (Parts 2021 Jan 1 13 0 0 0)
                                        )

                                    endSelection =
                                        ( baseDay
                                        , Time.partsToPosix timeZone (Parts 2021 Jan 1 15 30 0 0)
                                        )
                                in
                                Expect.equal
                                    (DurationUtilities.filterSelectableTimes timeZone baseDay (Just startSelection) (Just endSelection))
                                    { selectableStartHours = List.range 10 15
                                    , selectableStartMinutes = List.range 0 59
                                    , selectableEndHours = List.range 13 17
                                    , selectableEndMinutes = List.range 0 59
                                    }
                        ]
                    ]
                , describe "when start and end are different day"
                    [ test "start hours: earliest selectable hour - lateset selectable hour, start mins: start hour start bound - start hour end bound, end hours: earliest selectable hour - lateset selectable hour, end mins: end hour start bound - end hour end bound" <|
                        \_ ->
                            let
                                baseDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                    , disabled = False
                                    }

                                startDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                    , disabled = False
                                    }

                                endDay =
                                    { start = Time.partsToPosix timeZone (Parts 2021 Jan 3 9 30 0 0)
                                    , end = Time.partsToPosix timeZone (Parts 2021 Jan 3 17 30 0 0)
                                    , disabled = False
                                    }

                                startSelection1 =
                                    ( startDay
                                    , startDay.start
                                    )

                                -- because the selected minute is less than 30, 9 will not be selectable
                                -- start hour
                                startSelection2 =
                                    ( startDay
                                    , Time.partsToPosix timeZone (Parts 2021 Jan 1 13 0 0 0)
                                    )

                                endSelection1 =
                                    ( endDay
                                    , endDay.end
                                    )

                                -- because the selected minute is more than 30, 17 will not be selectable
                                -- end hour
                                endSelection2 =
                                    ( baseDay
                                    , Time.partsToPosix timeZone (Parts 2021 Jan 1 15 45 0 0)
                                    )
                            in
                            Expect.equal
                                [ DurationUtilities.filterSelectableTimes timeZone baseDay (Just startSelection1) (Just endSelection1)
                                , DurationUtilities.filterSelectableTimes timeZone baseDay (Just startSelection2) (Just endSelection2)
                                ]
                                [ { selectableStartHours = List.range 9 17
                                  , selectableStartMinutes = List.range 30 59
                                  , selectableEndHours = List.range 9 17
                                  , selectableEndMinutes = List.range 0 30
                                  }
                                , { selectableStartHours = List.range 10 17
                                  , selectableStartMinutes = List.range 0 59
                                  , selectableEndHours = List.range 9 16
                                  , selectableEndMinutes = List.range 0 59
                                  }
                                ]
                    ]
                ]
            , describe "with only prior start selection"
                [ test "start hours: earliest selectable hour - latest selectable hour, start mins: start hour start bound - start hour end bound, end hours: selected start hour - lateset selectable hour, end mins: selected start min - start hour end bound" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            startSelection1 =
                                ( baseDay
                                , baseDay.start
                                )

                            startSelection2 =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 13 30 0 0)
                                )

                            -- because the selected minute is less than 30, 9 will not be selectable
                            -- start hour
                            startSelection3 =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 17 0 0 0)
                                )

                            -- because the selected minute is more than 30, 17 will not be selectable
                            -- start/end hour
                            startSelection4 =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 13 45 0 0)
                                )
                        in
                        Expect.equal
                            [ DurationUtilities.filterSelectableTimes timeZone baseDay (Just startSelection1) Nothing
                            , DurationUtilities.filterSelectableTimes timeZone baseDay (Just startSelection2) Nothing
                            , DurationUtilities.filterSelectableTimes timeZone baseDay (Just startSelection3) Nothing
                            , DurationUtilities.filterSelectableTimes timeZone baseDay (Just startSelection4) Nothing
                            ]
                            [ { selectableStartHours = List.range 9 17
                              , selectableStartMinutes = List.range 30 59
                              , selectableEndHours = List.range 9 17
                              , selectableEndMinutes = List.range 30 59
                              }
                            , { selectableStartHours = List.range 9 17
                              , selectableStartMinutes = List.range 0 59
                              , selectableEndHours = List.range 13 17
                              , selectableEndMinutes = List.range 30 59
                              }
                            , { selectableStartHours = List.range 10 17
                              , selectableStartMinutes = List.range 0 30
                              , selectableEndHours = List.range 17 17
                              , selectableEndMinutes = List.range 0 30
                              }
                            , { selectableStartHours = List.range 9 16
                              , selectableStartMinutes = List.range 0 59
                              , selectableEndHours = List.range 13 16
                              , selectableEndMinutes = List.range 45 59
                              }
                            ]
                ]
            , describe "with only prior end selection"
                [ test "start hours: earliest selectable hour - selected end hour, start mins: end hour start bound - selected end min, end hours: earliest selectable hour - latest selectable hour, end mins: end hour start bound - end hour end bound" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            endSelection1 =
                                ( baseDay
                                , baseDay.end
                                )

                            endSelection2 =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 13 30 0 0)
                                )

                            -- because the selected minute is more than 30, 17 will not be selectable
                            -- end hour
                            endSelection3 =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 9 59 0 0)
                                )

                            -- because the selected minute is less than 30, 9 will not be selectable
                            -- start/end hour
                            endSelection4 =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 13 15 0 0)
                                )
                        in
                        Expect.equal
                            [ DurationUtilities.filterSelectableTimes timeZone baseDay Nothing (Just endSelection1)
                            , DurationUtilities.filterSelectableTimes timeZone baseDay Nothing (Just endSelection2)
                            , DurationUtilities.filterSelectableTimes timeZone baseDay Nothing (Just endSelection3)
                            , DurationUtilities.filterSelectableTimes timeZone baseDay Nothing (Just endSelection4)
                            ]
                            [ { selectableStartHours = List.range 9 17
                              , selectableStartMinutes = List.range 0 30
                              , selectableEndHours = List.range 9 17
                              , selectableEndMinutes = List.range 0 30
                              }
                            , { selectableStartHours = List.range 9 13
                              , selectableStartMinutes = List.range 0 30
                              , selectableEndHours = List.range 9 17
                              , selectableEndMinutes = List.range 0 59
                              }
                            , { selectableStartHours = List.range 9 9
                              , selectableStartMinutes = List.range 30 59
                              , selectableEndHours = List.range 9 16
                              , selectableEndMinutes = List.range 30 59
                              }
                            , { selectableStartHours = List.range 10 13
                              , selectableStartMinutes = List.range 0 15
                              , selectableEndHours = List.range 10 17
                              , selectableEndMinutes = List.range 0 59
                              }
                            ]
                ]
            , describe "with no prior selection"
                [ test "start hours: base day start bound - base day end bound, start mins: base day start hour start bound - base day start hour end bound, end hours: base day start bound - base day end bound, end mins: base day start hour start bound - base day start hour end bound" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (DurationUtilities.filterSelectableTimes timeZone baseDay Nothing Nothing)
                            { selectableStartHours = List.range 9 17
                            , selectableStartMinutes = List.range 30 59
                            , selectableEndHours = List.range 9 17
                            , selectableEndMinutes = List.range 30 59
                            }
                ]
            ]
        , describe "dayPickedOrBetween"
            [ describe "with prior start and end selection"
                [ test "selected days are marked as picked and days between are marked as between" <|
                    \_ ->
                        let
                            neitherSelectedNorBetween =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            startDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                , disabled = False
                                }

                            betweenDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 3 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 3 17 30 0 0)
                                , disabled = False
                                }

                            endDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 4 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 4 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            [ DurationUtilities.dayPickedOrBetween timeZone neitherSelectedNorBetween Nothing ( Just ( startDay, startDay.start ), Just ( endDay, endDay.end ) )
                            , DurationUtilities.dayPickedOrBetween timeZone startDay Nothing ( Just ( startDay, startDay.start ), Just ( endDay, endDay.end ) )
                            , DurationUtilities.dayPickedOrBetween timeZone betweenDay Nothing ( Just ( startDay, startDay.start ), Just ( endDay, endDay.end ) )
                            , DurationUtilities.dayPickedOrBetween timeZone endDay Nothing ( Just ( startDay, startDay.start ), Just ( endDay, endDay.end ) )
                            ]
                            [ ( False, False )
                            , ( True, False )
                            , ( False, True )
                            , ( True, False )
                            ]
                ]
            , describe "with prior start OR prior end And hovered day"
                [ test "selected day is marked as picked and days between selected and hovered day are marked as between" <|
                    \_ ->
                        let
                            neitherSelectedNorBetween =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            startDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                                , disabled = False
                                }

                            betweenDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 3 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 3 17 30 0 0)
                                , disabled = False
                                }

                            endDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 4 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 4 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            [ DurationUtilities.dayPickedOrBetween timeZone neitherSelectedNorBetween (Just endDay) ( Just ( startDay, startDay.start ), Nothing )
                            , DurationUtilities.dayPickedOrBetween timeZone startDay (Just endDay) ( Just ( startDay, startDay.start ), Nothing )
                            , DurationUtilities.dayPickedOrBetween timeZone betweenDay (Just endDay) ( Just ( startDay, startDay.start ), Nothing )
                            , DurationUtilities.dayPickedOrBetween timeZone endDay (Just endDay) ( Just ( startDay, startDay.start ), Nothing )
                            ]
                            [ ( False, False )
                            , ( True, False )
                            , ( False, True )
                            , ( False, False )
                            ]
                ]
            ]
        , describe "isPickedDaySelectionTuple"
            [ test "picked day is equal to startSelectionTuple and not equal to endSelectionTuple" <|
                \_ ->
                    let
                        pickedDay =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                            , disabled = False
                            }

                        startDay =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 2 9 30 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 2 17 30 0 0)
                            , disabled = False
                            }

                        endDay =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 4 9 30 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 4 17 30 0 0)
                            , disabled = False
                            }
                    in
                    Expect.equal
                        [ DurationUtilities.isPickedDaySelectionTuple pickedDay (Just ( startDay, startDay.start ))
                        , DurationUtilities.isPickedDaySelectionTuple pickedDay (Just ( endDay, endDay.start ))
                        ]
                        [ True
                        , False
                        ]
            ]
        ]
