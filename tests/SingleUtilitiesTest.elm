module SingleUtilitiesTest exposing (suite)

import DatePicker.SingleUtilities as SingleUtilities
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
    describe "DatePicker.SingleUtilities"
        [ describe "when provided day is disabled"
            [ test "if provided day is disabled: returns prior selections" <|
                \_ ->
                    let
                        pickerDay =
                            { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                            , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                            , disabled = True
                            }

                        priorSelectionTuple =
                            ( { start = Time.partsToPosix timeZone (Parts 2020 Dec 31 0 0 0 0)
                              , end = Time.partsToPosix timeZone (Parts 2020 Dec 31 23 59 0 0)
                              , disabled = False
                              }
                            , Time.partsToPosix timeZone (Parts 2020 Dec 31 12 0 0 0)
                            )
                    in
                    Expect.equal
                        (SingleUtilities.selectDay timeZone (Just priorSelectionTuple) pickerDay)
                        (Just priorSelectionTuple)
            ]
        , describe "selectDay"
            [ describe "without prior selection"
                [ test "selects the start time of the enclosing (selected) day" <|
                    \_ ->
                        let
                            selectedDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (SingleUtilities.selectDay timeZone Nothing selectedDay)
                            (Just ( selectedDay, selectedDay.start ))
                ]
            , describe "with prior selection"
                [ test "maintains the prior selection's time of day if it falls within the bounds of the new selected day" <|
                    \_ ->
                        let
                            selectedDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorSelectionTuple =
                                ( { start = Time.partsToPosix timeZone (Parts 2020 Dec 31 0 0 0 0)
                                  , end = Time.partsToPosix timeZone (Parts 2020 Dec 31 23 59 0 0)
                                  , disabled = False
                                  }
                                , Time.partsToPosix timeZone (Parts 2020 Dec 31 12 0 0 0)
                                )
                        in
                        Expect.equal
                            (SingleUtilities.selectDay timeZone (Just priorSelectionTuple) selectedDay)
                            (Just ( selectedDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 12 0 0 0) ))
                , test "selects the start of the new selected day when the prior selection's time of day does not fall within the bounds of the new selected day" <|
                    \_ ->
                        let
                            selectedDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorSelectedDay =
                                { start = Time.partsToPosix timeZone (Parts 2020 Dec 31 0 0 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2020 Dec 31 23 59 0 0)
                                , disabled = False
                                }

                            priorSelectionTuple1 =
                                ( priorSelectedDay
                                , priorSelectedDay.start
                                )

                            priorSelectionTuple2 =
                                ( priorSelectedDay
                                , priorSelectedDay.end
                                )
                        in
                        Expect.equal
                            [ SingleUtilities.selectDay timeZone (Just priorSelectionTuple1) selectedDay, SingleUtilities.selectDay timeZone (Just priorSelectionTuple2) selectedDay ]
                            [ Just ( selectedDay, selectedDay.start ), Just ( selectedDay, selectedDay.start ) ]
                ]
            ]
        , describe "selectHour"
            [ describe "without prior selection"
                [ test "selects: day -> base day, hour -> provided hour, minute -> earliest selectable minute for provided hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            [ SingleUtilities.selectHour timeZone baseDay Nothing 9, SingleUtilities.selectHour timeZone baseDay Nothing 10 ]
                            [ Just ( baseDay, baseDay.start ), Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 10 0 0 0) ) ]
                , test "returns Nothing if the provided hour is not selectable in base day" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            [ SingleUtilities.selectHour timeZone baseDay Nothing 8, SingleUtilities.selectHour timeZone baseDay Nothing 18 ]
                            [ Nothing, Nothing ]
                ]
            , describe "with prior selection"
                [ test "selects: day -> prior date, hour -> provided hour, minute -> prior or earliest selectable minute for provided hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorSelectionTuple =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 10 15 0 0)
                                )
                        in
                        Expect.equal
                            [ SingleUtilities.selectHour timeZone baseDay (Just priorSelectionTuple) 9, SingleUtilities.selectHour timeZone baseDay (Just priorSelectionTuple) 11 ]
                            [ Just ( baseDay, baseDay.start ), Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 11 15 0 0) ) ]
                , test "returns prior selection if the provided hour is not selectable in prior selection enclosing day" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorSelectionTuple =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 10 15 0 0)
                                )
                        in
                        Expect.equal
                            [ SingleUtilities.selectHour timeZone baseDay (Just priorSelectionTuple) 8, SingleUtilities.selectHour timeZone baseDay (Just priorSelectionTuple) 18 ]
                            [ Just priorSelectionTuple, Just priorSelectionTuple ]
                ]
            ]
        , describe "selectMinute"
            [ describe "without prior selection"
                [ test "selects: day -> base day, hour -> base day start hour, minute -> provided minute" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (SingleUtilities.selectMinute timeZone baseDay Nothing 45)
                            (Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 9 45 0 0) ))
                , test "returns Nothing if the provided minute is not selectable in base day start hour" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (SingleUtilities.selectMinute timeZone baseDay Nothing 15)
                            Nothing
                ]
            , describe "with prior selection"
                [ test "selects: day -> prior date, hour -> prior hour, minute -> provided minute" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorSelectionTuple =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 10 15 0 0)
                                )
                        in
                        Expect.equal
                            (SingleUtilities.selectMinute timeZone baseDay (Just priorSelectionTuple) 30)
                            (Just ( baseDay, Time.partsToPosix timeZone (Parts 2021 Jan 1 10 30 0 0) ))
                , test "returns prior selection if the resulting time of day from the provided minute is not selectable in prior selection enclosing day" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorSelectionTuple1 =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                )

                            priorSelectionTuple2 =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                )
                        in
                        Expect.equal
                            [ SingleUtilities.selectMinute timeZone baseDay (Just priorSelectionTuple1) 15, SingleUtilities.selectMinute timeZone baseDay (Just priorSelectionTuple2) 45 ]
                            [ Just priorSelectionTuple1, Just priorSelectionTuple2 ]
                ]
            ]
        , describe "filterSelectableTimes"
            [ describe "without prior selection"
                [ test "filters the selectable hours and minutes based on the start time of the base day" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }
                        in
                        Expect.equal
                            (SingleUtilities.filterSelectableTimes timeZone baseDay Nothing)
                            { selectableHours = List.range 9 17, selectableMinutes = List.range 30 59 }
                ]
            , describe "with prior selection"
                [ test "filters the selectable hours and minutes based on the selected time of day" <|
                    \_ ->
                        let
                            baseDay =
                                { start = Time.partsToPosix timeZone (Parts 2021 Jan 1 9 30 0 0)
                                , end = Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                , disabled = False
                                }

                            priorSelectionTuple1 =
                                ( baseDay
                                , baseDay.start
                                )

                            priorSelectionTuple2 =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 12 0 0 0)
                                )

                            priorSelectionTuple3 =
                                ( baseDay
                                , Time.partsToPosix timeZone (Parts 2021 Jan 1 17 30 0 0)
                                )
                        in
                        Expect.equal
                            [ SingleUtilities.filterSelectableTimes timeZone baseDay (Just priorSelectionTuple1)
                            , SingleUtilities.filterSelectableTimes timeZone baseDay (Just priorSelectionTuple2)
                            , SingleUtilities.filterSelectableTimes timeZone baseDay (Just priorSelectionTuple3)
                            ]
                            [ { selectableHours = List.range 9 17, selectableMinutes = List.range 30 59 }
                            , { selectableHours = List.range 9 17, selectableMinutes = List.range 0 59 }
                            , { selectableHours = List.range 9 17, selectableMinutes = List.range 0 30 }
                            ]
                ]
            ]
        ]
