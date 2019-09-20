module UtilitiesTest exposing (suite)

import DatePicker.Utilities as Utilities
import Expect
import Html exposing (option, text)
import Html.Attributes exposing (selected, value)
import Test exposing (..)
import Time exposing (Month(..))
import Time.Extra as Time exposing (Parts, partsToPosix)


suite : Test
suite =
    let
        expectedHourOptions =
            [ option [ value "0", selected False ] [ text "00" ]
            , option [ value "1", selected False ] [ text "01" ]
            , option [ value "2", selected False ] [ text "02" ]
            , option [ value "3", selected False ] [ text "03" ]
            , option [ value "4", selected False ] [ text "04" ]
            , option [ value "5", selected False ] [ text "05" ]
            , option [ value "6", selected False ] [ text "06" ]
            , option [ value "7", selected False ] [ text "07" ]
            , option [ value "8", selected False ] [ text "08" ]
            , option [ value "9", selected False ] [ text "09" ]
            , option [ value "10", selected False ] [ text "10" ]
            , option [ value "11", selected False ] [ text "11" ]
            , option [ value "12", selected True ] [ text "12" ]
            , option [ value "13", selected False ] [ text "13" ]
            , option [ value "14", selected False ] [ text "14" ]
            , option [ value "15", selected False ] [ text "15" ]
            , option [ value "16", selected False ] [ text "16" ]
            , option [ value "17", selected False ] [ text "17" ]
            , option [ value "18", selected False ] [ text "18" ]
            , option [ value "19", selected False ] [ text "19" ]
            , option [ value "20", selected False ] [ text "20" ]
            , option [ value "21", selected False ] [ text "21" ]
            , option [ value "22", selected False ] [ text "22" ]
            , option [ value "23", selected False ] [ text "23" ]
            ]

        expectedMinuteOptions =
            [ option [ value "0", selected False ] [ text "00" ]
            , option [ value "1", selected False ] [ text "01" ]
            , option [ value "2", selected False ] [ text "02" ]
            , option [ value "3", selected False ] [ text "03" ]
            , option [ value "4", selected False ] [ text "04" ]
            , option [ value "5", selected False ] [ text "05" ]
            , option [ value "6", selected False ] [ text "06" ]
            , option [ value "7", selected False ] [ text "07" ]
            , option [ value "8", selected False ] [ text "08" ]
            , option [ value "9", selected False ] [ text "09" ]
            , option [ value "10", selected False ] [ text "10" ]
            , option [ value "11", selected False ] [ text "11" ]
            , option [ value "12", selected False ] [ text "12" ]
            , option [ value "13", selected False ] [ text "13" ]
            , option [ value "14", selected False ] [ text "14" ]
            , option [ value "15", selected False ] [ text "15" ]
            , option [ value "16", selected False ] [ text "16" ]
            , option [ value "17", selected False ] [ text "17" ]
            , option [ value "18", selected False ] [ text "18" ]
            , option [ value "19", selected False ] [ text "19" ]
            , option [ value "20", selected False ] [ text "20" ]
            , option [ value "21", selected False ] [ text "21" ]
            , option [ value "22", selected False ] [ text "22" ]
            , option [ value "23", selected False ] [ text "23" ]
            , option [ value "24", selected False ] [ text "24" ]
            , option [ value "25", selected False ] [ text "25" ]
            , option [ value "26", selected False ] [ text "26" ]
            , option [ value "27", selected False ] [ text "27" ]
            , option [ value "28", selected False ] [ text "28" ]
            , option [ value "29", selected False ] [ text "29" ]
            , option [ value "30", selected True ] [ text "30" ]
            , option [ value "31", selected False ] [ text "31" ]
            , option [ value "32", selected False ] [ text "32" ]
            , option [ value "33", selected False ] [ text "33" ]
            , option [ value "34", selected False ] [ text "34" ]
            , option [ value "35", selected False ] [ text "35" ]
            , option [ value "36", selected False ] [ text "36" ]
            , option [ value "37", selected False ] [ text "37" ]
            , option [ value "38", selected False ] [ text "38" ]
            , option [ value "39", selected False ] [ text "39" ]
            , option [ value "40", selected False ] [ text "40" ]
            , option [ value "41", selected False ] [ text "41" ]
            , option [ value "42", selected False ] [ text "42" ]
            , option [ value "43", selected False ] [ text "43" ]
            , option [ value "44", selected False ] [ text "44" ]
            , option [ value "45", selected False ] [ text "45" ]
            , option [ value "46", selected False ] [ text "46" ]
            , option [ value "47", selected False ] [ text "47" ]
            , option [ value "48", selected False ] [ text "48" ]
            , option [ value "49", selected False ] [ text "49" ]
            , option [ value "50", selected False ] [ text "50" ]
            , option [ value "51", selected False ] [ text "51" ]
            , option [ value "52", selected False ] [ text "52" ]
            , option [ value "53", selected False ] [ text "53" ]
            , option [ value "54", selected False ] [ text "54" ]
            , option [ value "55", selected False ] [ text "55" ]
            , option [ value "56", selected False ] [ text "56" ]
            , option [ value "57", selected False ] [ text "57" ]
            , option [ value "58", selected False ] [ text "58" ]
            , option [ value "59", selected False ] [ text "59" ]
            ]
    in
    describe "DatePicker.Utilities"
        [ describe "setHourNotDay"
            [ test "updates only the hour of the provided Posix" <|
                \_ ->
                    let
                        dateTimeToUpdate =
                            Time.partsToPosix Time.utc (Parts 2019 Sep 19 0 0 0 0)

                        expectedResult =
                            Time.partsToPosix Time.utc (Parts 2019 Sep 19 12 0 0 0)
                    in
                    Expect.equal (Utilities.setHourNotDay 12 dateTimeToUpdate) expectedResult
            ]
        , describe "setMinuteNotDay"
            [ test "updates only the minute of the provided Posix" <|
                \_ ->
                    let
                        dateTimeToUpdate =
                            Time.partsToPosix Time.utc (Parts 2019 Sep 19 0 0 0 0)

                        expectedResult =
                            Time.partsToPosix Time.utc (Parts 2019 Sep 19 0 30 0 0)
                    in
                    Expect.equal (Utilities.setMinuteNotDay 30 dateTimeToUpdate) expectedResult
            ]
        , describe "setDayNotTime"
            [ test "returns a Posix with the day of the newly selected DateTime while maintaining the time of the previous selection" <|
                \_ ->
                    let
                        previouslySelectedDateTime =
                            Time.partsToPosix Time.utc (Parts 2019 Sep 17 13 30 0 0)

                        newSelectedDateTime =
                            Time.partsToPosix Time.utc (Parts 2019 Sep 19 0 0 0 0)

                        expectedResult =
                            Time.partsToPosix Time.utc (Parts 2019 Sep 19 13 30 0 0)
                    in
                    Expect.equal (Utilities.setDayNotTime newSelectedDateTime previouslySelectedDateTime) expectedResult
            ]
        , describe "generateHourOptions"
            [ test "generates a list of html options for every hour of the day. The selected option should match the hour passed in." <|
                \_ ->
                    Expect.equal (Utilities.generateHourOptions 12) expectedHourOptions
            ]
        , describe "generateMinuteOptions"
            [ test "generates a list of html options for every minute of an hour. The selected option should match the minute passed in." <|
                \_ ->
                    Expect.equal (Utilities.generateMinuteOptions 30) expectedMinuteOptions
            ]
        ]
