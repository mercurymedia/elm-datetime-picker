module AlignmentTest exposing (suite)

import DatePicker.Alignment as Alignment
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "DatePicker.Alignment"
        [ describe "calculatePlacement"
            [ test "should align to the `Left, Bottom` of the trigger element if there is enough space" <|
                \_ ->
                    let
                        params =
                            { triggerX = 10
                            , triggerY = 10
                            , triggerWidth = 10
                            , triggerHeight = 10
                            , pickerWidth = 20
                            , pickerHeight = 10
                            , viewPortWidth = 100
                            , viewPortHeight = 100
                            }

                        expectedPlacement =
                            ( Alignment.Left, Alignment.Bottom )
                    in
                    Expect.equal (Alignment.calculatePlacement params)
                        expectedPlacement
            , test "should align to the `Left, Top` of the trigger element if there is not enough space to the bottom" <|
                \_ ->
                    let
                        params =
                            { triggerX = 10
                            , triggerY = 90
                            , triggerWidth = 10
                            , triggerHeight = 10
                            , pickerWidth = 20
                            , pickerHeight = 10
                            , viewPortWidth = 100
                            , viewPortHeight = 100
                            }

                        expectedPlacement =
                            ( Alignment.Left, Alignment.Top )
                    in
                    Expect.equal (Alignment.calculatePlacement params)
                        expectedPlacement
            , test "should align to the `Right, Bottom` of the trigger element if there is not enough space to the right" <|
                \_ ->
                    let
                        params =
                            { triggerX = 90
                            , triggerY = 10
                            , triggerWidth = 10
                            , triggerHeight = 10
                            , pickerWidth = 20
                            , pickerHeight = 10
                            , viewPortWidth = 100
                            , viewPortHeight = 100
                            }

                        expectedPlacement =
                            ( Alignment.Right, Alignment.Bottom )
                    in
                    Expect.equal (Alignment.calculatePlacement params)
                        expectedPlacement
            , test "should align to the `Right, Top` of the trigger element if there is not enough space to the right and bottom" <|
                \_ ->
                    let
                        params =
                            { triggerX = 90
                            , triggerY = 90
                            , triggerWidth = 10
                            , triggerHeight = 10
                            , pickerWidth = 20
                            , pickerHeight = 10
                            , viewPortWidth = 100
                            , viewPortHeight = 100
                            }

                        expectedPlacement =
                            ( Alignment.Right, Alignment.Top )
                    in
                    Expect.equal (Alignment.calculatePlacement params)
                        expectedPlacement
            , test "should align to the `Center, Bottom` of the trigger element if there is not enough space to the right and left" <|
                \_ ->
                    let
                        params =
                            { triggerX = 45
                            , triggerY = 10
                            , triggerWidth = 10
                            , triggerHeight = 10
                            , pickerWidth = 60
                            , pickerHeight = 10
                            , viewPortWidth = 100
                            , viewPortHeight = 100
                            }

                        expectedPlacement =
                            ( Alignment.Center, Alignment.Bottom )
                    in
                    Expect.equal (Alignment.calculatePlacement params)
                        expectedPlacement
            , test "should align to the `Center, Top` of the trigger element if there is not enough space to the right, left and bottom" <|
                \_ ->
                    let
                        params =
                            { triggerX = 45
                            , triggerY = 90
                            , triggerWidth = 10
                            , triggerHeight = 10
                            , pickerWidth = 60
                            , pickerHeight = 10
                            , viewPortWidth = 100
                            , viewPortHeight = 100
                            }

                        expectedPlacement =
                            ( Alignment.Center, Alignment.Top )
                    in
                    Expect.equal (Alignment.calculatePlacement params)
                        expectedPlacement
            ]
        ]
