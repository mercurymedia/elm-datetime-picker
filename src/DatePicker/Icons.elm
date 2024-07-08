module DatePicker.Icons exposing (Icon(..), arrowRight, calendar, check, chevronDown, chevronLeft, chevronRight, chevronUp, chevronsLeft, chevronsRight, clock, edit, toHtml, withSize)

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)



-- The code below comes from feathericons/elm-feather (https://github.com/feathericons/elm-feather/)


{-| Customizable attributes of icon
-}
type alias IconAttributes =
    { size : Float
    , sizeUnit : String
    , strokeWidth : Float
    , class : Maybe String
    , viewBox : String
    }


{-| Default attributes, first argument is icon name
-}
defaultAttributes : String -> IconAttributes
defaultAttributes name =
    { size = 24
    , sizeUnit = ""
    , strokeWidth = 2
    , class = Just <| "feather feather-" ++ name
    , viewBox = "0 0 24 24"
    }


{-| Opaque type representing icon builder
-}
type Icon
    = Icon
        { attrs : IconAttributes
        , src : List (Svg Never)
        }


{-| Set size attribute of an icon
Icon.download
|> Icon.withSize 10
|> Icon.toHtml []
-}
withSize : Float -> Icon -> Icon
withSize size (Icon { attrs, src }) =
    Icon { attrs = { attrs | size = size }, src = src }


{-| Build icon, ready to use in html. It accepts list of svg attributes, for example in case if you want to add an event handler.
-- default
Icon.download
|> Icon.toHtml []
-- with some attributes
Icon.download
|> Icon.withSize 10
|> Icon.withClass "icon-download"
|> Icon.toHtml [ onClick Download ]
-}
toHtml : List (Svg.Attribute msg) -> Icon -> Html msg
toHtml attributes (Icon { src, attrs }) =
    let
        strSize =
            attrs.size |> String.fromFloat

        baseAttributes =
            [ fill "none"
            , height <| strSize ++ attrs.sizeUnit
            , width <| strSize ++ attrs.sizeUnit
            , stroke "currentColor"
            , strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth <| String.fromFloat attrs.strokeWidth
            , viewBox attrs.viewBox
            ]

        combinedAttributes =
            (case attrs.class of
                Just c ->
                    class c :: baseAttributes

                Nothing ->
                    baseAttributes
            )
                ++ attributes
    in
    src
        |> List.map (Svg.map never)
        |> svg combinedAttributes


makeBuilder : String -> List (Svg Never) -> Icon
makeBuilder name src =
    Icon { attrs = defaultAttributes name, src = src }


{-| check
![image](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDI0IDI0IiBmaWxsPSJub25lIiBzdHJva2U9ImN1cnJlbnRDb2xvciIgc3Ryb2tlLXdpZHRoPSIyIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwb2x5bGluZSBwb2ludHM9IjIwIDYgOSAxNyA0IDEyIj48L3BvbHlsaW5lPjwvc3ZnPg==)
-}
check : Icon
check =
    makeBuilder "check"
        [ Svg.polyline [ points "20 6 9 17 4 12" ] []
        ]


{-| chevron-left
![image](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDI0IDI0IiBmaWxsPSJub25lIiBzdHJva2U9ImN1cnJlbnRDb2xvciIgc3Ryb2tlLXdpZHRoPSIyIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwb2x5bGluZSBwb2ludHM9IjE1IDE4IDkgMTIgMTUgNiI+PC9wb2x5bGluZT48L3N2Zz4=)
-}
chevronLeft : Icon
chevronLeft =
    makeBuilder "chevron-left"
        [ Svg.polyline [ points "15 18 9 12 15 6" ] []
        ]


{-| chevron-right
![image](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDI0IDI0IiBmaWxsPSJub25lIiBzdHJva2U9ImN1cnJlbnRDb2xvciIgc3Ryb2tlLXdpZHRoPSIyIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwb2x5bGluZSBwb2ludHM9IjkgMTggMTUgMTIgOSA2Ij48L3BvbHlsaW5lPjwvc3ZnPg==)
-}
chevronRight : Icon
chevronRight =
    makeBuilder "chevron-right"
        [ Svg.polyline [ points "9 18 15 12 9 6" ] []
        ]


{-| chevrons-left
![image](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDI0IDI0IiBmaWxsPSJub25lIiBzdHJva2U9ImN1cnJlbnRDb2xvciIgc3Ryb2tlLXdpZHRoPSIyIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwb2x5bGluZSBwb2ludHM9IjExIDE3IDYgMTIgMTEgNyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIxOCAxNyAxMyAxMiAxOCA3Ij48L3BvbHlsaW5lPjwvc3ZnPg==)
-}
chevronsLeft : Icon
chevronsLeft =
    makeBuilder "chevrons-left"
        [ Svg.polyline [ points "11 17 6 12 11 7" ] []
        , Svg.polyline [ points "18 17 13 12 18 7" ] []
        ]


{-| chevrons-right
![image](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDI0IDI0IiBmaWxsPSJub25lIiBzdHJva2U9ImN1cnJlbnRDb2xvciIgc3Ryb2tlLXdpZHRoPSIyIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwb2x5bGluZSBwb2ludHM9IjEzIDE3IDE4IDEyIDEzIDciPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iNiAxNyAxMSAxMiA2IDciPjwvcG9seWxpbmU+PC9zdmc+)
-}
chevronsRight : Icon
chevronsRight =
    makeBuilder "chevrons-right"
        [ Svg.polyline [ points "13 17 18 12 13 7" ] []
        , Svg.polyline [ points "6 17 11 12 6 7" ] []
        ]


{-| chevron-up
![image](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDI0IDI0IiBmaWxsPSJub25lIiBzdHJva2U9ImN1cnJlbnRDb2xvciIgc3Ryb2tlLXdpZHRoPSIyIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwb2x5bGluZSBwb2ludHM9IjE4IDE1IDEyIDkgNiAxNSI+PC9wb2x5bGluZT48L3N2Zz4=)
-}
chevronUp : Icon
chevronUp =
    makeBuilder "chevron-up"
        [ Svg.polyline [ points "18 15 12 9 6 15" ] []
        ]


{-| chevron-down
![image](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDI0IDI0IiBmaWxsPSJub25lIiBzdHJva2U9ImN1cnJlbnRDb2xvciIgc3Ryb2tlLXdpZHRoPSIyIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwb2x5bGluZSBwb2ludHM9IjYgOSAxMiAxNSAxOCA5Ij48L3BvbHlsaW5lPjwvc3ZnPg==)
-}
chevronDown : Icon
chevronDown =
    makeBuilder "chevron-down"
        [ Svg.polyline [ points "6 9 12 15 18 9" ] []
        ]


{-| clock
-}
clock : Icon
clock =
    makeBuilder "clock"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.polyline [ points "12 6 12 12 16 14" ] []
        ]


{-| calendar
-}
calendar : Icon
calendar =
    makeBuilder "clock"
        [ Svg.rect [ x "3", y "4", width "18", height "18", rx "2", ry "2" ] []
        , Svg.line [ x1 "16", y1 "2", x2 "16", y2 "6" ] []
        , Svg.line [ x1 "8", y1 "2", x2 "8", y2 "6" ] []
        , Svg.line [ x1 "3", y1 "10", x2 "21", y2 "10" ] []
        ]


{-| edit
-}
edit : Icon
edit =
    makeBuilder "edit"
        [ Svg.path [ d "M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7" ] []
        , Svg.path [ d "M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z" ] []
        ]


{-| arrow-right
-}
arrowRight : Icon
arrowRight =
    makeBuilder "arrow-right"
        [ Svg.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        , Svg.polyline [ points "12 5 19 12 12 19" ] []
        ]
