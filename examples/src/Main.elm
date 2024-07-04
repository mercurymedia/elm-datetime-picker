module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import DurationDatePickerExample
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, style, target)
import ModalPickerExample
import SingleDatePickerExample
import Url exposing (Url)
import Url.Parser as Parser



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { page : Page
    , navKey : Nav.Key
    }


type Page
    = SingleDatePicker SingleDatePickerExample.Model
    | DurationDatePicker DurationDatePickerExample.Model
    | ModalPicker ModalPickerExample.Model
    | NotFound


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | SingleDatePickerMsg SingleDatePickerExample.Msg
    | DurationDatePickerMsg DurationDatePickerExample.Msg
    | ModalPickerMsg ModalPickerExample.Msg



-- MODEL


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        initialModel =
            { page = NotFound, navKey = navKey }
    in
    changePageTo url initialModel



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFound ->
            Sub.none

        SingleDatePicker pickerModel ->
            Sub.map SingleDatePickerMsg (SingleDatePickerExample.subscriptions pickerModel)

        DurationDatePicker pickerModel ->
            Sub.map DurationDatePickerMsg (DurationDatePickerExample.subscriptions pickerModel)

        ModalPicker pickerModel ->
            Sub.map ModalPickerMsg (ModalPickerExample.subscriptions pickerModel)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changePageTo url model

        ( SingleDatePickerMsg subMsg, SingleDatePicker subModel ) ->
            let
                ( updatedSubModel, pageCmd ) =
                    SingleDatePickerExample.update subMsg subModel
            in
            ( { model | page = SingleDatePicker updatedSubModel }, Cmd.map SingleDatePickerMsg pageCmd )

        ( DurationDatePickerMsg subMsg, DurationDatePicker subModel ) ->
            let
                ( updatedSubModel, pageCmd ) =
                    DurationDatePickerExample.update subMsg subModel
            in
            ( { model | page = DurationDatePicker updatedSubModel }, Cmd.map DurationDatePickerMsg pageCmd )

        ( ModalPickerMsg subMsg, ModalPicker subModel ) ->
            let
                ( updatedSubModel, pageCmd ) =
                    ModalPickerExample.update subMsg subModel
            in
            ( { model | page = ModalPicker updatedSubModel }, Cmd.map ModalPickerMsg pageCmd )

        ( _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "elm-datetime-picker"
    , body =
        [ viewSidebar
        , viewPage model.page
        ]
    }


viewSidebar : Html Msg
viewSidebar =
    div [ class "sidebar" ]
        [ viewHeader
        , viewSources
        , viewNavigation
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "header" ] [ text "elm-datetime-picker" ]


viewSources : Html Msg
viewSources =
    div [ style "margin-top" "2rem" ]
        [ a
            [ href "https://github.com/mercurymedia/elm-datetime-picker"
            , target "_blank"
            ]
            [ text "Github" ]
        , span
            [ style "padding" "0px 8px"
            ]
            [ text "|" ]
        , a
            [ href "https://package.elm-lang.org/packages/mercurymedia/elm-datetime-picker/latest/"
            , target "_blank"
            ]
            [ text "Docs" ]
        ]


viewNavigation : Html Msg
viewNavigation =
    div [ class "navigation" ]
        [ div [] [ text "Examples" ]
        , viewPageLink "SingleDatePicker" "/"
        , viewPageLink "DurationDatePicker" "/duration"
        , viewPageLink "ModalPicker" "/modal"
        ]


viewPageLink : String -> String -> Html Msg
viewPageLink title url =
    a
        [ href url
        , style "margin-left" "15px"
        ]
        [ text title ]


viewPage : Page -> Html Msg
viewPage page =
    let
        toPage toMsg pageView =
            Html.map toMsg pageView
    in
    div
        [ class "page"
        ]
        [ case page of
            NotFound ->
                text "Not found"

            SingleDatePicker pageModel ->
                toPage SingleDatePickerMsg (SingleDatePickerExample.view pageModel)

            DurationDatePicker pageModel ->
                toPage DurationDatePickerMsg (DurationDatePickerExample.view pageModel)

            ModalPicker pageModel ->
                toPage ModalPickerMsg (ModalPickerExample.view pageModel)
        ]



-- HELPER


changePageTo : Url -> Model -> ( Model, Cmd Msg )
changePageTo url model =
    let
        toPage toModel toMsg ( pageModel, pageCmd ) =
            ( { model | page = toModel pageModel }, Cmd.map toMsg pageCmd )

        parser =
            Parser.oneOf
                [ Parser.map (SingleDatePickerExample.init |> toPage SingleDatePicker SingleDatePickerMsg) Parser.top
                , Parser.map (DurationDatePickerExample.init |> toPage DurationDatePicker DurationDatePickerMsg) (Parser.s "duration")
                , Parser.map (ModalPickerExample.init |> toPage ModalPicker ModalPickerMsg) (Parser.s "modal")
                ]
    in
    Parser.parse parser url
        |> Maybe.withDefault ( { model | page = NotFound }, Cmd.none )
