# elm-datetime-picker

Single and duration datetime picker components written in Elm 0.19

## Install

`elm install mercurymedia/elm-datetime-picker`

## In action

#### Single Picker

![basic](https://user-images.githubusercontent.com/32676430/219370444-3e942c63-822f-45a7-8029-f7b0f1c30190.gif)

#### Duration Picker

![duration](https://user-images.githubusercontent.com/32676430/219370489-f9d21925-a7be-43b6-a2b9-b2e844c3c547.gif)

## Usage

This package exposes two modules `SingleDatePicker` and `DurationDatePicker`. As their names imply, `SingleDatePicker` can be used to pick a singular datetime while `DurationDatePicker` is used to select a datetime range. To keep things simple, the documentation here focuses on the `SingleDatePicker` but both types have an example app for additional reference.

There are four steps to configure the `DatePicker`:

1. Add the picker to the model and initialize it in the model init. One message needs to be defined that expects an internal `DatePicker` message. This is used to update the selection and view of the picker.

```elm
import SingleDatePicker as DatePicker

type alias Model =
    { ...
    , picker : DatePicker.DatePicker Msg
    }

type Msg
    = ...
    | UpdatePicker DatePicker.Msg

init : ( Model, Cmd Msg )
init =
    ( { ...
      , picker = DatePicker.init UpdatePicker
      }
    , Cmd.none
    )
```

1. We call the `DatePicker.view` function, passing it the picker `Settings` and the `DatePicker` instance to be operated on. The minimal picker `Settings` only require a `Time.Zone`

```elm
userDefinedDatePickerSettings : Zone -> DatePicker.Settings
userDefinedDatePickerSettings timeZone =
    DatePicker.defaultSettings timeZone

view : Model -> Html Msg
view model =
    ...
    div []
        [ button [ onClick OpenPicker ] [ text "Open Me!" ]
        , DatePicker.view userDefinedDatePickerSettings model.picker
        ]
```

While we are on the topic of the `DatePicker.view`, it is worth noting that this date picker does _not_ include an input or button to trigger the view to open, this is up to the user to define and allows the picker to be flexible across different use cases.

1. Now it is time for the meat and potatoes: handling the `DatePicker` updates, including `saving` the time selected in the picker to the calling module's model.

```elm
type alias Model =
    { ...
    , today : Posix
    , zone : Zone
    , pickedTime : Maybe Posix
    , picker : DatePicker.DatePicker
    }

type Msg
    = ...
    | OpenPicker
    | UpdatePicker DatePicker.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ...

        OpenPicker ->
            ( { model | picker = DatePicker.openPicker model.zone model.today model.pickedTime model.picker }, Cmd.none )

        UpdatePicker subMsg ->
            let
                ( newPicker, maybeNewTime ) =
                    SingleDatePicker.update (userDefinedDatePickerSettings model.zone model.currentTime) subMsg model.picker
            in
            ( { model | picker = newPicker, pickedTime = Maybe.map (\t -> Just t) maybeNewTime |> Maybe.withDefault model.pickedTime }, Cmd.none )
```

The user is responsible for defining his or her own `Open` picker message and placing the relevant event listener where he or she pleases. When handling this message in the `update` as seen above, we call `DatePicker.openPicker` which simply returns an updated picker instance to be stored on the model (`DatePicker.closePicker` is also provided and returns an updated picker instance like `openPicker` does). `DatePicker.openPicker` takes a `Zone` (the time zone in which to display the picker), `Posix` (the base time), a `Maybe Posix` (the picked time), and the `DatePicker` instance we wish to open. The base time is used to inform the picker what day it should center on in the event no datetime has been selected yet. This could be the current date or another date of the implementer's choosing.

Remember that message we passed into the `DatePicker` settings? Here is where it comes into play. `UpdatePicker` let's us know that an update of the `DatePicker` instance's internal state needs to happen. To process the `DatePicker.Msg` you can pass it to the respective `DatePicker.update` function along with the `Settings` and the current `DatePicker` instance. That will then return us the updated `DatePicker` instance, to save in the model of the calling module. Additionally, we get a `Maybe Posix`. In the case of `Just` a time, we set that on the model as the new `pickedTime` otherwise we default to the current `pickedTime`.

## Automatically close the picker

In the event you want the picker to close automatically when clicking outside of it, the module uses a subscription to determine when to close (outside of a save). Wire the picker subscription like below.

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    SingleDatePicker.subscriptions model.picker
```

## Additional Configuration

This is the settings type to be used when configuring the `datepicker`. More configuration will be available in future releases.

```elm
type alias Settings =
    { zone : Zone
    , formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , isDayDisabled : Zone -> Posix -> Bool
    , focusedDate : Maybe Posix
    , dateStringFn : Zone -> Posix -> String
    , timePickerVisibility : TimePickerVisibility
    , showCalendarWeekNumbers : Bool
    }
```

## Examples

Examples can be found in the [examples](https://github.com/mercurymedia/elm-datetime-picker/tree/master/examples) folder. To build the examples to view in the browser run: `cd examples && make && cd ..` from the root of the repository.

## CSS

The CSS for the date picker is distributed separately and can be found [here](https://github.com/mercurymedia/elm-datetime-picker/tree/master/css).
The styling is based on a CSS-Variables theme that can be easily adjusted for the most important design tokens.
