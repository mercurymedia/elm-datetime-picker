# elm-datetime-picker
Single and duration datetime picker components written in Elm 0.19

## Install

`elm install mercurymedia/elm-datetime-picker`

## In action

#### Single Picker

![SinglePicker](https://user-images.githubusercontent.com/20546636/71161850-6a9fc000-224a-11ea-997f-1caaa0659777.gif)

#### Duration Picker

![DurationPicker](https://user-images.githubusercontent.com/20546636/70648518-7f2cf880-1c4b-11ea-8c6e-e97e4615c34c.gif)

## Usage

This package exposes two modules `SingleDatePicker` and `DurationDatePicker`. As their names imply, `SingleDatePicker` can be used to pick a singular datetime while `DurationDatePicker` is used to select a datetime range. To keep things simple, the documentation here focuses on the `SingleDatePicker` but both types have an example app for additional reference.

There are four steps to configure the `DatePicker`:

1) Add the picker to the model and initialize it in the model init

```elm
import SingleDatePicker as DatePicker

type alias Model =
    { ...
    , picker : DatePicker.DatePicker
    }

init : ( Model, Cmd Msg )
init =
    ( { ...
      , picker = DatePicker.init
      }
    , Cmd.none
    )
```

2) One message needs to be defined that expects an updated `DatePicker` instance as well as a `Maybe Posix` representing a datetime selection. This message is added to the picker settings along with a `Zone`.

```elm
type Msg
    = ...
    | UpdatePicker ( DatePicker.DatePicker, Maybe Posix )

userDefinedDatePickerSettings : Zone -> DatePicker.Settings Msg
userDefinedDatePickerSettings timeZone =
    DatePicker.defaultSettings timeZone UpdatePicker
```
    
3) We call the `DatePicker.view` function, passing it the defined settings and the `DatePicker` instance to be operated on. Because the messages for handling picker updates are defined in the calling module and passed in via the settings, we do not need to worry about `Html.map`ping!

```elm
view : Model -> Html Msg
view model =
    ...
    div []
        [ button [ onClick OpenPicker ] [ text "Open Me!" ]
        , DatePicker.view userDefinedDatePickerSettings model.picker
        ]
```

While we are on the topic of the `DatePicker.view`, it is worth noting that this date picker does _not_ include an input or button to trigger the view to open, this is up to the user to define and allows the picker to be flexible across different use cases.


4) Now it is time for the meat and potatoes: handling the `DatePicker` updates, including `saving` the time selected in the picker to the calling module's model.

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
    | UpdatePicker ( DatePicker.DatePicker, Maybe Posix )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ...

        OpenPicker ->
            ( { model | picker = DatePicker.openPicker model.zone model.today model.pickedTime model.picker }, Cmd.none )

        UpdatePicker ( newPicker, maybeNewTime ) ->
            ( { model | picker = newPicker, pickedTime = Maybe.map (\t -> Just t) maybeNewTime |> Maybe.withDefault model.pickedTime }, Cmd.none )
```

The user is responsible for defining his or her own `Open` picker message and placing the relevant event listener where he or she pleases. When handling this message in the `update` as seen above, we call `DatePicker.openPicker` which simply returns an updated picker instance to be stored on the model (`DatePicker.closePicker` is also provided and returns an updated picker instance like `openPicker` does). `DatePicker.openPicker` takes a `Zone` (the time zone in which to display the picker), `Posix` (the base time), a `Maybe Posix` (the picked time), and the `DatePicker` instance we wish to open. The base time is used to inform the picker what day it should center on in the event no datetime has been selected yet. This could be the current date or another date of the implementer's choosing.

Remember that message we passed into the `DatePicker` settings? Here is where it comes into play. `UpdatePicker` let's us know that an update of the `DatePicker` instance's internal state has occured. Seeing as we don't need to do any additional processing here, the `UpdatePicker` message simply carries the updated `DatePicker` instance along with it to save in the model of the calling module. Additionally, we get a `Maybe Posix`. In the case of `Just` a time, we set that on the model as the new `pickedTime` otherwise we default to the current `pickedTime`.

## Automatically close the picker

In the event you want the picker to close automatically when clicking outside of it, the module uses a subscription to determine when to close (outside of a save). Wire the picker subscription like below.

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    SingleDatePicker.subscriptions UpdatePicker model.picker
```

## Additional Configuration

This is the settings type to be used when configuring the `datepicker`. More configuration will be available in future releases.

```elm
type alias Settings msg =
    { internalMsg : ( DatePicker, Maybe Posix ) -> msg
    , zone : Zone
    , formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , isDayDisabled : Zone -> Posix -> Bool
    , focusedDate : Maybe Posix
    , dateStringFn : Zone -> Posix -> String
    , timePickerVisibility : TimePickerVisibility
    }
```

## Examples

Examples can be found in the [examples](https://github.com/mercurymedia/elm-datetime-picker/tree/master/examples) folder. To build the examples to view in the browser run: `cd examples && make && cd ..` from the root of the repository.

## CSS

The CSS for the date picker is distributed separately and can be found [here](https://github.com/mercurymedia/elm-datetime-picker/tree/master/css)
