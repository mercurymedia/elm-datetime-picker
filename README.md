# elm-datetime-picker
Single and duration datetime picker components written in Elm 0.19

## Install

`elm install mercurymedia/elm-datetime-picker`

## In action

#### Single Picker

![SinglePicker](https://user-images.githubusercontent.com/20546636/64876897-82b2e280-d650-11e9-8b36-5609125b1665.gif)

#### Duration Picker

![DurationPicker](https://user-images.githubusercontent.com/20546636/64876920-91999500-d650-11e9-8f56-3088930b64f7.gif)

## Usage

This package exposes two modules `SingleDatePicker` and `DurationDatePicker`. As their names imply, `SingleDatePicker` can be used to pick a singular datetime while `DurationDatePicker` is used to select a datetime range. To keep things simple, the documentation here focuses on the `SingleDatePicker` but both types have an example app for additional reference.

There are three steps to configure the `DatePicker`:

1) Add the picker to the model and initialize it in the model init. The `SingleDatePicker.init` function takes the configured
settings. In the event you wish to `init` a picker with a previously picked datetime, simply pipe the result of the `init` to `SingleDatePicker.setPickedTime` passing the picked time.

Both datepicker modules expose a `defaultSettings` method for configuring the settings. It takes a `Posix` representing the date around which the picker should center on if no datetime has been picked yet. It also takes a message defined by the user to handle updates internal to the datepicker. The function returns a `Settings` type which can be extended for further customization.

```elm
import SingleDatePicker

type alias Model =
    { ...
    , picker : SingleDatePicker.DatePicker
    }

type Msg
    = ...
    | UpdatePicker SingleDatePicker.DatePicker

init : ( Model, Cmd Msg )
init =
    ( { ...
      , picker = SingleDatePicker.init (SingleDatePicker.defaultSettings today UpdatePicker) Nothing
      }
    , Cmd.none
    )
```
    
2) Call the `SingleDatePicker.view` function to render the picker, passing it the `DatePicker` instance to be operated on. Because the messages for handling picker updates are defined in the calling module and passed in via the settings, we do not need to worry about `Html.map`ping!

```elm
view : Model -> Html Msg
view model =
    ...
    div []
        [ button [ onClick <| OpenPicker ] [ text "Open Me!" ]
        , SingleDatePicker.view model.picker
        ]
```

While we are on the topic of the `SingleDatePicker.view`, it is worth noting that this date picker does _not_ include an input or button to trigger the view to open, this is up to the user to define and allows the picker to be flexible across different use cases.


3) Now it is time for the meat and potatoes: handling the `DatePicker` updates.

```elm
type alias Model =
    { ...
    , today : Posix
    , picker : SingleDatePicker.DatePicker
    }

type Msg
    = ...
    | OpenPicker
    | UpdatePicker SingleDatePicker.DatePicker

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ...

        OpenPicker ->
            ( { model | picker = SingleDatePicker.openPicker model.picker }, Cmd.none )

        UpdatePicker newPicker ->
            ( { model | picker = newPicker }, Cmd.none )
```

The user is responsible for defining his or her own `Open` picker message and placing the relevant event listener where he or she pleases. When handling this message in the `update` as seen above, we call `SingleDatePicker.openPicker` which simply returns an updated picker instance to be stored on the model (`SingleDatePicker.closePicker` is also provided and returns an updated picker instance like `openPicker` does).

Remember that message we passed into the `DatePicker` settings? Here is where it comes into play. `UpdatePicker` let's us know that an update of the `DatePicker` instance's internal state has occured. Seeing as we don't need to do any additional processing here, the `UpdatePicker` message simply carries the updated `DatePicker` instance along with it to save in the model of the calling module.

The picker also stores the current selection state. To access it, call `SingleDatePicker.getPickedTime`.

## Automatically close the picker

In the event you want the picker to close automatically when clicking outside of it, the module uses a subscription to determine when to close (outside of a save). Wire the picker subscription like below.

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    SingleDatePicker.subscriptions model.picker
```

## Additional Configuration

This is the settings type to be used when configuring the `DatePicker`. More configuration will be available in future releases.

```elm
type alias Settings msg =
    { formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , today : Maybe Posix
    , baseTime : Posix
    , dayDisabled : Posix -> Bool
    , internalMsg : DatePicker msg -> msg
    }
```

## Examples

Examples can be found in the [examples](https://github.com/mercurymedia/elm-datetime-picker/tree/master/examples) folder. To build the examples to view in the browser run: `cd examples && make && cd ..` from the root of the repository.

## CSS

The CSS for the date picker is distributed separately and can be found [here](https://github.com/mercurymedia/elm-datetime-picker/tree/master/css)
