# elm-datetime-picker

Single and duration datetime picker components written in Elm 0.19

## Install

`elm install mercurymedia/elm-datetime-picker`

## In action

#### Single Picker

![single-gif](https://github.com/user-attachments/assets/3ca259c0-9d7a-46b0-8979-c7dce07e7135)


#### Duration Picker

![duration-gif](https://github.com/user-attachments/assets/96f66a23-097b-49b2-bd16-3fa28a162abe)


## Usage

This package exposes two core modules, `SingleDatePicker` and `DurationDatePicker`. Three more modules are required / available for the configuration: `DatePicker.Settings`, `DatePicker.Theme` and `DatePicker.DateInput`. As their names imply, `SingleDatePicker` can be used to pick a singular datetime while `DurationDatePicker` is used to select a datetime range. To keep things simple, the documentation here focuses on the `SingleDatePicker` but both types have an example app for additional reference.

You can use both picker variants with an integrated date input element – or without. 

### Configure the Date Input Picker

1. Add the picker to the model and initialize it in the model init. One message needs to be defined that expects an internal `DatePicker` message. This is used to update the selection and view of the picker.

```elm
import DatePicker.Settings exposing (Settings, defaultSettings)
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

2. We call the `DatePicker.viewDateInput` function, passing it a  `List (Html.Attribute msg)` to add custom attributes to the date input's container element, the picker `Settings`, the current time, the picked time and the `DatePicker` instance to be operated on. The minimal picker `Settings` only require a `Time.Zone`

```elm
userDefinedDatePickerSettings : Zone -> Settings
userDefinedDatePickerSettings timeZone =
    defaultSettings timeZone

view : Model -> Html Msg
view model =
    ...
    div []
        [ DatePicker.viewDateInput []
            (userDefinedDatePickerSettings model.zone)
            model.today
            model.dateInputPickerTime
            model.dateInputPicker
        ]
```

3. Now it is time for the meat and potatoes: handling the `DatePicker` updates, including `saving` the time selected in the picker to the calling module's model.

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
    | UpdatePicker DatePicker.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ...
        UpdatePicker subMsg ->
            let
                ( ( updatedPicker, maybeUpdatedTime ), pickerCmd ) =
                    SingleDatePicker.update (userDefinedDatePickerSettings model.zone) subMsg model.picker
            in
            ( { model | picker = updatedPicker, pickedTime = maybeUpdatedTime }, pickerCmd )
```

Remember that message we passed into the `DatePicker` settings? Here is where it comes into play. `UpdatePicker` let's us know that an update of the `DatePicker` instance's internal state needs to happen. To process the `DatePicker.Msg` you can pass it to the respective `DatePicker.update` function along with the `Settings` and the current `DatePicker` instance. That will then return us the updated `DatePicker` instance, to save in the model of the calling module. Additionally, we get a `Maybe Posix`. In the case of `Just` a time, we set that on the model as the new `pickedTime` otherwise we default to the current `pickedTime`.

### Configure the Picker without Date Input

To use the picker without a date input in order to use is accross other use cases, you need to manually take care of the opening mechanism.

1. (Add initialization and `Update` method like in the previous date input variant)

2. We call the `DatePicker.view` function, passing it the picker `Settings` and the `DatePicker` instance to be operated on. The `Settings` are the same – but now you can add your own trigger element (e.g. a button). Make sure to give your trigger element an `id`, it needs to be passed to the picker later on.

```elm
userDefinedDatePickerSettings : Zone -> Settings
userDefinedDatePickerSettings timeZone =
    defaultSettings timeZone

view : Model -> Html Msg
view model =
    ...
    div []
        [ button [ id "my-button", onClick (OpenPicker "my-button") ] [ text "Open Me!" ]
        , DatePicker.view userDefinedDatePickerSettings model.picker
        ]
```

3. Handling the `DatePicker` updates works the same as in the date input variant – but as mentioned before, you now need to manually handle the opening:

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
    | OpenPicker String
    | UpdatePicker DatePicker.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ...

        OpenPicker triggerElementId ->
            let
                ( newPicker, pickerCmd ) =
                    SingleDatePicker.openPicker triggerElementId
                        (userDefinedDatePickerSettings model.zone model.today)
                        model.today
                        model.pickedTime
                        model.picker
            in
            ( { model | picker = newPicker }, pickerCmd )

        UpdatePicker subMsg ->
            let
                (( updatedPicker, maybeNewTime ), pickerCmd) =
                    SingleDatePicker.update (userDefinedDatePickerSettings model.zone model.today) subMsg model.picker
            in
            ( { model | picker = updatedPicker, pickedTime = Maybe.map (\t -> Just t) maybeNewTime |> Maybe.withDefault model.pickedTime }, pickerCmd )
```

The user is responsible for defining his or her own `Open` picker message and placing the relevant event listener where he or she pleases. When handling this message in the `update` as seen above, we call `DatePicker.openPicker` which simply returns an updated picker instance to be stored on the model (`DatePicker.closePicker` is also provided and returns an updated picker instance like `openPicker` does). `DatePicker.openPicker` takes a `Zone` (the time zone in which to display the picker), `Posix` (the base time), a `Maybe Posix` (the picked time), and the `DatePicker` instance we wish to open. The base time is used to inform the picker what day it should center on in the event no datetime has been selected yet. This could be the current date or another date of the implementer's choosing.

## Automatically close the picker

In the event you want the picker to close automatically when clicking outside of it, the module uses a subscription to determine when to close (outside of a save). Wire the picker subscription like below.

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    SingleDatePicker.subscriptions model.picker
```

## Picker positions

The picker's popover is positioned `fixed`. That is why you need need to pass an `id` of the trigger element when calling `DatePicker.openPicker`: the picker looks for the id's element and automatically aligns itself with that element's position. Based on the available space in each direction from the trigger element, the picker will try to find an alignment that prevents the element from being cut off by the viewport.


## Presets

Date or date range presets can be added with the settings configuration. The list of presets is empty by default.

```elm
type alias Settings =
    { -- [...]
    , presets : List Preset
    }
```

To configure presets, just add data of the following required type to the list:

```elm

type Preset
    = PresetDate PresetDateConfig -- for single date pickers
    | PresetRange PresetRangeConfig -- for duration date pickers

type alias PresetDateConfig =
    { title : String
    , date : Posix
    }

type alias PresetRangeConfig =
    { title : String
    , range : { start : Posix, end : Posix }
    }
```

Here's an example:

```elm
userDefinedDatePickerSettings : Zone -> Posix -> Settings
userDefinedDatePickerSettings zone today =
    let
        defaults =
            defaultSettings zone
    in
    { defaults
        | presetRanges =
            [ { title = "This month"
              , range =
                    { start = TimeExtra.floor Month zone today
                    , end =
                        TimeExtra.floor Month zone today
                            |> TimeExtra.add Month 1 zone
                            |> TimeExtra.add Day -1 zone
                    }
              }
            ]
    }
```

## Additional Configuration

This is the settings type to be used when configuring the `DatePicker`. More configuration will be available in future releases.

```elm
type alias Settings =
    { zone : Zone
    , id : String
    , firstWeekDay : Weekday
    , formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , isDayDisabled : Zone -> Posix -> Bool
    , focusedDate : Maybe Posix
    , dateStringFn : Zone -> Posix -> String
    , timePickerVisibility : TimePickerVisibility
    , showCalendarWeekNumbers : Bool
    , presets : List Preset
    , theme : Theme.Theme
    , dateInputSettings : DateInput.Settings
    }
```


## Date Input & Configuration

The `DateInput` module formats and validates a user's text input for dates and times. The following type is the `DateInput.Settings` type to be used when configuring the `DatePicker.Settings`.

```elm
type alias Settings =
    { format : Format
    , getErrorMessage : InputError -> String
    }
```

To configure the `Format` there are a few options:

```elm
{-| Either allow a date only or a date and a time.
-}
type Format
    = Date DateFormat
    | DateTime DateFormat TimeFormat


{-| Configuration for date formats.
-}
type alias DateFormat =
    { pattern : DatePattern
    , separator : Char
    , placeholders : DateParts Char
    }


{-| Configuration for time formats.
-}
type alias TimeFormat =
    { separator : Char
    , placeholders : TimeParts Char
    , allowedTimesOfDay : Zone -> Posix -> { startHour : Int, startMinute : Int, endHour : Int, endMinute : Int }
    }


{-| Available date format patterns (defines the order of day, month and year).
-}
type DatePattern
    = DDMMYYYY
    | MMDDYYYY
    | YYYYMMDD
    | YYYYDDMM

```

So to configure your custom date format you can add the following properties to your picker's `Settings`:

```elm
import DatePicker.Settings exposing (Settings, defaultSettings)
import DatePicker.DateInput as DateInput

-- [...]

getErrorMessage : DateInput.InputError -> String
getErrorMessage error =
    case error of
        DateInput.ValueInvalid ->
            "Invalid value. Make sure to use the correct format."

        DateInput.ValueNotAllowed ->
            "Date not allowed."

        DateInput.DurationInvalid ->
            "End date is before start date."


userDefinedDatePickerSettings : Zone -> Posix -> Settings
userDefinedDatePickerSettings zone today =
    let
        defaults =
            defaultSettings zone

        dateFormat =
            { pattern = DDMMYYYY
            , separator = '.'
            , placeholders = { day = 'd', month = 'm', year = 'y' }
            }

        timeFormat =
            { separator = ':'
            , placeholders = { hour = 'h', minute = 'm' }
            , allowedTimesOfDay = \_ _ -> { startHour = 0, startMinute = 0, endHour = 23, endMinute = 59 }
            }
    in
    { defaults
        | -- [...]
        , dateInputSettings = 
            { format = DateInput.DateTime dateFormat timeFormat
            , getErrorMessage = getErrorMessage
            }
    }

```

As you can see in the code snippet above, the date input's validation can trigger three types of errors. You can define your own messages for each type.


## CSS & Theming

The CSS for the date picker is now defined in a built-in way using [elm-css](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/).
There are some design tokens that can be configured individually in a theme.
In case you need to add additional styling, you can use the CSS-classes that are attached to all the components. You'll find a list of all classes under `/css/DateTimePicker.css`.

In case you'd like to use the Theme, you can pass your custom theme to the `Settings`. The `Theme` record currently looks like this:

```elm
type alias Theme =
    { fontSize :
        { base : Float
        , sm : Float
        , xs : Float
        , xxs : Float
        }
    , color :
        { text :
            { primary : Css.Color
            , secondary : Css.Color
            , disabled : Css.Color
            , error : Css.Color
            }
        , primary :
            { main : Css.Color
            , contrastText : Css.Color
            , light : Css.Color
            }
        , background :
            { container : Css.Color
            , footer : Css.Color
            , presets : Css.Color
            , input : Css.Color
            }
        , action : { hover : Css.Color }
        , border : Css.Color
        }
    , size :
        { presetsContainer : Float
        , day : Float
        , iconButton : Float
        , inputElement : Float
        }
    , spacing : { base : Float }
    , borderWidth : Float
    , borderRadius :
        { base : Float
        , lg : Float
        }
    , boxShadow :
        { offsetX : Float
        , offsetY : Float
        , blurRadius : Float
        , spreadRadius : Float
        , color : Css.Color
        }
    , zIndex : Int
    , transition : { duration : Float }
    , classNamePrefix : String
    }
```

Passing a customized theme to the settings works like this:

```elm
import Css -- from elm-css
import DatePicker.Settings exposing (Settings, defaultSettings)
import DatePicker.Theme exposing (Theme, defaultTheme)

-- [...]

customTheme : Theme
customTheme =
    { defaultTheme
        | color =
            { text =
                { primary = Css.hex "22292f"
                , secondary = Css.rgba 0 0 0 0.5
                , disabled = Css.rgba 0 0 0 0.25
                }
            , primary =
                { main = Css.hex "3490dc"
                , contrastText = Css.hex "ffffff"
                , light = Css.rgba 52 144 220 0.1
                }
            , background =
                { container = Css.hex "ffffff"
                , footer = Css.hex "ffffff"
                , presets = Css.hex "ffffff"
                }
            , action = { hover = Css.rgba 0 0 0 0.08 }
            , border = Css.rgba 0 0 0 0.1
            }
    }


userDefinedDatePickerSettings : Zone -> Posix -> Settings
userDefinedDatePickerSettings zone today =
    let
        defaults =
            defaultSettings zone
    in
    { defaults
        | -- [...]
        , theme = customTheme
    }

```

## Examples

Examples can be found in the [examples](https://github.com/mercurymedia/elm-datetime-picker/tree/master/examples) folder. To view the examples in the browser run `npm install` and `npm start` from the root of the repository.
