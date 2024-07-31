# Changelog

NOTE: as is the case in the README, all code snippets below are specific to the `SingleDatePicker`; however, the only real difference between the `SingleDatePicker` and `DurationDatePicker` from an API standpoint is the `Msg` that a user needs to define to handle updates. Keep this in mind when making updates to your code.

## [9.0.1]

### **CHANGED**
- Improved positioning of datepickers opened outside hierarchy. Based on the viewport size and position of the trigger element the datepicker popover will be aligned in a predefined priority: 1. align left of trigger element, 2. align right of trigger element, 3. align center of trigger element, 4. align left of viewport
- Fix a few general styles for new presets


## [9.0.0]

### **ADDED**
New fields have been added to the `Settings` to receive date or date range presets. Example date range presets have been added to the README and the Duration example.

## [8.0.1]

- Fix time picker visibility for SingleDatePicker

## [8.0.0]

### **ADDED**
Added functions to render the picker outside the DOM hierarchy while positioning it manually attached to a trigger element (`Datepicker.openPickerOutsideDomHierarchy` and `Datepicker.updatePickerPosition`). 
Also added a new example for that use case. See `BasicModal` example and README.

## [7.0.2]

- Removed duplicate `#month` ID selector for SingleDatePicker

## [7.0.1]

- Fixed a bug where the time was not visible for whole days (00:00 - 23:59)

## [7.0.0]

### **MAJOR/BREAKING CHANGE**

Previously, the DatePicker was updated in the view, and the updated DatePicker was then passed to the user's Msg. This could result in race conditions generating incorrect selection data. In order to prevent undesirable behavior as a result, the DatePicker should now be updated from the update function. This will result in a breaking change for any user, but should be relatively straightforward to fix.

The signature of the provided Msg for the DatePicker has changed:

```elm
-- from something like:
    | UpdatePicker ( DatePicker.DatePicker, Maybe Posix )

-- to now something like:
    | UpdatePicker SingleDatePicker.Msg
```

The implementation of the Msg now needs to call the update of the DatePicker to get the updated picker and the current selection:

```elm
-- from something like:
    UpdatePicker ( newPicker, maybeNewTime ) ->
        ( { model | picker = newPicker, pickedTime = Maybe.map (\t -> Just t) maybeNewTime |> Maybe.withDefault model.pickedTime }, Cmd.none )

-- to now something like:
    UpdatePicker subMsg ->
        let
            ( newPicker, maybeNewTime ) =
                SingleDatePicker.update (userDefinedDatePickerSettings model.zone model.currentTime) subMsg model.picker
        in
        ( { model | picker = newPicker, pickedTime = Maybe.map (\t -> Just t) maybeNewTime |> Maybe.withDefault model.pickedTime }, Cmd.none )
```

Additionally, the internalMsg is no longer provided on the `Settings` but instead provided on the `init` call for the DatePicker.

### Thanks to [patbro](https://github.com/patbro) for pointing this out.

## [6.0.0]

### **ADDED**

To keep track of the calendar weeks when picking dates, calendar week numbers have been added to the pickers as a separate column. This feature is optional and can be enabled with the new `Settings` entry:

```elm
type alias Settings msg =
    { {- [...] -}
    , showCalendarWeekNumbers : Bool
    }
```

## [5.0.2]

### **CHANGED**

To improve the visual design the markup and styling have been reworked. The files `SingleDatePicker.css` and `DurationDatePicker.css` have been merged and replaced by a shared CSS resource – `DateTimePicker.css` – for both picker variants (CSS classnames and -prefixes may have changed or extended during that process).
Also, the most important design tokens (sizes, colors, etc.) have been outsourced into CSS variables and collected in a theme file (`DateTimePickerTheme.css`) to allow easy styling customization.

## [5.0.1]

Automatically sets the lastest seconds when selecting an end date in the `DurationDatePicker` (e.g. 12:30:59).

## [4.0.1]

### **PATCH**

Fix `DurationDatePicker` docs.

## [4.0.0]

### **MAJOR/BREAKING CHANGE**

To allow greater flexibility regarding time picker visibility the dateTime picker `Settings` have been reworked.

Old settings:

```elm
type alias Settings msg =
    { internalMsg : ( DatePicker, Maybe Posix ) -> msg
    , zone : Zone
    , formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , focusedDate : Maybe Posix
    , dateTimeProcessor :
        { isDayDisabled : Zone -> Posix -> Bool
        , allowedTimesOfDay :
            Zone
            -> Posix
            ->
                { startHour : Int
                , startMinute : Int
                , endHour : Int
                , endMinute : Int
                }
        }
    , dateStringFn : Zone -> Posix -> String
    , timeStringFn : Zone -> Posix -> String
    , isFooterDisabled : Bool
    , isFullDayEnabled : Bool
    }
```

New settings:

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

type TimePickerVisibility
    = NeverVisible
    | Toggleable TimePickerSettings
    | AlwaysVisible TimePickerSettings

type alias TimePickerSettings =
    { timeStringFn : Zone -> Posix -> String
    , allowedTimesOfDay :
        Zone
        -> Posix
        ->
            { startHour : Int
            , startMinute : Int
            , endHour : Int
            , endMinute : Int
            }
    }
```

The `TimePickerVisibility` combined with `TimePickerSettings` have replaced the `dateTimeProcessor` field from the prior versions. The `isFooterDisabled` & `isFullDayEnabled` fields which had previously controlled time picker visibility & helped determine time boundaries for a selected day have been removed entirely.

Users can now choose to have the time picker `NeverVisible`, `Toggleable`, or `AlwaysVisible`. All fields related to time (`timeStringFn` & `allowedTimesOfDay`) have been moved to `TimePickerSettings`, which only need to be provided for the cases in which the time picker is (potentially) visible. It is worth noting that even when the time picker is hidden/`NeverVisible` a day within the picker runs from `00:00` to `23:59`. When a selection is made in this case, the start of the selected day, `00:00`, is returned as the time of the selected day.

### **MAJOR/BREAKING CHANGE**

The `openPicker` function for both modules now expects `Settings` as the first argument instead of a `Time.Zone`.

Old:

```elm
openPicker : Zone -> Posix -> Maybe Posix -> DatePicker -> DatePicker
```

New:

```elm
openPicker : Settings msg -> Posix -> Maybe Posix -> DatePicker -> DatePicker
```

## [3.0.0]

### **MAJOR/BREAKING CHANGE**

Two fields have been added to the `Settings` type: `isFooterDisabled` & `isFullDayEnabled`. `isFooterDisabled` allows the time picker in the dateTime picker to be hidden, ostensibly allowing a user to select just a day and
not necessarily a time of day. The `isFullDayEnabled` field was introduced to ensure that, when the time picker is hidden, a day on the picker day would start at `00:00` and end at `23:59`.

Old settings:

```elm
type alias Settings msg =
    { internalMsg : ( DatePicker, Maybe Posix ) -> msg
    , zone : Zone
    , formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , focusedDate : Maybe Posix
    , dateTimeProcessor :
        { isDayDisabled : Zone -> Posix -> Bool
        , allowedTimesOfDay :
            Zone
            -> Posix
            ->
                { startHour : Int
                , startMinute : Int
                , endHour : Int
                , endMinute : Int
                }
        }
    , dateStringFn : Zone -> Posix -> String
    , timeStringFn : Zone -> Posix -> String
    }
```

New settings:

```elm
type alias Settings msg =
    { internalMsg : ( DatePicker, Maybe Posix ) -> msg
    , zone : Zone
    , formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , focusedDate : Maybe Posix
    , dateTimeProcessor :
        { isDayDisabled : Zone -> Posix -> Bool
        , allowedTimesOfDay :
            Zone
            -> Posix
            ->
                { startHour : Int
                , startMinute : Int
                , endHour : Int
                , endMinute : Int
                }
        }
    , dateStringFn : Zone -> Posix -> String
    , timeStringFn : Zone -> Posix -> String
    , isFooterDisabled : Bool
    , isFullDayEnabled : Bool
    }
```

## [2.0.3]

### **PATCH**

Add html `id`s/`class`es to `DurationDatePicker` elements for testing.

## [2.0.2]

### **PATCH**

Add html `id`s/`class`es to `SingleDatePicker` elements for testing.

## [2.0.1]

### **PATCH**

Fix time of day validation for both pickers.

## [2.0.0]

### **MAJOR/BREAKING CHANGE**

The dateTime picker API has been simplified while also allowing the selection of partial days (limiting times of day that are selectable for a given day). This has led to major changes in the `Settings` type and the way that picker updates are processed.

#### User defined Msg

Previously, an implementor needed to define two `Msg`s: one to handle picker updates & another to handle when a selection has been made. Now, only one message needs to be defined that expects a `Tuple` containing an updated picker instance and a `Maybe Posix` representing the selected datetime.

So where an implementor's `Msg`s may have previously looked like this:

```elm
type Msg
    = ...
    | Selected Posix
    | UpdatePicker SingleDatePicker.DatePicker
```

Now they look like this:

```elm
type Msg
    = ...
    | UpdatePicker ( SingleDatePicker.DatePicker, Maybe Posix )
```

#### Settings changes

The `today` has been renamed to `focusedDate`.

New functions:
`allowedTimesOfDay` - determine the selectable time boundaries for a given `Zone` and day (`Posix`)
`dateStringFn` - function to represent the selected day
`timeStringFn` - function to represent the selected time of day

Additionally, the dateTime picker now takes time zones into account. As such, the `Settings` type now expects a `Time.Zone` to be provided to it. The `defaultSettings` fn, `allowedTimesOfDay` fn, `dateStringFn`, and `timeStringFn` all require a `Time.Zone` to be passed as the first argument.

Old settings:

```elm
type alias Settings msg =
    { internalMsg : DatePicker -> msg
    , selectedMsg : Posix -> msg
    , formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , today : Maybe Posix
    , dayDisabled : Posix -> Bool
    }

type alias MsgConfig msg =
    { internalMsg : DatePicker -> msg
    , externalMsg : Posix -> msg
    }

defaultSettings : MsgConfig msg -> Settings msg
```

New settings:

```elm
type alias Settings msg =
    { internalMsg : ( DatePicker, Maybe Posix ) -> msg
    , zone : Zone
    , formattedDay : Weekday -> String
    , formattedMonth : Month -> String
    , focusedDate : Maybe Posix
    , dateTimeProcessor :
        { isDayDisabled : Zone -> Posix -> Bool
        , allowedTimesOfDay :
            Zone
            -> Posix
            ->
                { startHour : Int
                , startMinute : Int
                , endHour : Int
                , endMinute : Int
                }
        }
    , dateStringFn : Zone -> Posix -> String
    , timeStringFn : Zone -> Posix -> String
    }

defaultSettings : Zone -> (( DatePicker, Maybe Posix ) -> msg) -> Settings msg
```

### **MAJOR/BREAKING CHANGE**

The `openPicker` function for both modules now expects `Time.Zone` as the first argument.

Old:

```elm
openPicker : Posix -> Maybe Posix -> DatePicker -> DatePicker
```

New:

```elm
openPicker : Zone -> Posix -> Maybe Posix -> DatePicker -> DatePicker
```

### **MAJOR/BREAKING CHANGE**

The `subscriptions` function for both modules now expects the configured `Settings` as the first argument.

Old:

```elm
subscriptions : (( DatePicker, Maybe Posix ) -> msg) -> DatePicker -> Sub msg
```

New:

```elm
subscriptions : Settings msg -> (( DatePicker, Maybe Posix ) -> msg) -> DatePicker -> Sub msg
```

### Additional Minor changes

Added utility functions to examples to avoid compilation errors.

## [1.1.0]

### **MINOR CHANGE**

Add `isOpen` function to both pickers to allow a user to query if the picker is open.

## [1.0.1]

### **PATCH**

Add tests for Utilities
