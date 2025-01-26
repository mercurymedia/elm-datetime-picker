module DatePicker.Theme exposing (Theme, defaultTheme)

import Css


{-| The type facilitating the Theme with the most important design tokens
-}
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


{-| The default theme that is included in the defaultSettings
-}
defaultTheme : Theme
defaultTheme =
    { fontSize =
        { base = 16
        , sm = 14
        , xs = 12
        , xxs = 10
        }
    , color =
        { text =
            { primary = Css.hex "22292f"
            , secondary = Css.rgba 0 0 0 0.5
            , disabled = Css.rgba 0 0 0 0.25
            , error = Css.hex "#dc3434"
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
            , input = Css.hex "ffffff"
            }
        , action = { hover = Css.rgba 0 0 0 0.08 }
        , border = Css.rgba 0 0 0 0.1
        }
    , size =
        { presetsContainer = 150
        , day = 36
        , iconButton = 32
        , inputElement = 32
        }
    , borderWidth = 1
    , borderRadius =
        { base = 3
        , lg = 6
        }
    , boxShadow =
        { offsetX = 0
        , offsetY = 0
        , blurRadius = 5
        , spreadRadius = 0
        , color = Css.rgba 0 0 0 0.25
        }
    , zIndex = 100
    , transition = { duration = 300 }
    , classNamePrefix = "elm-datetimepicker"
    }
