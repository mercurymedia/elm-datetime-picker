import "./index.css";
import "../css/DateTimePickerTheme.css";
import "../css/DateTimePicker.css";
import { Elm } from "./src/Main.elm";

Elm.Main.init({ node: document.getElementById("app") });
