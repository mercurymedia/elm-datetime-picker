module DatePicker.Styles exposing (durationDayClasses, durationStartOrEndClasses, singleDayClasses)


durationDayClasses : String -> Bool -> Bool -> Bool -> Bool -> Bool -> String
durationDayClasses classPrefix isHidden isDisabled isPicked isToday isBetween =
    if isHidden then
        classPrefix ++ "calendar-day " ++ classPrefix ++ "hidden"

    else if isDisabled then
        classPrefix ++ "calendar-day " ++ classPrefix ++ "disabled"

    else if isPicked then
        classPrefix ++ "calendar-day " ++ classPrefix ++ "picked"

    else if isBetween && isToday then
        classPrefix ++ "calendar-day " ++ classPrefix ++ "today-between"

    else if isToday then
        classPrefix ++ "calendar-day " ++ classPrefix ++ "today"

    else if isBetween then
        classPrefix ++ "calendar-day " ++ classPrefix ++ "between"

    else
        classPrefix ++ "calendar-day"


durationStartOrEndClasses : String -> Bool -> Bool -> String
durationStartOrEndClasses classPrefix isStart isEnd =
    if isStart && isEnd then
        classPrefix ++ "start " ++ classPrefix ++ "end"

    else if isStart then
        classPrefix ++ "start"

    else if isEnd then
        classPrefix ++ "end"

    else
        ""


singleDayClasses : String -> Bool -> Bool -> Bool -> Bool -> String
singleDayClasses classPrefix isHidden isDisabled isPicked isToday =
    if isHidden then
        classPrefix ++ "calendar-day " ++ classPrefix ++ "hidden"

    else if isDisabled then
        classPrefix ++ "calendar-day " ++ classPrefix ++ "disabled"

    else if isPicked then
        classPrefix ++ "calendar-day " ++ classPrefix ++ "picked"

    else if isToday then
        classPrefix ++ "calendar-day " ++ classPrefix ++ "today"

    else
        classPrefix ++ "calendar-day"
