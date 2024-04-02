module DatePicker.Styles exposing (durationDayClasses, durationStartOrEndClasses, singleDayClasses)


durationDayClasses : String -> Bool -> Bool -> Bool -> Bool -> Bool -> String
durationDayClasses classPrefix isHidden isDisabled isPicked isToday isBetween =
    let
        hiddenClass =
            calenderDayClass classPrefix "hidden" isHidden

        disabledClass =
            calenderDayClass classPrefix "disabled" isDisabled

        pickedClass =
            calenderDayClass classPrefix "picked" isPicked

        todayClass =
            calenderDayClass classPrefix "today" isToday

        betweenClass =
            calenderDayClass classPrefix "between" isBetween

        defaultClass =
            classPrefix ++ "calendar-day"
    in
    String.join " " [ hiddenClass, disabledClass, pickedClass, todayClass, betweenClass, defaultClass ]


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
    let
        hiddenClass =
            calenderDayClass classPrefix "hidden" isHidden

        disabledClass =
            calenderDayClass classPrefix "disabled" isDisabled

        pickedClass =
            calenderDayClass classPrefix "picked" isPicked

        todayClass =
            calenderDayClass classPrefix "today" isToday

        defaultClass =
            classPrefix ++ "calendar-day"
    in
    String.join " " [ hiddenClass, disabledClass, pickedClass, todayClass, defaultClass ]


calenderDayClass : String -> String -> Bool -> String
calenderDayClass classPrefix classSuffix condition =
    if condition then
        classPrefix ++ "calendar-day " ++ classPrefix ++ classSuffix

    else
        ""
