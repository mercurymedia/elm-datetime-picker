module Task.Extra exposing (andMap)

import Task exposing (Task)

andMap : Task x a -> Task x (a -> b) -> Task x b
andMap =
    Task.map2 (|>)
