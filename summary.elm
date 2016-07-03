module Summary exposing (view)

import Html exposing (..)


view model =
    if model == [] then
        div [] [ text "Welcome. Add a task to start" ]
    else
        let
            notDone =
                model
                    |> List.filter (.isDone >> not)
                    |> List.length

            pluralizeItems =
                pluralize ( "item", "items" )
        in
            div [] [ text (pluralizeItems notDone ++ " remainig") ]


pluralize ( singular, plural ) number =
    let
        word =
            if (number == 1) then
                singular
            else
                plural

        numberStr =
            toString number
    in
        numberStr ++ " " ++ word
