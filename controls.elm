module Controls exposing (Msg(..), update, view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type Msg
    = HideDone
    | DeleteCompleted


view model =
    if hasDone model.tasks then
        div []
            [ input
                [ type' "checkbox"
                , checked model.hideDone
                , onClick HideDone
                ]
                []
            , text "hide done"
            , button [ onClick DeleteCompleted ]
                [ text "Delete completed" ]
            ]
    else
        div [] []


update msg model =
    case msg of
        HideDone ->
            { model | hideDone = not model.hideDone }

        DeleteCompleted ->
            { model | tasks = model.tasks |> List.filter (.isDone >> not) }

hasDone tasks =
    tasks |> List.any (.isDone)
