module Controls exposing (Msg(..), update, view, filter)

import TodoItem exposing (Model)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String exposing (..)


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
            { model | tasks = model.tasks |> List.filter (.model >> .isDone >> not) }


filter : String -> Bool -> (TodoItem.Model -> Bool)
filter search hideDone =
    let
        searchFilter =
            case search of
                "" ->
                    \_ -> True

                s ->
                    .description >> .text >> contains s

        doneFilter =
            if (hideDone) then
                .isDone >> not
            else
                \_ -> True
    in
        \t -> doneFilter t && searchFilter t


hasDone tasks =
    tasks |> List.any (.model >> .isDone)
