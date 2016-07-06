module Controls exposing (Msg(..), update, view, filter)

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


filter model =
    let
        searchFilter =
            case model.search of
                "" ->
                    \_ -> True

                s ->
                    .description >> contains s

        doneFilter =
            if (model.hideDone) then
                .isDone >> not
            else
                \_ -> True
    in
        \t -> doneFilter t.model && searchFilter t.model 


hasDone tasks =
    tasks |> List.any (.model >> .isDone)
