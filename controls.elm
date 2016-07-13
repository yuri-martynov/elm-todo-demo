module Controls exposing (Msg(..), update, view)

import TodoList exposing (hasDone)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type Msg
    = DeleteCompleted


view model =
    if TodoList.hasDone model.tasks then
        div []
            [ button [ onClick DeleteCompleted ]
                [ text "Delete completed" ]
            ]
    else
        div [] []


update msg model =
    case msg of
        DeleteCompleted ->
            { model | tasks = model.tasks |> List.filter (.model >> .isDone >> not) }

