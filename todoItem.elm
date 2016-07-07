module TodoItem exposing (Model, Msg, update, view, init)

import EditableInput
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (map)
import Html.Lazy exposing (lazy)


type alias Model =
    { description : EditableInput.Model
    , isDone : Bool
    }


type Msg
    = Done
    | Description EditableInput.Msg


update msg model =
    case msg of
        Description msg ->
            { model | description = model.description |> EditableInput.update msg }

        Done ->
            { model | isDone = not model.isDone }


view model =
    let
        doneView =
            input
                [ type' "checkbox"
                , checked model.isDone
                , onClick Done
                ]
                []
    in
        span []
            [ doneView
            , map Description (lazy EditableInput.view model.description)
            ]


init : String -> Model
init description =
    { description = EditableInput.init description, isDone = False }
