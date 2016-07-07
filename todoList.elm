module TodoList exposing (Model, Msg, view, update, initItem)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (..)
import TodoItem exposing (Model, Msg, view, update)


type alias TodoItemWithId =
    { id : Int
    , model : TodoItem.Model
    }


type alias Model =
    List TodoItemWithId


type Msg
    = Delete Int
    | TodoItem Int TodoItem.Msg


view : Model -> (TodoItem.Model -> Bool) -> Html Msg
view items filter =
    let
        taskView task =
            let
                deleteButtonView =
                    button [ onClick (Delete task.id) ]
                        [ text "x" ]
            in
                li []
                    [ map (TodoItem task.id) (TodoItem.view task.model)
                    , deleteButtonView
                    ]

        tasksView =
            items
                |> List.filter (.model >> filter)
                |> List.map taskView
    in
        ul [] tasksView


update msg items =
    let
        updateTask id msg t =
            if (t.id == id) then
                { t | model = t.model |> TodoItem.update msg }
            else
                t
    in
        case msg of
            TodoItem id msg ->
                items |> List.map (updateTask id msg)

            Delete id ->
                items |> List.filter (.id >> (/=) id)


initItem : Int -> String -> TodoItemWithId
initItem id description =
    { id = id, model = TodoItem.init description }
