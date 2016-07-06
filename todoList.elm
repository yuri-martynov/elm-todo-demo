module TodoList exposing (Model, Msg, view, update, newTask)

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
                |> List.filter filter
                |> List.map taskView
    in
        ul [] tasksView


update msg items =
    let
        todoItem id msg =
            let
                updateTask t =
                    if (t.id == id) then
                        { t | model = t.model |> TodoItem.update msg }
                    else
                        t
            in
                items |> List.map updateTask
    in
        case msg of
            TodoItem id msg ->
                todoItem id msg

            Delete id ->
                items |> List.filter (.id >> (/=) id)

newTask : Int -> String -> TodoItemWithId
newTask id description =
    { id = id, model = { description = description, isDone = False, newDescription = Nothing } }
