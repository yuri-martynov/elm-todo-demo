module TodoList exposing (Model, Msg, view, update, initItem, hasDone)

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
        deleteButtonView item =
            button [ onClick (Delete item.id) ] [ text "x" ]

        styles filter item =
            if filter item then
                []
            else
                [("display", "none")]

        taskView filter item  =
                li [style (styles filter item)]
                    [ map (TodoItem item.id) (TodoItem.view item.model)
                    , deleteButtonView item
                    ]

        tasksView =
            items
                |> List.map (taskView (.model >> filter))
    in
        ul [] tasksView


update: Msg -> Model -> Model
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


-- public

hasDone: Model -> Bool
hasDone tasks =
    tasks |> List.any (.model >> .isDone)

