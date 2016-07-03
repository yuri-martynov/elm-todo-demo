module Main exposing (..)

import TodoItem
import Search
import TodoEntry
import Summary
import Controls
import Html exposing (..)
import Html.App exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String


type alias TodoItem =
    TodoItem.Model { id : Int }


type alias Model =
    { nextId : Int
    , newTask : TodoEntry.Model
    , search : Search.Model
    , hideDone : Bool
    , tasks : List TodoItem
    }


emptyModel : Model
emptyModel =
    { nextId = 1
    , newTask = ""
    , tasks = []
    , search = ""
    , hideDone = False 
    }


newTask : Int -> String -> TodoItem
newTask id description =
    { id = id, description = description, isDone = False, newDescription = Nothing }


type Msg
    = Add
    | Delete Int
    | TodoEntryMsg TodoEntry.Msg
    | SearchMsg Search.Msg
    | TodoItemMsg Int TodoItem.Msg
    | Controls Controls.Msg


update' : Msg -> Model -> Model
update' msg model =
    let
        addNewTask model =
            case model.newTask of
                "" ->
                    model

                s ->
                    { model
                        | nextId = model.nextId + 1
                        , newTask = ""
                        , tasks = (newTask model.nextId s) :: model.tasks
                    }

        todoEntry msg model =
            { model | newTask = model.newTask |> TodoEntry.update msg }

        taskMsg id msg tasks =
            let
                updateTask t =
                    if (t.id == id) then
                        t |> TodoItem.update msg
                    else
                        t
            in
                tasks |> List.map updateTask
    in
        case msg of
            Add ->
                addNewTask model

            Controls msg ->
                model |> Controls.update msg

            Delete id ->
                { model | tasks = model.tasks |> List.filter (\t -> t.id /= id) }

            TodoItemMsg id msg ->
                { model | tasks = model.tasks |> taskMsg id msg }

            SearchMsg msg ->
                { model | search = model.search |> Search.update msg }

            TodoEntryMsg ((TodoEntry.Enter) as msg) ->
                model |> addNewTask >> (todoEntry msg)

            TodoEntryMsg msg ->
                todoEntry msg model


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    (update' msg model) ! []


view : Model -> Html Msg
view model =
    let
        taskListView model  =
            let
                searchFilter tasks =
                    case model.search of
                        "" ->
                            tasks

                        s ->
                            tasks |> List.filter (\t -> t.description |> String.contains s)

                doneFilter tasks =
                    if (model.hideDone) then
                        tasks |> List.filter (\t -> t.isDone == False)
                    else
                        tasks

                filter =
                    doneFilter >> searchFilter

                tasksView =
                    model.tasks
                        |> filter
                        |> List.map taskView
            in
                ul [] tasksView

        taskView : TodoItem -> Html Msg
        taskView task =
            let
                deleteButtonView =
                    button [ onClick (Delete task.id) ]
                        [ text "x" ]
            in
                li []
                    [ map (TodoItemMsg task.id) (TodoItem.view task)
                    , deleteButtonView
                    ]
    in
        div []
            [ map TodoEntryMsg (TodoEntry.view model.newTask)
            , map SearchMsg (Search.view model.search)
            , map Controls (Controls.view model)
            , taskListView model
            , Summary.view model.tasks
            ]


init =
    emptyModel ! []


main =
    program { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
