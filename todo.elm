module Main exposing (..)

import TodoItem
import Search
import TodoEntry
import Html exposing (..)
import Html.App exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String


type alias TodoItemWithId =
    { id : Int
    , model : TodoItem.Model
    }


type alias Model =
    { nextId : Int
    , newTask : TodoEntry.Model
    , search : Search.Model
    , hideDone : Bool
    , tasks : List TodoItemWithId
    }


emptyModel : Model
emptyModel =
    { nextId = 1
    , newTask = Nothing
    , tasks = []
    , search = Nothing
    , hideDone = False
    }


newTask : Int -> String -> TodoItemWithId
newTask id description =
    { id = id, model = { description = description, isDone = False, newDescription = Nothing } }


type Msg
<<<<<<< HEAD
    = UpdateNewTask String
    | UpdateSearch String
    | Add
=======
    = TodoEntryMsg TodoEntry.Msg
>>>>>>> d72a09c40e3dd60b8ddf3797f2849b07156350c0
    | HideDone
    | Delete Int
    | DeleteCompleted
    | TodoItemMsg Int TodoItem.Msg
    | SearchMsg Search.Msg


update' : Msg -> Model -> Model
update' msg model =
    let
        addNewTask model =
            case model.newTask of
                Nothing ->
                    model

                Just s ->
                    { model
                        | nextId = model.nextId + 1
                        , newTask = Nothing
                        , tasks = (newTask model.nextId s) :: model.tasks
                    }

       
        todoEntry msg model  = 
            { model | newTask = model.newTask |> TodoEntry.update msg }

        taskMsg id msg tasks =
            let
                updateTask t =
                    if (t.id == id) then
                        { t | model = (t.model |> TodoItem.update msg) }
                    else
                        t
            in
                tasks |> List.map updateTask
    in
        case msg of
<<<<<<< HEAD
            UpdateNewTask s ->
                case s of
                    "" ->
                        { model | newTask = Nothing }

                    _ ->
                        { model | newTask = Just s }

            Add ->
                addNewTask model

            UpdateSearch s ->
                case s of
                    "" ->
                        { model | search = Nothing }

                    _ ->
                        { model | search = Just s }

=======
>>>>>>> d72a09c40e3dd60b8ddf3797f2849b07156350c0
            HideDone ->
                { model | hideDone = not model.hideDone }

            Delete id ->
                { model | tasks = model.tasks |> List.filter (\t -> t.id /= id) }

            DeleteCompleted ->
                { model | tasks = model.tasks |> List.filter (.model >> .isDone >> not) }

            TodoItemMsg id msg ->
                { model | tasks = model.tasks |> taskMsg id msg }

            SearchMsg msg ->
                { model | search = model.search |> Search.update msg }

            TodoEntryMsg (TodoEntry.Enter as msg) ->
                model
                |> addNewTask >> (todoEntry msg)
            
            TodoEntryMsg msg ->
                todoEntry msg model


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    (update' msg model) ! []


view : Model -> Html Msg
view model =
    let
        toStr =
            Maybe.withDefault ""

        hideDoneView hideDone tasks =
            if hasDone tasks then
                div []
                    [ input
                        [ type' "checkbox"
                        , checked hideDone
                        , onClick HideDone
                        ]
                        []
                    , text "hide done"
                    , button [ onClick DeleteCompleted ]
                        [ text "Delete completed" ]
                    ]
            else
                div [] []

        taskListView tasks search hideDone =
            let
                searchFilter tasks =
                    case search of
                        Nothing ->
                            tasks

                        Just s ->
                            tasks |> List.filter (\t -> t.model.description |> String.contains s)

                doneFilter tasks =
                    if (hideDone) then
                        tasks |> List.filter (\t -> t.model.isDone == False)
                    else
                        tasks

                filter =
                    doneFilter >> searchFilter

                tasksView =
                    tasks
                        |> filter
                        |> List.map taskView
            in
                ul [] tasksView

        taskView : TodoItemWithId -> Html Msg
        taskView { id, model } =
            let
                deleteButtonView =
                    button [ onClick (Delete id) ]
                        [ text "x" ]
            in
                li []
                    [ map (TodoItemMsg id) (TodoItem.view model)
                    , deleteButtonView
                    ]

        summaryView tasks =
            if tasks == [] then
                div [] [ text "Welcome. Add a task to start" ]
            else
                let
                    notDone =
                        tasks |> List.filter (.isDone >> not) |> List.length
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

        pluralizeItems =
            pluralize ( "item", "items" )

        hasDone tasks =
            tasks |> List.any (.model >> .isDone)
    in
        div []
            [ map TodoEntryMsg (TodoEntry.view model.newTask)
            , map SearchMsg (Search.view model.search)
            , taskListView model.tasks model.search model.hideDone
            , summaryView (model.tasks |> List.map (.model))
            , hideDoneView model.hideDone model.tasks
            ]


init =
    emptyModel ! []


main =
    program { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
