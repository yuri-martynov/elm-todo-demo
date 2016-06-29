module Main exposing (..)

import TodoItem

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import String
import List.Extra


type alias WithId baseType idType =  
    { baseType | id : idType }

type alias TodoItemWithId = WithId TodoItem.Model Int


type alias Model =
    { nextId : Int
    , newTask : Maybe String
    , search : Maybe String
    , hideDone : Bool
    , tasks : List TodoItemWithId
    }


emptyModel : Model
emptyModel =
    { nextId = 1, newTask = Nothing, tasks = [], search = Nothing, hideDone = False }


newTask : Int -> String -> TodoItemWithId
newTask id description =
    { id = id, description = description, isDone = False, newDescription = Nothing }


type Msg
    = NoOp
    | UpdateNewTask String
    | UpdateSearch String
    | Add
    | Done Int
    | HideDone
    | StartEditing Int
    | FinishEditing Int
    | TaskChanged Int String 
    | CancelEditing Int
    | Delete Int
    | DeleteCompleted


update' : Msg -> Model -> Model
update' msg model =
    let
        updateTask' : List a -> (a -> Bool) -> (a -> a) -> List a
        updateTask' tasks find update =
            let
                ( start, end ) =
                    tasks |> List.Extra.break find
            in
                case end of
                    [] ->
                        start

                    t :: rest ->
                        start ++ ((update t) :: rest)

        updateTask id =
            updateTask' model.tasks (\t -> t.id == id)

        markDone t =
            { t | isDone = not t.isDone }

        startEditing t =
            { t | newDescription = Just t.description }

        finishEditing t =
            case t.newDescription of
                Nothing  ->
                    t

                Just "" ->
                    t

                Just s ->
                    { t | description = s, newDescription = Nothing }

        cancelEditing t =
            { t | newDescription = Nothing }

        changeDescription s t =
            { t | newDescription = Just s }
                

        addNewTask model =
            case model.newTask of
                Nothing -> model
                Just s ->
                    { model
                        | nextId = model.nextId + 1
                        , newTask = Nothing
                        , tasks = (newTask model.nextId s) :: model.tasks
                    }
    in
        case msg of
            NoOp ->
                model

            UpdateNewTask s ->
                case s of
                    "" -> { model | newTask = Nothing }
                    _ -> { model | newTask = Just s }

            Add ->
                addNewTask model

            UpdateSearch s ->
                case s of
                    "" -> { model | search = Nothing }
                    _ ->  { model | search = Just s }

            Done id ->
                { model | tasks = updateTask id markDone }

            StartEditing id ->
                { model | tasks = updateTask id startEditing }

            FinishEditing id ->
                { model | tasks = updateTask id finishEditing }

            HideDone ->
                { model | hideDone = not model.hideDone }

            TaskChanged id s  ->
                { model | tasks = updateTask id (changeDescription s) }

            CancelEditing id ->
                { model | tasks = updateTask id cancelEditing }

            Delete id ->
                { model | tasks = model.tasks |> List.filter (\t -> t.id /= id)}

            DeleteCompleted ->
                { model | tasks = model.tasks |> List.filter (.isDone >> not)}


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    (update' msg model) ! []


view : Model -> Html Msg
view model =
    let
        toStr = Maybe.withDefault ""
        newTaskView newTask =
            input
                [ placeholder "Enter new task"
                , value (newTask |> toStr)
                , onInput UpdateNewTask
                , onEnter Add
                ]
                []

        searchView search =
            input
                [ placeholder "search"
                , value (search |> toStr)
                , onInput UpdateSearch
                ]
                []

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
                    , button
                        [ onClick DeleteCompleted ]
                        [ text "Delete completed"]
                    ]
            else
                div [] []

        taskListView tasks search hideDone =
            let
                searchFilter tasks =
                    case search of
                        Nothing -> tasks
                        Just s ->
                            tasks |> List.filter (\t -> t.description |> String.contains s)

                doneFilter tasks =
                    if (hideDone) then
                        tasks |> List.filter (\t -> not t.isDone)
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
        taskView task =
            let
                descriptionView =
                    case task.newDescription of
                        Nothing ->
                            label [ onDoubleClick (StartEditing task.id) ] [ text task.description ]

                        Just s ->
                            input
                                [ value s
                                , onInput (\s -> TaskChanged task.id s)
                                , onEnterOrEscape (FinishEditing task.id) (CancelEditing task.id) 
                                ]
                                []

                deleteButtonView = 
                    button 
                        [ onClick (Delete task.id) ] 
                        [ text "x"]
            in
                li []
                    [ input
                        [ type' "checkbox"
                        , checked task.isDone
                        , onClick (Done task.id)
                        ]
                        []
                    , descriptionView
                    , deleteButtonView
                    ]

        summaryView tasks = 
            if tasks == [] then
                div [] [ text "Welcome. Add a task to start" ]
            else
                let 
                    notDone = tasks |> List.filter (.isDone >> not) |> List.length
                in
                    div [] [ text (pluralizeItems notDone ++ " remainig") ]

        pluralize (singular, plural) number =
            let 
                word = 
                    if (number == 1) then singular
                    else plural
                numberStr = toString number
            in 
                numberStr ++ " " ++ word

        pluralizeItems = pluralize ("item", "items")

        hasDone tasks = tasks |> List.any (.isDone)
        
        onKeyUp fail options =
            let
                tagger options code =
                    case options of
                        [] ->
                            fail

                        ( c, msg ) :: rest ->
                            if (c == code) then
                                msg
                            else
                                tagger rest code
            in
                on "keyup" (Json.map (tagger options) keyCode)

        onEnter msg =
            onKeyUp NoOp [ ( 13, msg ) ]

        onEnterOrEscape enter escape =
            onKeyUp NoOp [(13, enter), (27, escape)]
    in
        div []
            [ newTaskView model.newTask
            , searchView model.search
            , taskListView model.tasks model.search model.hideDone
            , summaryView model.tasks
            , hideDoneView model.hideDone model.tasks
            ]


init =
    emptyModel ! []


main =
    Html.program { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
