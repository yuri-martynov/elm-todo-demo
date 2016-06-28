module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import String
import List.Extra


type alias Task =
    { id : Int
    , description : String
    , isDone : Bool
    , newDescription : Maybe String
    }


type alias Model =
    { nextId : Int
    , newTask : String
    , search : String
    , hideDone : Bool
    , tasks : List Task
    }


emptyModel : Model
emptyModel =
    { nextId = 1, newTask = "", tasks = [], search = "", hideDone = False }


newTask : Int -> String -> Task
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
    | TaskChanged ( Int, String )
    | CancelEditing Int


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
                Nothing ->
                    t

                Just "" ->
                    t

                Just s ->
                    { t | description = s, newDescription = Nothing }

        cancelEditing t =
            { t | newDescription = Nothing }

        changeDescription newDescription t =
            { t | newDescription = Just newDescription }

        addNewTask model =
            case model.newTask of
                "" ->
                    model

                _ ->
                    { model
                        | nextId = model.nextId + 1
                        , newTask = ""
                        , tasks = (newTask model.nextId model.newTask) :: model.tasks
                    }
    in
        case msg of
            NoOp ->
                model

            UpdateNewTask s ->
                { model | newTask = s }

            Add ->
                addNewTask model

            UpdateSearch s ->
                { model | search = s }

            Done id ->
                { model | tasks = updateTask id markDone }

            StartEditing id ->
                { model | tasks = updateTask id startEditing }

            FinishEditing id ->
                { model | tasks = updateTask id finishEditing }

            HideDone ->
                { model | hideDone = not model.hideDone }

            TaskChanged ( id, s ) ->
                { model | tasks = updateTask id (changeDescription s) }

            CancelEditing id ->
                { model | tasks = updateTask id cancelEditing }


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    (update' msg model) ! []


view : Model -> Html Msg
view model =
    let
        newTaskView newTask =
            input
                [ placeholder "Enter new task"
                , value newTask
                , onInput UpdateNewTask
                , onEnter Add
                ]
                []

        searchView search =
            input
                [ placeholder "search"
                , value search
                , onInput UpdateSearch
                ]
                []

        hideDoneView hideDone =
            div []
                [ input
                    [ type' "checkbox"
                    , checked hideDone
                    , onClick HideDone
                    ]
                    []
                , text "hide done"
                ]

        taskListView tasks search hideDone =
            let
                searchFilter tasks =
                    if (String.isEmpty search) then
                        tasks
                    else
                        tasks |> List.filter (\t -> t.description |> String.contains search)

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

        taskView : Task -> Html Msg
        taskView task =
            let
                descriptionView =
                    case task.newDescription of
                        Nothing ->
                            label [ onDoubleClick (StartEditing task.id) ] [ text task.description ]

                        Just s ->
                            input
                                [ value s
                                , onInput (\s -> TaskChanged ( task.id, s ))
                                , onKeyUp NoOp [ ( 13, (FinishEditing task.id) ), ( 27, (CancelEditing task.id) ) ]
                                ]
                                []
            in
                li []
                    [ input
                        [ type' "checkbox"
                        , checked task.isDone
                        , onClick (Done task.id)
                        ]
                        []
                    , descriptionView
                    ]

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
    in
        div []
            [ newTaskView model.newTask
            , searchView model.search
            , hideDoneView model.hideDone
            , taskListView model.tasks model.search model.hideDone
            ]


init =
    emptyModel ! []


main =
    Html.program { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
