module Main exposing (..)

import Html exposing (..)
import Html.App exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import String
import List.Extra


type alias Task =
    { id : Int
    , description : String
    , isDone : Bool
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
    { id = id, description = description, isDone = False }


type Msg
    = NoOp
    | UpdateNewTask String
    | UpdateSearch String
    | Add
    | Done Int
    | HideDone


update : Msg -> Model -> Model
update msg model =
    let
        markDone : List Task -> Int -> List Task
        markDone tasks id =
            let
                ( start, end ) =
                    tasks
                        |> List.Extra.break (\t -> t.id == id)
            in
                case end of
                    [] ->
                        start

                    t :: rest ->
                        start ++ ({ t | isDone = not t.isDone } :: rest)

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
                { model | tasks = markDone model.tasks id }

            HideDone ->
                { model | hideDone = not model.hideDone }


view : Model -> Html Msg
view model =
    let
        newTaskView newTask =
            input
                [ placeholder "Enter new task"
                , value newTask
                , onInput UpdateNewTask
                , onEnter NoOp Add
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
                    , onClick (HideDone)
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
            li []
                [ input
                    [ type' "checkbox"
                    , checked task.isDone
                    , onClick (Done task.id)
                    ]
                    []
                , label [] [ text task.description ]
                ]

        onEnter fail success =
            let
                tagger code =
                    case code of
                        13 ->
                            success

                        _ ->
                            fail
            in
                on "keyup" (Json.map tagger keyCode)
    in
        div []
            [ newTaskView model.newTask
            , searchView model.search
            , hideDoneView model.hideDone
            , taskListView model.tasks model.search model.hideDone
            ]


main =
    beginnerProgram { model = emptyModel, view = view, update = update }
