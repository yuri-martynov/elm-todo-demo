-- port
module Todo exposing (main, tests)

import Search
import TodoEntry
import Summary
import Controls
import TodoList
import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)

import ElmTest exposing (..)

-- port setStorage : Model -> Cmd msg


type alias Model =
    { nextId : Int
    , newTask : TodoEntry.Model
    , search : Search.Model
    , hideDone : Bool
    , tasks : TodoList.Model
    }


emptyModel : Model
emptyModel =
    { nextId = 1
    , newTask = ""
    , tasks = []
    , search = ""
    , hideDone = False
    }


type Msg
    = TodoList TodoList.Msg
    | TodoEntry TodoEntry.Msg
    | Search Search.Msg
    | Controls Controls.Msg


update' : Msg -> Model -> Model
update' msg model =
    case msg of
        Controls msg ->
            model |> Controls.update msg

        TodoList msg ->
            { model | tasks = model.tasks |> TodoList.update msg }

        Search msg ->
            { model | search = model.search |> Search.update msg }

        TodoEntry TodoEntry.Enter ->
            model |> add |> (todoEntry TodoEntry.Enter)

        TodoEntry msg ->
            todoEntry msg model


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    ( update' msg model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ map TodoEntry (lazy TodoEntry.view model.newTask)
        , map Search (lazy Search.view model.search)
        , map Controls (lazy Controls.view model)
        , map TodoList (lazy2 TodoList.view model.tasks (Controls.filter model.search model.hideDone))
        , lazy Summary.view model.tasks
        ]


init =
    (emptyModel, Cmd.none )

main =
    program--WithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- helpers


add model =
    case model.newTask of
        "" ->
            model

        s ->
            { model
                | nextId = model.nextId + 1
                , tasks = (TodoList.initItem model.nextId s) :: model.tasks
            }


todoEntry msg model =
    { model | newTask = model.newTask |> TodoEntry.update msg }


-- test

tests =
    let
        model: Model
        model = 
            { emptyModel |  newTask = "new" }
    in
        [ test "Adds new todo item" (
            (model 
                |> update' (TodoEntry TodoEntry.Enter) 
                |> .tasks 
                |> List.length) 
                |> assertEqual 1
            )
        -- , test "Resets on Reset" (assertEqual ("str" |> update Reset) "")
        ]