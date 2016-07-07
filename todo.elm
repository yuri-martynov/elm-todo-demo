port module Todo exposing (..)

import TodoItem
import Search
import TodoEntry
import Summary
import Controls
import TodoList
import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)


port setStorage : Model -> Cmd msg


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
    | TodoEntryMsg TodoEntry.Msg
    | SearchMsg Search.Msg
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
                        , tasks = (TodoList.initItem model.nextId s) :: model.tasks
                    }

        todoEntry msg model =
            { model | newTask = model.newTask |> TodoEntry.update msg }
    in
        case msg of
            Controls msg ->
                model |> Controls.update msg

            TodoList msg ->
                { model | tasks = model.tasks |> TodoList.update msg }

            SearchMsg msg ->
                { model | search = model.search |> Search.update msg }

            TodoEntryMsg ((TodoEntry.Enter) as msg) ->
                model |> addNewTask |> (todoEntry msg)

            TodoEntryMsg msg ->
                todoEntry msg model


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    ( update' msg model, setStorage model )


view : Model -> Html Msg
view model =
    div []
        [ map TodoEntryMsg (lazy TodoEntry.view model.newTask)
        , map SearchMsg (lazy Search.view model.search)
        , map Controls (lazy Controls.view model)
        , map TodoList (lazy2 TodoList.view model.tasks (Controls.filter model.search model.hideDone))
        , lazy Summary.view model.tasks
        ]


init savedModel =
    ( savedModel ? emptyModel, Cmd.none )


(?) maybe default =
    Maybe.withDefault default maybe


main =
    programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
