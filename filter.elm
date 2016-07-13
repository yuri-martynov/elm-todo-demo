module Filter exposing (Model, Msg, update, view, init, filter)

import Events exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import TodoItem exposing (Model, descriptionOf)
import String


type alias Model =
    { search : String
    , hideDone : Bool
    }


type Msg
    = UpdateSearch String
    | ResetSearch
    | HideDone Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSearch s ->
            { model | search = s }

        ResetSearch ->
            { model | search = "" }

        HideDone b ->
            { model | hideDone = b }


view : Model -> Bool -> Html Msg
view model hasDone =
    let
        hideDoneView hasDone hideDone =
            if hasDone then
                [ div []
                    [ input
                        [ type' "checkbox"
                        , checked hideDone
                        , onClick (HideDone (not hideDone))
                        ]
                        []
                    , text "hide done"
                    ]
                ]
            else
                []

        searchView' attrs =
            input ([ onInput UpdateSearch ] ++ attrs) []

        emptySearchView =
            searchView' [ placeholder "Enter text to search" ]

        searchView s =
            case s of
                "" ->
                    emptySearchView

                _ ->
                    searchView' [ value s, onEscape ResetSearch ]
    in
        div []
            ([ searchView model.search ] ++ (hideDoneView hasDone model.hideDone))


init : Model
init =
    { search = "", hideDone = False }


filter : Model -> (TodoItem.Model -> Bool)
filter model =
    let
        byStatus =
            if model.hideDone then
                .isDone >> not
            else
                \_ -> True

        byDescription =
            if model.search == "" then
                \_ -> True
            else
                TodoItem.descriptionOf >> String.contains model.search
    in
        \t -> byStatus t && byDescription t
