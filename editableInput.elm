module EditableInput exposing (Model, Msg, update, view, init, tests)

import Events exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import ElmTest exposing (..)


type alias Model =
    { text : String
    , edits : Maybe String
    }


init : String -> Model
init text =
    { text = text, edits = Nothing }


type Msg
    = BeginEdit
    | Editing String
    | EndEdit
    | CancelEdit


update msg model =
    case msg of
        BeginEdit ->
            { model | edits = Just model.text }

        EndEdit ->
            case model.edits of
                Nothing ->
                    model

                Just "" ->
                    model

                Just s ->
                    { model | text = s, edits = Nothing }

        Editing s ->
            { model | edits = Just s }

        CancelEdit ->
            { model | edits = Nothing }


view model =
    case model.edits of
        Nothing ->
            label [ onDoubleClick BeginEdit ] [ text model.text ]

        Just s ->
            input
                [ value s
                , onInput Editing
                , onEnterOrEscape EndEdit CancelEdit
                ]
                []


tests =
    let
        model =
            { text = "old", edits = Nothing, isDone = False }

        assert f expected model =
            assertEqual (f model) expected
    in
        [ test "cancel rollbacks changes"
            (model
                |> update BeginEdit
                |> update (Editing "new")
                |> update CancelEdit
                |> assert .text "old"
            )
        , test "enter commits changes"
            (model
                |> update BeginEdit
                |> update (Editing "new")
                |> update EndEdit
                |> assert .text "new"
            )
        , test "empty string rollbacks changes"
            (model
                |> update BeginEdit
                |> update (Editing "")
                |> update EndEdit
                |> assert .text "old"
            )
        ]
