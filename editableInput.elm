module EditableInput exposing (Model, Msg, update, view, init, tests)

import Events exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import ElmTest exposing (..)


-- model


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
    | Commit
    | Rollback



-- update


update msg model =
    case msg of
        BeginEdit ->
            { model | edits = Just model.text }

        Editing value ->
            { model | edits = Just value }

        Commit ->
            case model.edits of
                Just "" ->
                    model

                Just s ->
                    { model | text = s, edits = Nothing }

                _ ->
                    Debug.crash ("can not commit value")

        Rollback ->
            { model | edits = Nothing }



-- view


view model =
    let
        viewReadOnly s =
            label [ onDoubleClick BeginEdit ] [ text s ]

        viewEditing s =
            input
                [ value s
                , onInput Editing
                , onEnterOrEscape Commit Rollback
                ]
                []

        viewEmptyEditing =
            input [ onInput Editing ] []
    in
        case model.edits of
            Nothing ->
                lazy viewReadOnly model.text

            Just "" ->
                viewEmptyEditing

            Just s ->
                lazy viewEditing s

-- tests


tests =
    let
        model =
            { text = "old", edits = Nothing, isDone = False }

        assert f expected model =
            assertEqual (f model) expected
    in
        [ test "starts editing from current description"
            (model
                |> update BeginEdit
                |> assert .edits (Just "old")
            )
        , test "rollbacks changes"
            (model
                |> update BeginEdit
                |> update (Editing "new")
                |> update Rollback
                |> assert .text "old"
            )
        , test "commits changes"
            (model
                |> update BeginEdit
                |> update (Editing "new")
                |> update Commit
                |> assert .text "new"
            )
        , test "ignores empty edits"
            (model
                |> update BeginEdit
                |> update (Editing "")
                |> update Commit
                |> assert .text "old"
            )
        ]
