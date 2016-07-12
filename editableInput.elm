module EditableInput exposing (Model, Msg, update, view, init, tests)

import Events exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import ElmTest exposing (..)
import Html.App exposing (..)


-- model


type Model
    = ReadOnly String
    | Edit String Model


init : String -> Model
init text =
    ReadOnly text


type Msg
    = BeginEdit
    | Editor EditorMsg


type EditorMsg
    = Editing String
    | Commit
    | Rollback



-- update


update msg model =
    case model of
        ReadOnly s ->
            Edit s model

        Edit s oldModel ->
            case msg of
                Editor msg ->
                    case msg of
                        Editing s ->
                            Edit s oldModel

                        Commit ->
                            ReadOnly s

                        Rollback ->
                            oldModel

                _ ->
                    Debug.crash ("invalid message: " ++ (msg |> toString))



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
            input
                [ onInput Editing
                , onEscape Rollback
                ]
                []
    in
        case model of
            ReadOnly s ->
                lazy viewReadOnly s

            Edit "" _ ->
                map Editor viewEmptyEditing

            Edit s _ ->
                map Editor (lazy viewEditing s)



-- tests


tests =
    let
        model =
            ReadOnly "old"
    in
        [ test "starts editing from current description"
            (model
                |> update BeginEdit
                |> assertEqual (Edit "old" model)
            )
        , test "rollbacks changes"
            (model
                |> update BeginEdit
                |> update (Editor (Editing "new"))
                |> update (Editor Rollback)
                |> assertEqual model
            )
        , test "commits changes"
            (model
                |> update BeginEdit
                |> update (Editor (Editing "new"))
                |> update (Editor Commit)
                |> assertEqual (ReadOnly "new")
            )
        ]
