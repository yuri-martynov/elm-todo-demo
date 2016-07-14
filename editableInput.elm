module EditableInput exposing (Model, Msg, update, view, init, valueOf, tests)

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
    | Edit String String


type Msg
    = BeginEdit
    | Editor EditorMsg


type EditorMsg
    = Editing String
    | Commit
    | Rollback



-- update


update : Msg -> Model -> Model
update msg model =
    case model of
        ReadOnly s ->
            Edit s s

        Edit s old ->
            case msg of
                Editor msg ->
                    case msg of
                        Editing s ->
                            Edit s old

                        Commit ->
                            ReadOnly s

                        Rollback ->
                            ReadOnly old

                _ ->
                    Debug.crash ("invalid message: " ++ (msg |> toString))



-- view


view : Model -> Html Msg
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



-- model functions


init : String -> Model
init text =
    ReadOnly text


valueOf : Model -> String
valueOf model =
    case model of
        ReadOnly s ->
            s

        Edit _ old ->
            old

-- tests


tests : List Test
tests =
    let
        model =
            ReadOnly "old"
    in
        [ test "starts editing from current value"
            (model
                |> update BeginEdit
                |> assertEqual (Edit "old" "old")
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
