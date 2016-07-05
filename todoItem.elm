module TodoItem exposing (Model, Msg, update, view, tests)

import Events exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import ElmTest exposing (..)


type alias Model =
    { description : String
    , isDone : Bool
    , newDescription : Maybe String
    }


type Msg
    = Done
    | StartEditing
    | FinishEditing
    | Editing String
    | CancelEditing


update msg model =
    case msg of
        StartEditing ->
            { model | newDescription = Just model.description }

        FinishEditing ->
            case model.newDescription of
                Nothing ->
                    model

                Just "" ->
                    model

                Just s ->
                    { model | description = s, newDescription = Nothing }

        Editing s ->
            { model | newDescription = Just s }

        CancelEditing ->
            { model | newDescription = Nothing }

        Done ->
            { model | isDone = not model.isDone }


view model =
    let
        descriptionView =
            case model.newDescription of
                Nothing ->
                    label [ onDoubleClick StartEditing ] [ text model.description ]

                Just s ->
                    input
                        [ value s
                        , onInput Editing
                        , onEnterOrEscape FinishEditing CancelEditing
                        ]
                        []

        doneView =
            input
                [ type' "checkbox"
                , checked model.isDone
                , onClick Done
                ]
                []
    in
        span []
            [ doneView
            , descriptionView
            ]


tests =
    let
        model =
            { description = "old", newDescription = Nothing, isDone = False }

        editing s =
            model
                |> update StartEditing
                |> update (Editing s)

        editAndCancel =
            editing "new" |> update CancelEditing

        editAndCommit =
            editing "new" |> update FinishEditing

        editToEmptyString =
            editing "" |> update FinishEditing

        assertDescription model expectedDescription =
            assertEqual (model.description) expectedDescription
    in
        [ test "cancel rolbacks changes" (assertDescription editAndCancel "old")
        , test "enter commits changes" (assertDescription editAndCommit "new")
        , test "enter empty string rollbacks changes" (assertDescription editToEmptyString "old")
        ]
