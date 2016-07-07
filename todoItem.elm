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

        assertDescription expectedDescription model =
            assertEqual (model.description) expectedDescription
    in
        [ test "cancel rollbacks changes"
            (model
                |> update StartEditing
                |> update (Editing "new")
                |> update CancelEditing
                |> assertDescription "old"
            )
        , test "enter commits changes"
            (model
                |> update StartEditing
                |> update (Editing "new")
                |> update FinishEditing
                |> assertDescription "new"
            )
        , test "empty string rollbacks changes"
            (model
                |> update StartEditing
                |> update (Editing "")
                |> update FinishEditing
                |> assertDescription "old"
            )
        ]
