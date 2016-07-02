module TodoItem exposing (Model, Msg, update, view)

import Events exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

type alias Model =
    { description : String
    , isDone : Bool
    , newDescription : Maybe String
    }


type Msg
    = Done
    | StartEditing
    | FinishEditing
    | TaskChanged String
    | CancelEditing


update : Msg -> Model -> Model
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

        TaskChanged s ->
            { model | newDescription = Just s }

        CancelEditing ->
            { model | newDescription = Nothing }

        Done ->
            { model | isDone = not model.isDone }

view: Model -> Html Msg
view model =
    let
        descriptionView =
            case model.newDescription of
                Nothing ->
                    label [ onDoubleClick StartEditing ] [ text model.description ]

                Just s ->
                    input
                        [ value s
                        , onInput TaskChanged
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
