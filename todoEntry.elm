module TodoEntry exposing (Model, Msg(Enter), update, view)

import Events exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type alias Model = Maybe String


type Msg
    = Change String
    | Reset
    | Enter


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change s -> 
            Just s

        Reset ->
            Nothing

        Enter ->
            Nothing


view : Model -> Html Msg
view model =
    input
        [ placeholder "Enter new task"
        , value (model |> Maybe.withDefault "")
        , onInput Change
        , onEnterOrEscape Enter Reset 
        ]
        []
