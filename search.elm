module Search exposing (Model, Msg, update, view)

import Events exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    String


type Msg
    = Change String
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change s ->
            s

        Reset ->
            ""


view : Model -> Html Msg
view model =
    input
        [ placeholder "Enter text to search"
        , value model
        , onInput Change
        , onEscape Reset
        ]
        []
