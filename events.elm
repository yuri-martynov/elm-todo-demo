module Events exposing (..)

import Html.Events exposing (..)
import Json.Decode exposing (..)


onEnter msg =
    onKeyUp [ ( 13, msg ) ]


onEscape msg =
    onKeyUp [ ( 27, msg ) ]


onEnterOrEscape enter escape =
    onKeyUp [ ( 13, enter ), ( 27, escape ) ]


onKeyUp options =
    let
        filter optionsToCheck code =
            case optionsToCheck of
                [] ->
                    Err "key code is not in the list"

                ( c, msg ) :: rest ->
                    if (c == code) then
                        Ok msg
                    else
                        filter rest code

        keyCodes =
            customDecoder keyCode (filter options)
    in
        on "keyup" keyCodes
