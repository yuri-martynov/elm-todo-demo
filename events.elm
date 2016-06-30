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
        codes =
            options |> List.map fst

        tagger options code =
            case options of
                [] ->
                    Debug.crash "key code should exists"

                ( c, msg ) :: rest ->
                    if (c == code) then
                        msg
                    else
                        tagger rest code
    in
        on "keyup" (map (tagger options) (keyCodes codes))


keyCodes codes =
    let
        filterCodes codesToCheck code =
            case codesToCheck of
                [] ->
                    Err "key code is not in the list"

                c :: rest ->
                    if (c == code) then
                        Ok code
                    else
                        filterCodes rest code
    in
        customDecoder keyCode (filterCodes codes)
