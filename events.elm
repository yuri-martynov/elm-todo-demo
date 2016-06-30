module Events exposing (..)

import Html.Events exposing (..)
import Json.Decode exposing (..)


onKeyUp options =
    let
        codes = options |> List.map fst
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


onEnter msg =
    onKeyUp [ ( 13, msg ) ]


onEnterOrEscape enter escape =
    onKeyUp [ ( 13, enter ), ( 27, escape ) ]

keyCodes codes = 
    let
        f codesToCheck code =
            case codesToCheck of
                [] -> Err "key code not found"
                c :: rest ->
                    if (c == code) then Ok code
                    else f rest code
    in
        customDecoder keyCode (f codes)
