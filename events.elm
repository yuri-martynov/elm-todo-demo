module Events exposing (..)

import Html.Events exposing (..)
import Json.Decode as Json


onKeyUp fail options =
    let
        tagger options code =
            case options of
                [] ->
                    fail

                ( c, msg ) :: rest ->
                    if (c == code) then
                        msg
                    else
                        tagger rest code
    in
        on "keyup" (Json.map (tagger options) keyCode)


onEnter fail msg =
    onKeyUp fail [ ( 13, msg ) ]


onEnterOrEscape fail enter escape =
    onKeyUp fail [ ( 13, enter ), ( 27, escape ) ]
