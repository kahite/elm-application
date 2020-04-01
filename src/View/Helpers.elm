module View.Helpers exposing (onEnter)

import Html exposing (Attribute)
import Html.Events exposing (on, keyCode)
import Json.Decode as Decode

onEnter: msg -> Attribute msg
onEnter msg = 
  let
      isEnter code = 
        if code == 13 then 
          Decode.succeed msg
        else
          Decode.fail "other"

  in
    on "keydown" (Decode.andThen isEnter keyCode)


