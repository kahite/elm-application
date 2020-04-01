module Modules.Counters exposing (Model, Message(..), init, subscriptions, update, view)


import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Time

import Model.CSSSettings exposing (CSSSettings, getBorderColor, getBackgroundColor, getSelectedBackgroundColor)

type alias Counter = {
    name: String,
    value: Int
  }

type alias Model = {
    counters: List Counter,
    selected: String
  }

init : Model
init = Model [Counter "food" 0, Counter "wood" 0, Counter "metal" 0] "food"

subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every 100 Timer

type Message = 
    SwitchCounter String
    | Timer Time.Posix

update : Message -> Model -> Model
update msg model = 
    case msg of
      SwitchCounter elem ->
        { model | selected = elem }
      Timer _ ->
        { model | counters = updateCounters model.counters model.selected 1 }


updateCounters : List Counter -> String -> Int -> List Counter
updateCounters counters elem value = 
    List.map (\counter -> if counter.name == elem then {counter | value = counter.value + value} else counter) counters

view : Model -> CSSSettings -> Html Message
view model cssSettings = 
    div [] (
        List.append 
            [text "Counters"]
            (List.map (\
                counter ->
                div [onClick (SwitchCounter counter.name), class (getSelBgColor model counter cssSettings ++ " " ++ getBorderColor cssSettings ++ " ba bw2 pa3 ma3") ][
                    span [] [
                        text counter.name
                    ],
                    span [] [
                        text (String.fromInt counter.value)
                    ]
                ]) 
                model.counters
            )
        )
    
getSelBgColor: Model -> Counter -> CSSSettings -> String
getSelBgColor model counter cssSettings =
    if model.selected == counter.name then getSelectedBackgroundColor cssSettings else getBackgroundColor cssSettings
