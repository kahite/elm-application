module Modules.TodoApp exposing (Model, Message, init, update, view)

import Html exposing (Html, text, div, input, a)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput, onClick)

import Model.CSSSettings exposing (CSSSettings, getBorderColor)

import View.Helpers exposing (onEnter)


type alias Element = {
    text : String,
    visible : Bool
  }

type alias Model = {
    currentText : String,
    elements : List Element
  }  

type Message
  = Store String
  | AddElement
  | DeleteElement String 
  | ToggleVisibleElement String
  | ToggleAllElements

init : Model
init = Model "" []

update : Message -> Model -> Model
update msg todoApp =
  case msg of
    Store value ->
        {todoApp | currentText = value}
    AddElement ->
      if todoApp.currentText == "" then 
        todoApp
      else 
        {todoApp | currentText = "", elements = List.append todoApp.elements [Element todoApp.currentText True]}
    DeleteElement value ->
      {todoApp | elements = List.filter (\elem -> elem.text /= value) todoApp.elements}
    ToggleVisibleElement value ->
      {todoApp | elements = List.map (\elem -> 
        if elem.text == value && elem.visible
        then {elem | visible = False} 
        else if elem.text == value
        then {elem | visible = True} 
        else elem) todoApp.elements}
    ToggleAllElements -> 
      {todoApp | elements = List.map (\elem -> {elem | visible = False}) todoApp.elements}


view: Model -> CSSSettings -> Html Message
view  todoApp cssSettings = 
  div [ class "TodoApp"] [
    div [ class "Header"] [text "Header"],
    div [ class "Action pa4" ] [
      displayStyledButton "Toggle" ToggleAllElements cssSettings,
      input [onInput Store, onEnter AddElement, value todoApp.currentText] [],
      displayStyledButton "Add" AddElement cssSettings
    ],
    div [ class "List pa4" ] 
      (List.map (\ elem ->
        div [class (if elem.visible then "Element" else "Element o-20")] [
          displayStyledButton "o" (ToggleVisibleElement elem.text) cssSettings,
          div [] [text elem.text],
          displayStyledButton "x" (DeleteElement elem.text) cssSettings
        ]
      ) todoApp.elements)
  ]

displayStyledButton: String -> message -> CSSSettings -> Html message
displayStyledButton value msg cssSettings =
  a [onClick msg, class ("f6 grow no-underline br-pill ba ph3 pv2 mb2 dib shadow-hover " ++ getBorderColor cssSettings)] [text value]