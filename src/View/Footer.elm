module View.Footer exposing (view)

import Html exposing (Html, footer, div, label, input, text)
import Html.Attributes exposing (type_, name, class)
import Html.Events exposing (onClick)

import Model.CSSSettings exposing (Theme(..), CSSSettings, Message(..), getTextColor, getBackgroundColor)

view: CSSSettings -> Html Message
view cssSettings =
  footer [class ( getBackgroundColor cssSettings ++ " " ++ getTextColor cssSettings ++ " pv4 fixed bottom-0 left-0 right-0 flex justify-around")] [
    div [] [
      div [] [text "Theme"],
      displayFooterRadio "Blue" (SwitchTheme Blue) (cssSettings.theme == Blue),
      displayFooterRadio "Green" (SwitchTheme Green) (cssSettings.theme == Green)
    ],
    div [] [
      div [] [text "Options"],
      label [] [
        input [type_ "checkbox", name "white-text", Html.Attributes.checked cssSettings.whiteText, onClick ToggleWhiteText] [],
        text "White Text" 
      ]
    ]
  ]

displayFooterRadio: String -> Message -> Bool -> Html Message
displayFooterRadio labelString msg checked =
  label [] [
    input [type_ "radio", name "background-color", onClick msg, Html.Attributes.checked checked] [],
    text labelString
  ]