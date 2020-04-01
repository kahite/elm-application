module View.Header exposing (view)

import Html exposing (Html, header, text)
import Html.Attributes exposing (class)

import Model.CSSSettings exposing (CSSSettings, getTextColor, getBackgroundColor)

view: CSSSettings -> String -> Html msg
view cssSettings pageTitle =
  header [ class ( getBackgroundColor cssSettings ++ " " ++ getTextColor cssSettings ++ " pv4")] [
    text pageTitle
  ]
