module Model.CSSSettings exposing (..)

type Theme =
  Blue
  | Green

type alias CSSSettings = {
    whiteText : Bool,
    theme : Theme
  }
 
type Message = 
  SwitchTheme Theme
  | ToggleWhiteText

init : CSSSettings
init = CSSSettings False Blue

update : Message -> CSSSettings -> CSSSettings
update msg settings =
  case msg of
    SwitchTheme theme ->
      { settings | theme = theme }
    ToggleWhiteText ->
      { settings | whiteText = not settings.whiteText }

getTextColor : CSSSettings -> String
getTextColor settings =
  if settings.whiteText then "light-gray" else "black"

getBackgroundColor : CSSSettings -> String
getBackgroundColor settings = 
  case settings.theme of
     Green -> "bg-dark-green"
     Blue -> "bg-dark-blue"

getBorderColor : CSSSettings -> String
getBorderColor settings = 
  case settings.theme of
     Green -> "b--dark-green"
     Blue -> "b--dark-blue"

getSelectedBackgroundColor : CSSSettings -> String
getSelectedBackgroundColor settings = 
  case settings.theme of
     Green -> "bg-washed-green"
     Blue -> "bg-washed-blue"
