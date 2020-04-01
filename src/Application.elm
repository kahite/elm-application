module Application exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class)

import Model.CSSSettings
import View.Footer
import View.Header
import Modules.Counters
import Modules.Life
import Modules.Map
import Modules.TodoApp
import Modules.TurnBased


type alias Model = {
    counters : Modules.Counters.Model,
    todoApp : Modules.TodoApp.Model,
    cssSettings : Model.CSSSettings.CSSSettings,
    map : Modules.Map.Map,
    turnBased : Modules.TurnBased.Model,
    lifeConfined: Modules.Life.Model,
    lifeWrapped: Modules.Life.Model
  }

type Message
  = TodoAppMessage Modules.TodoApp.Message
  | CSSSettingsMessage Model.CSSSettings.Message
  | CountersMessage Modules.Counters.Message
  | MapMessage Modules.Map.Message
  | TurnBasedMessage Modules.TurnBased.Message
  | LifeMessageConfined Modules.Life.Message
  | LifeMessageWrapped Modules.Life.Message

main : Program () Model Message
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : () -> (Model, Cmd Message)
init _ = (Model Modules.Counters.init Modules.TodoApp.init Model.CSSSettings.init Modules.Map.init Modules.TurnBased.init Modules.Life.initConfined Modules.Life.initWrapped, 
    Cmd.batch [
      Cmd.map (\ ms -> TurnBasedMessage ms) Modules.TurnBased.getCmd,
      Cmd.map (\ ms -> LifeMessageConfined ms) Modules.Life.getCmd,
      Cmd.map (\ ms -> LifeMessageWrapped ms) Modules.Life.getCmd
    ]
  )

 
update : Message -> Model -> (Model, Cmd Message)
update msg model = 
  case msg of 
    TodoAppMessage todoAppMessage ->
      ({model | todoApp = Modules.TodoApp.update todoAppMessage model.todoApp}, Cmd.none)
    CSSSettingsMessage settingsMessage -> 
      ({model | cssSettings = Model.CSSSettings.update settingsMessage model.cssSettings}, Cmd.none)
    CountersMessage countersMessage ->
      ({ model | counters = Modules.Counters.update countersMessage model.counters}, Cmd.none)
    MapMessage mapMsg ->
      ({model | map = Modules.Map.update mapMsg model.map}, Cmd.none)
    TurnBasedMessage turnBasedMessage ->
      let
          (m, c) = Modules.TurnBased.update turnBasedMessage model.turnBased
      in
        ({model | turnBased = m}, Cmd.map (\ ms -> TurnBasedMessage ms) c)
    LifeMessageConfined lifeMessage ->
      let
          (m, c) = Modules.Life.update lifeMessage model.lifeConfined
      in
        ({model | lifeConfined = m}, Cmd.map (\ ms -> LifeMessageConfined ms) c)
    LifeMessageWrapped lifeMessage ->
      let
          (m, c) = Modules.Life.update lifeMessage model.lifeWrapped
      in
        ({model | lifeWrapped = m}, Cmd.map (\ ms -> LifeMessageWrapped ms) c)


subscriptions : Model -> Sub Message
subscriptions model = 
  Sub.batch [
    Sub.map (\msg -> CountersMessage msg) (Modules.Counters.subscriptions model.counters),
    Sub.map (\msg -> MapMessage msg) (Modules.Map.subscriptions model.map),
    Sub.map (\msg -> LifeMessageConfined msg) (Modules.Life.subscriptions model.lifeConfined),
    Sub.map (\msg -> LifeMessageWrapped msg) (Modules.Life.subscriptions model.lifeWrapped)
  ]


view : Model -> Html Message
view model =
  div [ class "tc f4 avenir bg-moon-gray" ] [
    View.Header.view model.cssSettings "Elm application",
    div [class ("Application flex justify-around " ++ Model.CSSSettings.getTextColor model.cssSettings)] [
      Html.map (\msg -> TodoAppMessage msg) (Modules.TodoApp.view model.todoApp model.cssSettings),
      Html.map (\msg -> CountersMessage msg) (Modules.Counters.view model.counters model.cssSettings),
      Modules.Map.view model.map model.cssSettings,
      Modules.TurnBased.view model.turnBased model.cssSettings,
      Modules.Life.view model.lifeConfined model.cssSettings,
      Modules.Life.view model.lifeWrapped model.cssSettings
    ],
    Html.map (\msg -> CSSSettingsMessage msg) (View.Footer.view model.cssSettings)
  ]

 
