module Modules.TurnBased exposing (Model, Message(..), init, update, view, getCmd)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Process
import Task

import Model.CSSSettings exposing (CSSSettings, getBorderColor, getBackgroundColor, getSelectedBackgroundColor)

type alias Player = {
        name: String,
        total: Int
    }

type alias Model = {
        player1: Player,
        player2: Player,
        currentTurn: Int
    }

init: Model
init = Model (Player "Toto" 10) (Player "Titi" 20) 0

type Message = 
    StartTurnPlayer1
    | StartTurnPlayer2
    | EndTurnPlayer1
    | EndTurnPlayer2
    | Sleep Message

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        StartTurnPlayer1 ->
            ({model | currentTurn = 0}, Task.perform (\_ -> Sleep EndTurnPlayer1)  (Task.succeed 1) )
        StartTurnPlayer2 ->
            ({model | currentTurn = 1}, Task.perform (\_ -> Sleep EndTurnPlayer2)  (Task.succeed 1) )
        EndTurnPlayer1 ->
            ({model | currentTurn = 2}, Task.perform (\_ -> Sleep StartTurnPlayer2)  (Task.succeed 1))
        EndTurnPlayer2 ->
            ({model | currentTurn = 2}, Task.perform (\_ -> Sleep StartTurnPlayer1)  (Task.succeed 1))
        Sleep msg2 ->
            (model, Task.perform (\_ -> msg2)  (Task.sequence [Process.sleep 500, Task.succeed ()]))

getCmd: Cmd Message
getCmd =
    Task.perform (\_ -> Sleep StartTurnPlayer1)  (Task.succeed 1) 

view: Model -> CSSSettings -> Html msg
view model settings =
    div [class "TurnBased"] [
        div [][text "Turn Based"],
        div [class "flex justify-content"] [
            div [class (getClass settings (model.currentTurn == 0))] [text model.player1.name],
            div [class (getClass settings (model.currentTurn == 1))] [text model.player2.name]
        ]
    ]

getClass: CSSSettings -> Bool -> String
getClass settings selected =
    if selected
    then getBorderColor settings ++ " " ++ getBackgroundColor settings ++ " pa3 ba bw-2"
    else getBorderColor settings ++ " " ++ getSelectedBackgroundColor settings ++ " pa3 ba bw-2"

