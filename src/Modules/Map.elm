module Modules.Map exposing (Map, Message, init, view, subscriptions, update)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Time

import Model.CSSSettings exposing (CSSSettings, getBorderColor, getBackgroundColor, getSelectedBackgroundColor)

type Object = 
    Nothing
    | Cat
    | Dog

type State =
    Visited
    | On
    | Unknown

type alias Cell = {
        object: Object,
        state: State
    }

type alias Map = {
        positionInMap: Int,
        cells: List Cell 
    }

init : Map
init = Map 0 [Cell Cat Unknown, Cell Nothing Unknown, Cell Dog Unknown, Cell Nothing Unknown, Cell Nothing Unknown, Cell Nothing Unknown, Cell Cat Unknown]

type Message = 
    Timer Time.Posix

update: Message -> Map -> Map
update msg map = 
    case msg of 
        Timer _ -> 
            {map | 
                positionInMap = modBy (List.length map.cells) (map.positionInMap + 1), 
                cells = List.indexedMap (\ index cell -> 
                    if index == map.positionInMap 
                    then {cell | state = On} 
                    else if map.positionInMap == 0
                    then {cell | state = Unknown}
                    else if cell.state /= Unknown 
                    then {cell | state = Visited} 
                    else cell
                ) map.cells
            }


subscriptions : Map -> Sub Message
subscriptions _ =
    Time.every 1000 Timer

view: Map -> CSSSettings -> Html msg
view map settings = 
    div [class "Map"] [
        div [] [text "Map"],
        div [] (List.map (\ cell ->
            case cell.object of
                Nothing -> div [class (getCellBgColor cell settings ++ " " ++ getBorderColor settings)] [text "x"]
                Cat -> div [class (getCellBgColor cell settings ++ " " ++ getBorderColor settings)] [text "c"]
                Dog -> div [class (getCellBgColor cell settings ++ " " ++ getBorderColor settings)] [text "d"]
        ) map.cells)

    ]

getCellBgColor: Cell -> CSSSettings -> String
getCellBgColor cell settings = 
    case cell.state of
        Unknown -> ""
        On -> getBackgroundColor settings
        Visited -> getSelectedBackgroundColor settings
