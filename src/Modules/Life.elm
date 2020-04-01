module Modules.Life exposing (Model, Message, initConfined, initWrapped, view, getCmd, subscriptions, update)

import Html exposing (Html, div, table, tr, td, text)
import Html.Attributes exposing (class, width, height)
import Random
import Time

import Model.CSSSettings exposing (CSSSettings, getBackgroundColor, getSelectedBackgroundColor)

type alias Cell = {
        active: Bool,
        aliveNeighbors: Int
    }

cellGen: Random.Generator Cell
cellGen = 
    Random.map2 Cell (Random.map (\n -> n == 0) (Random.int 0 1)) (Random.constant 0)

type alias Row = List Cell

rowGen: Random.Generator Row
rowGen = 
    Random.list 20 cellGen

type alias Model = {
        rows: List Row,
        confined: Bool
    }

modelGen: Random.Generator (List Row)
modelGen = 
    Random.list 20 rowGen


type Message = 
    NewModel (List Row)
    | Timer Time.Posix

initConfined: Model
initConfined = Model [] True

initWrapped: Model
initWrapped = Model [] False

getCmd: Cmd Message
getCmd = 
    Random.generate NewModel modelGen


subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every 750 Timer


update: Message -> Model -> (Model, Cmd Message)
update msg model = 
    case msg of
        NewModel rows ->
            ({model | rows = rows}, Cmd.none)
        Timer _ ->
            (updateModel (computeModelNextTurn model), Cmd.none)

view: Model -> CSSSettings -> Html msg
view model settings =
    div [class "Life"] [
        div [][text ("Life " ++ if model.confined then "Confined" else "Wrapped")],
        table [] (
            List.map (\ row -> 
                tr [] (
                    List.map (\ cell ->
                        td [class (getClass settings cell), width 10, height 10] [text " "]
                    ) row
                )
            ) model.rows
        )
    ]

getClass: CSSSettings -> Cell -> String
getClass settings cell =
    if cell.active
    then getBackgroundColor settings
    else getSelectedBackgroundColor settings



computeCellNextTurn: Cell -> Cell -> Cell -> Cell -> Cell -> Cell -> Cell -> Cell -> Cell -> Cell
computeCellNextTurn cell n ne e se s sw w nw = 
    {cell | aliveNeighbors = 
        (if n.active then 1 else 0)
        + (if ne.active then 1 else 0)
        + (if e.active then 1 else 0)
        + (if se.active then 1 else 0)
        + (if s.active then 1 else 0)
        + (if sw.active then 1 else 0)
        + (if w.active then 1 else 0)
        + (if nw.active then 1 else 0)
    }

updateCell: Cell -> Cell
updateCell cell = 
    {cell | active = 
        cell.aliveNeighbors == 3
        || cell.active && cell.aliveNeighbors == 2
    }

updateModel: Model -> Model
updateModel model = 
    {model | rows = 
        List.map (\ row ->
            List.map (\ cell ->
                updateCell cell
            ) row
        ) model.rows
    }


computeRowNextTurn: Row -> Row -> Row -> Bool -> Row
computeRowNextTurn row up down confined = 
    List.indexedMap (\ index cell ->
        computeCellNextTurn 
            cell 
            (getCell index up confined) 
            (getCell (index+1) up confined) 
            (getCell (index+1) row confined) 
            (getCell (index+1) down confined) 
            (getCell index down confined) 
            (getCell (index-1) down confined) 
            (getCell (index-1) row confined) 
            (getCell (index-1) up confined) 
    ) row

getCell: Int -> Row -> Bool -> Cell
getCell index row confined = 
    if confined && (index < 0 || index >= 20)
    then inactiveCell
    else 
        let cellIndex = modBy 20 index
        in
            case List.head (List.drop cellIndex row) of
            Nothing -> Cell True 2
            Just cell -> cell

computeModelNextTurn: Model -> Model
computeModelNextTurn model = 
    {model | rows =
        List.indexedMap (\ index row ->
            computeRowNextTurn 
                row 
                (getRow (index-1) model)
                (getRow (index+1) model)
                model.confined
        ) model.rows
    }
 
getRow: Int -> Model -> Row
getRow index model = 
    if model.confined && (index < 0 || index >= 20)
    then inactiveRow
    else 
        let rowIndex = modBy 20 index
        in
            case List.head (List.drop rowIndex model.rows) of
                Nothing -> []
                Just row -> row

inactiveCell: Cell
inactiveCell = Cell False 0

inactiveRow: Row
inactiveRow = List.repeat 20 inactiveCell