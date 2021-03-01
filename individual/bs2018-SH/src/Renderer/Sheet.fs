﻿module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type SelectionBox = {
    FixedCorner: XYPos
    MovingCorner: XYPos
    Show: bool
}

type Grid = {
    Size: float
    SnapToGrid: bool
    Show: bool
}

type MouseDown =
    | MouseIsUp
    | FromPort of CommonTypes.PortId * XYPos
    | FromSymbol
    | FromEmpty
    | FromWire of CommonTypes.ConnectionId


type Model = {
    Wire: BusWire.Model
    SelectedSymbols: CommonTypes.ComponentId list
    SelectedWire: CommonTypes.ConnectionId Option
    MouseState: MouseDown
    LastMousePos: XYPos
    SelectionBox: SelectionBox
    Grid: Grid
    ScrollOffset: XYPos
    Zoom: float
}

type KeyboardMsg =
    | AltShiftZ
    | DEL
    | CtrlA
    | CtrlN
    | CtrlG
    | CtrlEquals
    | CtrlMinus
    | CtrlQ
    | CtrlW

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseDown of XYPos * bool
    | MouseMove of XYPos
    | MouseUp of XYPos * bool
    | Symbol of Symbol.Msg
    | Scroll of float * float


/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    let selectionBox = model.SelectionBox
    let grid = model.Grid
    let size = 1000.0
    let sizeInPixels = sprintf "%.2fpx" ((size * model.Zoom))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false

    let bodyLst = [wireSvg]

    let bodyLst = if selectionBox.Show then
                    List.append bodyLst [ 
                                polygon [
                                    SVGAttr.Points (polygonPointsString selectionBox.FixedCorner selectionBox.MovingCorner)
                                    SVGAttr.Fill "LightBlue"
                                    SVGAttr.Stroke "Blue"
                                    SVGAttr.FillOpacity 0.5
                                    SVGAttr.StrokeWidth 1 ] []
                            ]
                  else bodyLst
    
    let bodyLst =
        if grid.Show then
            [0 .. int (size / grid.Size)]
            |> List.collect (fun i ->
                [
                    line [
                        X1 0.
                        X2 size
                        Y1 ((float i)*grid.Size)
                        Y2 ((float i)*grid.Size)
                        SVGAttr.Stroke "grey"
                        SVGAttr.StrokeWidth 1
                        SVGAttr.StrokeOpacity 0.3
                    ] []

                    line [
                        X1 ((float i)*grid.Size)
                        X2 ((float i)*grid.Size)
                        Y1 0.
                        Y2 size
                        SVGAttr.Stroke "grey"
                        SVGAttr.StrokeWidth 1
                        SVGAttr.StrokeOpacity 0.3
                    ] []
                
                ]
            )
            |> List.append bodyLst
        else
            bodyLst

    let bodyLst =
        match model.MouseState with
        | FromPort (pId, mousePos) ->
            bodyLst
            |> List.append [
                let portPos = Symbol.portPos model.Wire.Symbol pId
                line [
                        X1 portPos.X
                        X2 mousePos.X
                        Y1 portPos.Y
                        Y2 mousePos.Y
                        SVGAttr.Stroke "green"
                        SVGAttr.StrokeWidth 2
                        SVGAttr.StrokeOpacity 1
                        SVGAttr.StrokeDasharray "5, 3"
                    ] []
            ]
        | _ -> bodyLst

    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mousePos x y =
        posOf ((x+model.ScrollOffset.X)/model.Zoom) ((y+model.ScrollOffset.Y)/model.Zoom)
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "90%"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ]
          Id "sheetDiv"

          OnScroll (fun _ ->
            let sDiv = document.getElementById "sheetDiv"
            Scroll (sDiv.scrollLeft, sDiv.scrollTop)
            |> dispatch
          )

          OnMouseDown (fun ev -> 
            MouseDown(mousePos ev.pageX ev.pageY, ev.shiftKey)
            |> dispatch
          )

          OnMouseMove (fun ev -> 
            MouseMove(mousePos ev.pageX ev.pageY)
            |> dispatch
          )
          
          OnMouseUp (fun ev -> 
            MouseUp(mousePos ev.pageX ev.pageY, ev.shiftKey)
            |> dispatch
          )

          OnMouseLeave (fun ev ->
            MouseUp(mousePos ev.pageX ev.pageY, ev.shiftKey)
            |> dispatch
          )
        ]
        [ svg
            [ Style 
                [
                    Border "3px solid green"
                    Height sizeInPixels
                    Width sizeInPixels           
                ]
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" model.Zoom)]] // top-level transform style attribute for zoom
                bodyLst
            ]
        ]


let posToGridIfEnabled (grid : Grid) (pos : XYPos) : XYPos =
    if grid.SnapToGrid then
        let leftDist = pos.X % grid.Size
        let upDist = pos.Y % grid.Size
        let x = if leftDist < grid.Size - leftDist then pos.X - leftDist else pos.X + grid.Size - leftDist
        let y = if upDist < grid.Size - upDist then pos.Y - upDist else pos.Y + grid.Size - upDist
        posOf x y
    else
        pos


let highlightPorts (model : Model) (pos : XYPos) (fromPid : CommonTypes.PortId) = 
    let fromPortType = Symbol.portType model.Wire.Symbol fromPid    
    let portsInRange =
        Symbol.portsInRange model.Wire.Symbol pos 100.
        |> List.filter (fun pId -> (Symbol.portType model.Wire.Symbol pId) <> fromPortType) //ensure port types are opposite
    let ports = List.append portsInRange [fromPid]

    [Cmd.ofMsg (Symbol <| Symbol.HighlightPort ports)]
    |> List.append [Cmd.ofMsg (Symbol <| Symbol.UnhighlightPorts)]

let alignSymbolsToGrid (model : Model) =
    Symbol.getAllSymbols model.Wire.Symbol
    |> List.map (fun el -> (Symbol.symbolBBox model.Wire.Symbol el), el)
    |> List.map (fun (bb,id) -> posToGridIfEnabled model.Grid bb.Pos, bb.Pos, id)
    |> List.map (fun (dst,src, id) -> (posDiff dst src), id)
    |> List.map (fun (movement, id) ->
        [
            Cmd.ofMsg (Symbol <| Symbol.StartDragging ([id], posOf 0. 0.))
            Cmd.ofMsg (Symbol <| Symbol.Dragging ([id], movement))
        ]
    )
    |> List.collect id
    |> Cmd.batch


let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Scroll (x, y) ->
        {model with ScrollOffset = posOf x y}
        , Cmd.none

    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd

    | Symbol sMsg ->
        let wModel, wCmd = BusWire.update (BusWire.Symbol sMsg) model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd

    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged

    | KeyPress CtrlA ->
        let idLst = Symbol.getAllSymbols model.Wire.Symbol
        {model with SelectedSymbols = idLst},
        Cmd.ofMsg (Symbol <| Symbol.SetSelected idLst)

    | KeyPress DEL ->
        let model, wireCommands = 
            match model.SelectedWire with
            | None -> model, [Cmd.none]
            | Some wId -> {model with SelectedWire = None}, [Cmd.ofMsg (Wire <| BusWire.DeleteWire wId)]

        let symbolCommands = [Cmd.ofMsg (Symbol <| Symbol.DeleteSymbols model.SelectedSymbols)]
        model, Cmd.batch (List.append wireCommands symbolCommands)

    | KeyPress CtrlN ->
        let posOnGrid = posToGridIfEnabled model.Grid model.LastMousePos
        model, Cmd.ofMsg (Symbol <| Symbol.AddSymbol (CommonTypes.Not, posOnGrid))
    
    | KeyPress CtrlG ->
        {model with
            Grid = {model.Grid with SnapToGrid = (not model.Grid.SnapToGrid)}
        }, Cmd.none

    | KeyPress CtrlEquals ->
        {model with Zoom = model.Zoom + 0.1},Cmd.none

    | KeyPress CtrlMinus ->
        if model.Zoom > 0.1 then
            {model with Zoom = model.Zoom - 0.1},Cmd.none
        else
            model,Cmd.none

    | KeyPress CtrlQ ->
        let newGridSize = if model.Grid.Size > 2. then model.Grid.Size - 2. else model.Grid.Size
        let newModel =
            {
                model with
                    Grid = {model.Grid with Size = newGridSize}
            }
        newModel, alignSymbolsToGrid newModel

     | KeyPress CtrlW ->
        let newModel =
            {
                model with
                    Grid = {model.Grid with Size = model.Grid.Size + 2.}
            }
        newModel, alignSymbolsToGrid newModel
            
    | MouseDown (pos, isShift) ->
        let processSelectedSymbols targetedId =
            if List.contains targetedId model.SelectedSymbols then
                if isShift then
                    List.filter (fun el -> el <> targetedId) model.SelectedSymbols
                else
                    model.SelectedSymbols
            else
                if isShift then List.append model.SelectedSymbols [targetedId]
                else [targetedId]

        let mouseState, selectedSymbols, showSelection =
            let targetedPort = Symbol.getTargetedPort model.Wire.Symbol pos
            match targetedPort with
            | Some portId -> FromPort (portId, pos), model.SelectedSymbols, false
            | None ->
                let targetedSymbol = Symbol.getTargetedSymbol model.Wire.Symbol pos
                match targetedSymbol with
                | Some symbolId -> FromSymbol, processSelectedSymbols symbolId, false
                | None -> 
                    match BusWire.getTargetedWire model.Wire pos with
                        | Some wId -> FromWire wId, model.SelectedSymbols, false
                        | None -> FromEmpty, [], true

        let wireDragCommands =
            match mouseState with
            | FromWire wId -> [Cmd.ofMsg (Wire <| BusWire.StartDrag (wId, pos))]
            | _ -> []        
        
        {model with
            MouseState = mouseState;
            SelectedSymbols = selectedSymbols;
            SelectionBox = {
                model.SelectionBox with
                    FixedCorner = pos
                    MovingCorner = pos
                    Show = showSelection
            }
            LastMousePos = pos
        },
        Cmd.batch(
            [
                Cmd.ofMsg (Symbol <| Symbol.StartDragging (selectedSymbols, (posToGridIfEnabled model.Grid pos)));
                Cmd.ofMsg (Symbol <| Symbol.SetSelected selectedSymbols);
            ]
            |> List.append wireDragCommands
        )
        
    | MouseUp (pos, isShift) ->
        let processSelectionBox =
            let symbolsInSelectionBox =
                let c1 = model.SelectionBox.FixedCorner
                let c2 = pos
                let selectionBBox = bboxFromDiagonals c1 c2
                Symbol.getSymbolsInTargetArea model.Wire.Symbol selectionBBox
            if List.length symbolsInSelectionBox > 1 then symbolsInSelectionBox
            else model.SelectedSymbols

        let selectedSymbols =
            match model.MouseState with
            | FromEmpty -> processSelectionBox
            | _ -> model.SelectedSymbols

        let selectWireCommands, selectedWire =
            if selectedSymbols = model.SelectedSymbols then
                match BusWire.getTargetedWire model.Wire pos with
                | None -> [], None
                | Some wId -> [Cmd.ofMsg (Wire <| BusWire.SetSelected wId)], Some wId
            else
                [], None

        let selectWireCommands = 
            selectWireCommands
            |> List.append [Cmd.ofMsg (Wire <| BusWire.UnselectAll)]

        let addWireCommands =
            match model.MouseState with
            | FromPort (fromPid, toPos) ->
                match Symbol.getTargetedPort model.Wire.Symbol toPos with
                | Some toPid ->
                    let fromPortType = Symbol.portType model.Wire.Symbol fromPid
                    let toPortType = Symbol.portType model.Wire.Symbol toPid
                    if fromPortType <> toPortType then
                        [Cmd.ofMsg (Wire <| BusWire.AddWire (fromPid, toPid))]
                    else []
                | None -> []
            | _ -> []

        let cmds = 
            [
                Cmd.ofMsg (Symbol <| Symbol.EndDragging)
                Cmd.ofMsg (Symbol <| Symbol.SetSelected selectedSymbols)
                Cmd.ofMsg (Wire <| BusWire.EndDrag)
                Cmd.ofMsg (Symbol <| Symbol.UnhighlightPorts)
            ]
            |> List.append addWireCommands
            |> List.append selectWireCommands

        {
            model with
                SelectedSymbols = selectedSymbols
                SelectedWire = selectedWire
                MouseState = MouseIsUp
                SelectionBox = {
                    model.SelectionBox with
                        Show = false
                }
        }, 
        Cmd.batch cmds


    | MouseMove pos ->
        let model, draggingCommands =
            match model.MouseState with
            | FromEmpty ->
                {
                    model with
                        SelectionBox = {
                            model.SelectionBox with
                                MovingCorner = pos
                        }
                },
                [Cmd.none]
            | FromSymbol ->
                model, [Cmd.ofMsg (Symbol <| Symbol.Dragging (model.SelectedSymbols, (posToGridIfEnabled model.Grid pos)))]
            | FromPort (pId,_) ->
                {
                    model with
                        MouseState = FromPort (pId, pos)
                },
                highlightPorts model pos pId
            | FromWire wId -> model, [Cmd.ofMsg (Wire <| BusWire.Dragging (wId, pos))]
            | _ -> model, [Cmd.none]


        model, Cmd.batch draggingCommands


    | _ -> failwithf "Sheet - message not implemented"

let init() = 
    let model,cmds = (BusWire.init 400)()
    {
        Wire = model
        SelectedSymbols = []
        SelectedWire = None
        MouseState = MouseIsUp
        SelectionBox = {
            FixedCorner = posOf 0.0 0.0
            MovingCorner = posOf 0.0 0.0
            Show = false
        }
        Grid = {
            Size = 20.
            SnapToGrid = true
            Show = true
        }
        LastMousePos = posOf 0. 0.
        ScrollOffset = posOf 0. 0.
        Zoom = 1.5
    }, Cmd.map Wire cmds
