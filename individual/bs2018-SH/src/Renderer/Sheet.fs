module Sheet
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

type MouseState =
    | MouseIsUp
    | FromPort of CommonTypes.PortId * XYPos    //PortId indicates the source port, XYPos indicates the destiantion (mouse end) of the preview wire.
    | FromSymbol
    | FromEmpty of SelectionBox
    | FromWire of CommonTypes.ConnectionId


type Model = {
    Wire: BusWire.Model
    SelectedSymbols: CommonTypes.ComponentId list
    SelectedWire: CommonTypes.ConnectionId Option
    MouseState: MouseState
    LastMousePos: XYPos
    Grid: Grid
    ScrollOffset: XYPos
    Zoom: float
    Clipboard: CommonTypes.ComponentId list
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
    | CtrlF
    | CtrlC
    | CtrlV

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseDown of XYPos * bool //bool for if shift is pressed.
    | MouseMove of XYPos
    | MouseUp of XYPos
    | Symbol of Symbol.Msg
    | Scroll of float * float
    | SnapToGridMsg

let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    let grid = model.Grid
    let size = 1000.0
    let sizeInPixels = sprintf "%.2fpx" ((size * model.Zoom))

    let bodyLst = [wireSvg]

    // add selection box if SelectionBox.Show
    let bodyLst = 
        match model.MouseState with
        | FromEmpty sb ->
            List.append bodyLst [ 
                        polygon [
                            SVGAttr.Points (polygonPointsString sb.FixedCorner sb.MovingCorner)
                            SVGAttr.Fill "LightBlue"
                            SVGAttr.Stroke "Blue"
                            SVGAttr.FillOpacity 0.5
                            SVGAttr.StrokeWidth 1 ] []
                    ]
        | _ -> bodyLst
    
    // add grid lines if Grid.Show
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

    // add green dotted line if previewing wire
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

    // convert mouse click to zoomed + scrolled version
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
          
          // update scroll offset when div id scrolled
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
            MouseUp(mousePos ev.pageX ev.pageY)
            |> dispatch
          )

          OnMouseLeave (fun ev ->
            MouseUp(mousePos ev.pageX ev.pageY)
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

///If the grid is enabled, converts a position to the closest position on the grid.
let posToGridIfEnabled (grid : Grid) (pos : XYPos) : XYPos =
    if grid.SnapToGrid then
        let leftDist = pos.X % grid.Size
        let upDist = pos.Y % grid.Size
        let x = if leftDist < grid.Size - leftDist then pos.X - leftDist else pos.X + grid.Size - leftDist
        let y = if upDist < grid.Size - upDist then pos.Y - upDist else pos.Y + grid.Size - upDist
        posOf x y
    else
        pos

///Generates commands to highlight the source port and any ports of opposite type within 100px.
let highlightPorts (model : Model) (pos : XYPos) (fromPid : CommonTypes.PortId) = 
    let fromPortType = Symbol.portType model.Wire.Symbol fromPid    
    let portsInRange =
        Symbol.portsInRange model.Wire.Symbol pos 100.
        |> List.filter (fun pId -> (Symbol.portType model.Wire.Symbol pId) <> fromPortType) //ensure port types are opposite
    let ports = List.append portsInRange [fromPid]

    [Cmd.ofMsg (Symbol <| Symbol.HighlightPort ports)]
    |> List.append [Cmd.ofMsg (Symbol <| Symbol.UnhighlightPorts)]
    |> Cmd.batch

///Generates commands to snap all symbols to the grid.
let alignSymbolsToGrid (model : Model) =
    Symbol.getAllSymbols model.Wire.Symbol
    |> List.map (fun el -> (Symbol.symbolBBox model.Wire.Symbol el), el)
    |> List.map (fun (bb,id) -> posToGridIfEnabled {model.Grid with SnapToGrid = true} bb.Pos, bb.Pos, id)
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
    | Scroll (x, y) -> {model with ScrollOffset = posOf x y}, Cmd.none

    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd

    | Symbol sMsg ->
        let wModel, wCmd = BusWire.update (BusWire.Symbol sMsg) model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd

    | KeyPress AltShiftZ -> 
        printStats()
        model, Cmd.none

    ///Set all symbols as selected.
    | KeyPress CtrlA ->
        let idLst = Symbol.getAllSymbols model.Wire.Symbol
        {model with SelectedSymbols = idLst},
        Cmd.ofMsg (Symbol <| Symbol.SetSelected idLst)
    
    ///Delete all selected symbols and any selected wire.
    | KeyPress DEL ->
        let model, wireCommands = 
            match model.SelectedWire with
            | None -> model, [Cmd.none]
            | Some wId -> {model with SelectedWire = None}, [Cmd.ofMsg (Wire <| BusWire.DeleteWire wId)]

        let symbolCommands = [Cmd.ofMsg (Symbol <| Symbol.DeleteSymbols model.SelectedSymbols)]
        model, Cmd.batch (List.append wireCommands symbolCommands)

    ///Create a new symbol at the last position the mouse was down clicked at.
    | KeyPress CtrlN ->
        let posOnGrid = posToGridIfEnabled model.Grid model.LastMousePos
        model, Cmd.ofMsg (Symbol <| Symbol.AddSymbol (CommonTypes.Not, posOnGrid))
    
    ///Toggles snap-to-grid.
    | KeyPress CtrlG ->
        {model with
            Grid = {model.Grid with SnapToGrid = (not model.Grid.SnapToGrid)}
        }, if model.Grid.SnapToGrid then Cmd.none else alignSymbolsToGrid model

    ///Zoom in.
    | KeyPress CtrlEquals ->
        {model with Zoom = model.Zoom + 0.1},Cmd.none

    ///Zoom out.
    | KeyPress CtrlMinus ->
        if model.Zoom > 0.1 then
            {model with Zoom = model.Zoom - 0.1},Cmd.none
        else
            model,Cmd.none

    ///Decrease grid size.
    | KeyPress CtrlQ ->
        let newGridSize = if model.Grid.Size > 2. then model.Grid.Size - 2. else model.Grid.Size
        let newModel =
            {
                model with
                    Grid = {model.Grid with Size = newGridSize}
            }
        newModel, if model.Grid.SnapToGrid then alignSymbolsToGrid newModel else Cmd.none

    ///Increase grid size.
    | KeyPress CtrlW ->
        let newModel =
            {
                model with
                    Grid = {model.Grid with Size = model.Grid.Size + 2.}
            }
        newModel, if model.Grid.SnapToGrid then alignSymbolsToGrid newModel else Cmd.none

    ///Toggle show grid.
    | KeyPress CtrlF ->
        {
            model with
                Grid = {
                    model.Grid with
                        Show = not(model.Grid.Show)
                }
        },
        Cmd.none

    ///Copy
    | KeyPress CtrlC -> {model with Clipboard = model.SelectedSymbols}, Cmd.none

    ///Paste new symbols shifted 20px right and 20px down from originals.
    | KeyPress CtrlV ->
        model,
        [Cmd.ofMsg SnapToGridMsg]
        |> List.append (
            model.Clipboard
            |> List.map (Symbol.symbolBBox model.Wire.Symbol)
            |> List.map (fun bb -> Cmd.ofMsg(Symbol <| Symbol.AddSymbol (CommonTypes.Not, (posAdd bb.Pos (posOf 20. 20.)))))
        )
        |> Cmd.batch

    ///Snaps all symbols to grid.
    | SnapToGridMsg -> model, alignSymbolsToGrid model

    ///Processes mouse down events. Click priority: Port -> Symbol -> Wire -> Empty
    | MouseDown (pos, isShift) ->
        ///Process selected list depending on if symbol is already selected and if shift is pressed.
        let processSelectedSymbols targetedId =
            if List.contains targetedId model.SelectedSymbols then
                if isShift then
                    List.filter (fun el -> el <> targetedId) model.SelectedSymbols
                else
                    model.SelectedSymbols
            else
                if isShift then List.append model.SelectedSymbols [targetedId]
                else [targetedId]

        let mouseState, selectedSymbols, cmds =

            match Symbol.getTargetedPort model.Wire.Symbol pos with
            | Some portId -> FromPort (portId, pos), model.SelectedSymbols, []  //clicked on port

            | None ->
                match Symbol.getTargetedSymbol model.Wire.Symbol pos with
                | Some symbolId ->                                              //clicked on symbol; select it and start dragging.
                    let selSyms = processSelectedSymbols symbolId
                    FromSymbol, selSyms,
                    [
                        Cmd.ofMsg (Symbol <| Symbol.StartDragging (selSyms, (posToGridIfEnabled model.Grid pos)))
                        Cmd.ofMsg (Symbol <| Symbol.SetSelected selSyms)
                    ]

                | None -> 
                    match BusWire.getTargetedWire model.Wire pos with
                        | Some wId -> FromWire wId, model.SelectedSymbols, [Cmd.ofMsg (Wire <| BusWire.StartDrag (wId, pos))]   //clicked on wire; start dragging.

                        | None -> FromEmpty {FixedCorner = pos; MovingCorner = pos; Show = true}, [], []    //clicked on empty space; initialise selection box.

        {model with
            MouseState = mouseState;
            SelectedSymbols = selectedSymbols;
            LastMousePos = pos
        },
        Cmd.batch cmds
    
    ///Processes mouse up events. Behaviour depends on MouseState (i.e. what was mouse-down'ed on)
    | MouseUp pos ->
        let selectedSymbols, selectedWire, cmds =

            match model.MouseState with
            ///If down click was on a port, create a wire if mouse is currently over a port and unhighlight all ports
            | FromPort (fromPid, toPos) ->
                model.SelectedSymbols, model.SelectedWire,
                match Symbol.getTargetedPort model.Wire.Symbol toPos with
                | Some toPid ->
                    let fromPortType = Symbol.portType model.Wire.Symbol fromPid
                    let toPortType = Symbol.portType model.Wire.Symbol toPid
                    if fromPortType <> toPortType then
                        [Cmd.ofMsg (Wire <| BusWire.AddWire (fromPid, toPid))]
                    else []
                | None -> []
                |> List.append [Cmd.ofMsg (Symbol <| Symbol.UnhighlightPorts)]

            ///If down click was on empty space, set selected symbols to those within the selection box,
            ///and deselect any selected wire.
            | FromEmpty sb ->
                let symbolsInSelectionBox =
                    let selectionBBox = bboxFromDiagonals sb.FixedCorner pos
                    Symbol.getSymbolsInTargetArea model.Wire.Symbol selectionBBox
                symbolsInSelectionBox, None,
                [
                    Cmd.ofMsg (Symbol <| Symbol.SetSelected symbolsInSelectionBox)
                    Cmd.ofMsg (Wire <| BusWire.UnselectAll)
                ]

            ///If down click was on a wire, select the wire if it's still targeted, and end wire dragging.
            | FromWire _ -> 
                match BusWire.getTargetedWire model.Wire pos with
                | None -> model.SelectedSymbols, None, [Cmd.ofMsg (Wire <| BusWire.EndDrag)]
                | Some wId -> model.SelectedSymbols, Some wId, [Cmd.ofMsg (Wire <| BusWire.EndDrag); Cmd.ofMsg (Wire <| BusWire.SetSelected wId)]

            ///If down click was on a symbol, end symbol dragging.
            | FromSymbol -> model.SelectedSymbols, model.SelectedWire, [Cmd.ofMsg (Symbol <| Symbol.EndDragging)]

            ///Otherwise do nothing.
            | _ -> model.SelectedSymbols, model.SelectedWire, []

        {
            model with
                SelectedSymbols = selectedSymbols
                SelectedWire = selectedWire
                MouseState = MouseIsUp
        }, 
        Cmd.batch cmds

    ///Processes mouse down events. Behaviour depends on MouseState (i.e. what was mouse-down'ed on)
    | MouseMove pos ->
        match model.MouseState with

        ///Update SelectionBox moving corner.
        | FromEmpty sb -> {model with MouseState = FromEmpty {sb with MovingCorner = pos}}, Cmd.none

        ///Drag symbols with snap-to-grid, if it's enabled.
        | FromSymbol ->
            model,
            Cmd.ofMsg (Symbol <| Symbol.Dragging (model.SelectedSymbols, (posToGridIfEnabled model.Grid pos)))

        ///Update FromPort values to draw preview wire, and highlight relevant ports.
        | FromPort (pId,_) -> {model with MouseState = FromPort (pId, pos)}, highlightPorts model pos pId

        ///Drag selected wire.
        | FromWire wId -> model, Cmd.ofMsg (Wire <| BusWire.Dragging (wId, pos))

        ///Otherwise do nothing
        | _ -> model, Cmd.none


///Intialise state
let init() = 
    let model,cmds = (BusWire.init)()
    {
        Wire = model
        SelectedSymbols = []
        SelectedWire = None
        MouseState = MouseIsUp
        Grid = {
            Size = 20.
            SnapToGrid = true
            Show = true
        }
        LastMousePos = posOf 0. 0.
        ScrollOffset = posOf 0. 0.
        Zoom = 1.
        Clipboard = []
    },
    Cmd.map Wire cmds
