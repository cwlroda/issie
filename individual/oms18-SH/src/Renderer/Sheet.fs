module Sheet

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers
open BBox

type DragState =
    | Wire of CommonTypes.ConnectionId
    | Symbol
    | AreaSelect of XYPos * XYPos * bool
    | WireCreation of CommonTypes.PortId * XYPos
    | Pan of XYPos * XYPos * XYPos
    | NotDragging

type SelectionState =
    | Wire of CommonTypes.ConnectionId
    | Symbols of CommonTypes.ComponentId list
    | Port of CommonTypes.PortId
    | Empty

type Model = {
    Wire: BusWire.Model
    DragState: DragState
    Selection: SelectionState
    PanX: float
    PanY: float
    Zoom: float
    Size: float
}

type KeyboardMsg =
    | CtrlS
    | AltC
    | AltV
    | AltZ
    | AltShiftZ
    | DEL
    | Space
    | Escape
    | AltA
    | CtrlShiftEqual
    | CtrlMinus

type Modifier =
    | Control
    | NoModifier

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT * Modifier

let gridSize = 5

let discretizeToGrid v =
    let fGridSize = float gridSize
    (fGridSize * (floor (v / (float fGridSize))))

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (model: Model) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =
    let borderSize = 3.
    let sizeInPixels = sprintf "%.2fpx" ((model.Size))
    /// Is the mouse button currently down?
    let mDown (ev: Types.MouseEvent) = ev.buttons <> 0.
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev: Types.MouseEvent) =
        let (panX, panY) =
            match model.DragState with
            | Pan (origPan, _, _) ->
                (origPan.X, origPan.Y)
            | _ -> (model.PanX, model.PanY)

        dispatch
        <| MouseMsg(
            { 
                Button = match ev.button with
                         | 0. -> MouseButton.Left
                         | 1. -> MouseButton.Middle
                         | 2. -> MouseButton.Right
                         | _ -> MouseButton.Unknown
                Op = op
                Pos =
                    { X = (ev.clientX - borderSize - panX) / model.Zoom
                      Y = (ev.clientY - borderSize - panY) / model.Zoom } },
            if ev.ctrlKey then
                Control
            else
                NoModifier
        )

    let gridlines =
        let panX = int (discretizeToGrid model.PanX)
        let panY = int (discretizeToGrid model.PanY)
        let size = int model.Size

        let getGridCoords offset =
            [0..int (ceil model.Size)]
            |> List.map (fun i -> i * gridSize)
            |> List.takeWhile (fun gc -> gc < int model.Size)

        let color gc offset =
            if (gc - offset) % (gridSize * 10) = 0 then "grey" else "lightgrey"

        let makeLine x0 y0 x1 y1 color offset =
            line [
                X1 (x0 - panX)
                Y1 (y0 - panY)
                X2 (x1 - panX)
                Y2 (y1 - panY)
                SVGAttr.Stroke (color offset)
                SVGAttr.StrokeWidth "1px"
            ] []
        let createHalfGrid offset lineFun =
            getGridCoords offset
            |> List.map (lineFun >> (fun f -> f offset))

        g [] (
            [
                createHalfGrid panX (fun x -> makeLine x -gridSize x size <| color x)
                createHalfGrid panY (fun y -> makeLine -gridSize y size y <| color y)
            ]
            |> List.concat
        )

    div [ Style [ Height sizeInPixels
                  MaxWidth sizeInPixels
                  Border (sprintf "%fpx solid green" borderSize)
                  CSSProp.OverflowX OverflowOptions.Hidden
                  CSSProp.OverflowY OverflowOptions.Hidden
                ]
    ] [ svg [ Style [ Height sizeInPixels
                      Width sizeInPixels ]
              OnMouseDown(fun ev -> (mouseOp Down ev))
              OnMouseUp(fun ev -> (mouseOp Up ev))
              OnMouseMove(fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
              OnMouseLeave(fun ev -> mouseOp Leave ev)] [
            g [ Style [ Transform (sprintf "translate(%fpx,%fpx) scale(%f)" model.PanX model.PanY model.Zoom) ] ] [  // top-level transform style attribute for zoom
                match model.DragState with
                | AreaSelect (p1, p2, additive) ->
                    let area = pointsToBBox p1 p2
                    let color =
                        if additive then
                            "Green"
                        else
                            "Blue"

                    gridlines
                    svgReact // the application code
                    rect [
                        X area.Pos.X
                        Y area.Pos.Y
                        SVGAttr.Width area.Width
                        SVGAttr.Height area.Height
                        SVGAttr.Fill color
                        SVGAttr.Stroke color
                        SVGAttr.StrokeWidth "1px"
                        SVGAttr.FillOpacity 0.3
                    ] []
                | WireCreation (pId, p) -> 
                    let portPos = Symbol.portPos model.Wire.Symbol pId

                    gridlines
                    svgReact
                    line [
                        X1 portPos.X
                        Y1 portPos.Y
                        X2 p.X
                        Y2 p.Y
                        SVGAttr.Stroke "Purple"
                        SVGAttr.StrokeWidth "3px"
                    ] []
                | _ ->
                    gridlines
                    svgReact
            ]
        ] // top-level transform style attribute for zoom
    ] // top-level transform style attribute for zoom



/// for the demo code
let view (model: Model) (dispatch: Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom model wireSvg dispatch


let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let snapToGrid p =
        posOf (discretizeToGrid p.X) (discretizeToGrid p.Y)

    match msg with
    | Wire wMsg ->
        let wModel, wCmd = BusWire.update wMsg model.Wire
        { model with Wire = wModel }, Cmd.map Wire wCmd
    | MouseMsg (mT, modifier) ->
        match (mT.Op, mT.Pos, modifier) with
        | (Down, p, m) ->
            match mT.Button with
            | MouseButton.Left ->
                let targetedPort = Symbol.getTargetedPort model.Wire.Symbol p
                let selectedWire = BusWire.getTargetedWire model.Wire p
                let selectedSymbol =
                    Symbol.getTargetedSymbol model.Wire.Symbol p

                match (targetedPort, selectedSymbol, selectedWire, m) with
                | (Some pId, _, _, _) ->
                    { model with
                        Selection=SelectionState.Port pId
                        DragState=WireCreation (pId, p)
                    }, Cmd.batch [
                        Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPort pId)))
                        Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                    ]
                | (_, Some sym, _, m) ->
                    let isSelected =
                        match model.Selection with
                        | Symbols sIdLst ->
                            sIdLst
                            |> Set.ofList
                            |> Set.contains sym
                        | _ -> false

                    let selectedSymbols =
                        if isSelected then
                            match model.Selection with
                            | Symbols sIdLst -> sIdLst
                            | _ -> [sym]
                        else if m = Control then
                            match model.Selection with
                            | Symbols sIdLst ->
                                sIdLst
                                |> Set.ofList
                                |> Set.add sym
                                |> Set.toList
                            | _ -> [sym]
                        else
                            [sym]

                    { model with
                        Selection = Symbols selectedSymbols
                        DragState=DragState.Symbol
                    }
                    , Cmd.batch [
                        Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select selectedSymbols)))
                        Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.StartDragging (selectedSymbols, snapToGrid p))))
                        Cmd.ofMsg (Wire (BusWire.UnselectAll))
                    ]
                | (_, _, Some wId, _) ->
                    { model with
                        Selection = SelectionState.Wire wId
                        DragState = DragState.Wire wId
                    }
                    , Cmd.batch [
                        Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                        Cmd.ofMsg (Wire (BusWire.Select wId))
                        Cmd.ofMsg (Wire (BusWire.StartDrag (wId, snapToGrid p)))
                    ]
                | (_, None, None, Control) ->
                    { model with
                        DragState=AreaSelect (p, p, true)
                    }, Cmd.ofMsg (Wire (BusWire.UnselectAll))
                | (_, None, None, NoModifier) ->
                    { model with
                        Selection = Empty
                        DragState = AreaSelect (p, p, false)
                    }, Cmd.batch [
                        Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                        Cmd.ofMsg (Wire (BusWire.UnselectAll))
                    ]
            | MouseButton.Right ->
                { model with Selection=Empty;DragState=NotDragging }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.UnhighlightPorts)))
                    Cmd.ofMsg (Wire (BusWire.UnselectAll))
                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.AddSymbol (CommonTypes.ComponentType.And, snapToGrid p))))
                ]
            | MouseButton.Middle ->
                { model with
                    Selection = Empty
                    DragState = Pan (posOf model.PanX model.PanY, p, p)
                }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.UnhighlightPorts)))
                    Cmd.ofMsg (Wire (BusWire.UnselectAll))
                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                ]
            | _ ->
                { model with Selection=Empty;DragState=NotDragging }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.UnhighlightPorts)))
                    Cmd.ofMsg (Wire (BusWire.UnselectAll))
                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                ]
        | (Drag, p, _) ->
            // TODO: Send StartDrag
            match model.DragState with
            | AreaSelect (start, _, additive) ->
                { model with DragState=AreaSelect (start, p, additive)}
                , Cmd.none
            | DragState.Symbol ->
                let selectedSymbols =
                    match model.Selection with
                    | Symbols s -> s
                    | _ -> failwithf "We only drag if there is a selection"
                model,
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Dragging (selectedSymbols, snapToGrid p))))
            | DragState.Wire wId ->
                model,
                Cmd.ofMsg (Wire (BusWire.Dragging (wId, snapToGrid p)))
            | WireCreation (pId, _) ->
                { model with
                    DragState=WireCreation (pId, p)
                }, Cmd.none
            | Pan (origPan, panStart, _) ->
                { model with
                    DragState=Pan (origPan, panStart, p)
                    PanX = origPan.X + (p.X - panStart.X) * model.Zoom
                    PanY = origPan.Y + (p.Y - panStart.Y) * model.Zoom
                }, Cmd.none
            | NotDragging -> model, Cmd.none
        | (Up, _, _) ->
            match model.DragState with
            | AreaSelect (p1, p2, additive) ->
                let area = pointsToBBox p1 p2
                let selectedSymbols =
                    if additive then
                        let selectedSymbols = match model.Selection with
                                              | Symbols s -> s
                                              | _ -> []
                        selectedSymbols
                        |> Set.ofList
                        |> Set.union (
                            Set.ofList (
                                Symbol.getSymbolsInBBox model.Wire.Symbol area
                                )
                            )
                        |> Set.toList
                    else
                        Symbol.getSymbolsInBBox model.Wire.Symbol area

                {model with
                    DragState = NotDragging
                    Selection = Symbols selectedSymbols
                }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select selectedSymbols)))
                ]
            | DragState.Wire _ ->
                { model with
                    DragState = NotDragging
                }
                , Cmd.ofMsg (Wire (BusWire.EndDrag))
            | DragState.Symbol ->
                { model with
                    DragState = NotDragging
                }
                , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
            | DragState.WireCreation (pIdStart, p) ->
                let targetedPort = Symbol.getTargetedPort model.Wire.Symbol p
                { model with
                    DragState = NotDragging
                    Selection = Empty
                }
                , match targetedPort with
                  | Some pIdEnd when pIdEnd <> pIdStart ->
                      Cmd.batch [
                          Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.UnhighlightPorts)))
                          Cmd.ofMsg (Wire (BusWire.AddWire (pIdStart, pIdEnd)))
                      ]
                  | _ -> 
                      Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.UnhighlightPorts)))
            | DragState.Pan (origPan, panStart, panEnd) ->
                { model with
                    DragState=NotDragging
                    PanX = origPan.X + (panEnd.X - panStart.X) * model.Zoom
                    PanY = origPan.Y + (panEnd.Y - panStart.Y) * model.Zoom
                }, Cmd.none
            | _ -> model, Cmd.none
        | (Leave, _, _) ->
            match model.DragState with
            | AreaSelect _ ->
                {model with
                    DragState = NotDragging
                } , Cmd.none
            | DragState.Wire _ ->
                { model with
                    DragState = NotDragging
                }
                , Cmd.ofMsg (Wire (BusWire.EndDrag))
            | DragState.Symbol ->
                { model with
                    DragState = NotDragging
                }
                , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
            | DragState.WireCreation _ ->
                { model with
                    DragState = NotDragging
                    Selection = Empty
                }
                , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.UnhighlightPorts)))
            | DragState.Pan (origPan, pStart, panEnd) ->
                { model with
                    DragState=NotDragging
                    PanX = origPan.X + (panEnd.X - pStart.X)
                    PanY = origPan.Y + (panEnd.Y - pStart.Y)
                }, Cmd.none
            | NotDragging -> model, Cmd.none
        | (Move, _, _) -> model, Cmd.none
    | KeyPress AltA ->
        let selectedSymbols = Symbol.getAllSymbols model.Wire.Symbol

        { model with Selection = Symbols selectedSymbols }
        , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select selectedSymbols)))
    | KeyPress Escape ->
        match model.DragState with
        | NotDragging ->
            { model with Selection = Empty }
            , Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.UnhighlightPorts)))
                Cmd.ofMsg (Wire (BusWire.UnselectAll))
            ]
        | WireCreation _ ->
            { model with
                Selection=Empty
                DragState=NotDragging
            }
            , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.UnhighlightPorts)))
        | _ ->
            {model with DragState=NotDragging}
            , Cmd.none
    | KeyPress k when k = CtrlShiftEqual || k = CtrlMinus ->
        let multiplier =
            match k with
            | CtrlShiftEqual -> 1.05
            | CtrlMinus -> 0.95
            | _ -> failwithf "This can't happen"

        let adjustPanValue pan =
            let translatedPan = pan - model.Size / 2.
            let multipliedPan = multiplier * translatedPan
            multipliedPan + model.Size / 2.

        let (panX, panY) =
            (adjustPanValue model.PanX, adjustPanValue model.PanY)
        { model with
            Zoom = multiplier * model.Zoom
            PanX = panX
            PanY = panY
        }, Cmd.none
    | KeyPress AltShiftZ ->
        printStats () // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress DEL ->
        { model with Selection = Empty;DragState=NotDragging },
        match model.Selection with
        | SelectionState.Wire wId ->
            Cmd.ofMsg (Wire (BusWire.Delete wId))
        | Symbols sIdLst ->
            Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.DeleteSymbols sIdLst)))
        | SelectionState.Port _ -> Cmd.none
        | Empty -> Cmd.none
    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            //| Space -> CommonTypes.Green
            | _ -> CommonTypes.Grey

        printfn "Color:%A" c
        printfn "Key:%A" s
        model, Cmd.none

let init () =
    let model, cmds = (BusWire.init 40) ()
    {
        Wire = model
        Selection = Empty
        DragState = NotDragging
        PanX = 0.
        PanY = 0.
        Zoom = 1.
        Size = 1000.
    }, Cmd.map Wire cmds
