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
    | Symbol of BusWire.Model * bool
    | AreaSelect of XYPos * XYPos * bool
    | WireCreation of CommonTypes.PortId * XYPos
    | Pan of XYPos * XYPos * XYPos
    | NotDragging

type SelectionState =
    | Wire of CommonTypes.ConnectionId
    | Symbols of CommonTypes.ComponentId list
    | Empty

type CopyState =
    | Copied of (CommonTypes.ComponentType * XYPos) list
    | Uninitialized

type Model = {
    Wire: BusWire.Model
    DragState: DragState
    Selection: SelectionState
    CopyState: CopyState
    MousePosition: XYPos
    PanX: float
    PanY: float
    Zoom: float
    Size: float
    UndoList: BusWire.Model list
    UndoLocation: int
}

type KeyboardMsg =
    | AltC
    | AltV
    | AltZ
    | AltShiftZ
    | DEL
    | Escape
    | AltA
    | CtrlShiftEqual
    | CtrlEqual
    | CtrlMinus

type Modifier =
    | Control
    | NoModifier

type Msg =
    | SaveState of BusWire.Model
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT * Modifier

let gridSize = 5
let undoHistorySize = 50

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
        let panX = int (discretizeToGrid (model.PanX / model.Zoom))
        let panY = int (discretizeToGrid (model.PanY / model.Zoom))
        let size = int (ceil (model.Size / model.Zoom))

        let getGridCoords =
            [0..gridSize..size]

        let getColorAndOpacity gc offset =
            if (gc - offset) % (gridSize * 10) = 0 then
                "grey", 1.
            else
                "lightgrey", 0.5

        let makeLine x0 y0 x1 y1 colorAndOpacity offset =
            let (color, opacity) = colorAndOpacity offset
            line [
                X1 (x0 - panX)
                Y1 (y0 - panY)
                X2 (x1 - panX)
                Y2 (y1 - panY)
                SVGAttr.Stroke color
                SVGAttr.StrokeWidth "1px"
                SVGAttr.StrokeOpacity opacity
            ] []
        let createHalfGrid offset lineFun =
            getGridCoords
            |> List.map (lineFun >> (fun f -> f offset))

        g [] (
            [
                createHalfGrid panX (fun x -> makeLine x -gridSize x size <| getColorAndOpacity x)
                createHalfGrid panY (fun y -> makeLine -gridSize y size y <| getColorAndOpacity y)
            ]
            |> List.concat
        )

    let overlay =
        match model.DragState with
        | AreaSelect (p1, p2, additive) ->
            let area = pointsToBBox p1 p2
            let color = if additive then "Green" else "Blue"

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

            line [
                X1 portPos.X
                Y1 portPos.Y
                X2 p.X
                Y2 p.Y
                SVGAttr.Stroke "Purple"
                SVGAttr.StrokeWidth "3px"
            ] []
        | _ -> g [] []

    let errorOverlay =
        g [] (
            BusWire.getErrors model.Wire
            |> List.map (fun e ->
                // Approximate the width of the text, does not work very well
                // the good solutions are all very hacky, will look at more in
                // the team phase per piazza
                let textWidth = 8.8 * float e.Msg.Length
                g [] [
                    rect [
                        X e.Pos.X
                        Y e.Pos.Y
                        SVGAttr.Width (textWidth + 20.)
                        SVGAttr.Height "30"
                        SVGAttr.Fill "#a11"
                        SVGAttr.Stroke "black"
                        SVGAttr.StrokeWidth "4px"
                    ] []
                    text [ // "Bus Decoder" header
                        X (e.Pos.X + 5.)
                        Y (e.Pos.Y + 20.)
                        Style [
                            UserSelect UserSelectOptions.None
                            TextAnchor "left"
                            DominantBaseline "baseline"
                            FontFamily "Monospace"
                            FontSize "15px"
                            FontWeight "Bold"
                            Fill "White"
                        ]
                    ] [str e.Msg]
                ]
            )
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
                    gridlines
                    svgReact
                    overlay
                    errorOverlay
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

    let handleLeftClick p m =
        let targetedPort = Symbol.getTargetedPort model.Wire.Symbol p
        let selectedWire = BusWire.getTargetedWire model.Wire p
        let selectedSymbol = Symbol.getTargetedSymbol model.Wire.Symbol p

        match (targetedPort, selectedSymbol, selectedWire, m) with
        | (Some pId, _, _, _) ->
            { model with DragState=WireCreation (pId, p) }
            , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
        | (None, Some sym, _, m) ->
            let isSelected =
                match model.Selection with
                | Symbols sIdLst -> Set.contains sym (Set.ofList sIdLst)
                | _ -> false

            let selectedSymbols =
                match (model.Selection, isSelected, m) with
                | (Symbols sIdLst, true, _) -> sIdLst
                | (Symbols sIdLst, _, Control) ->
                    sIdLst
                    |> Set.ofList
                    |> Set.add sym
                    |> Set.toList
                | _ -> [sym]

            { model with
                Selection = Symbols selectedSymbols
                DragState = DragState.Symbol (model.Wire, false)
            }, Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select selectedSymbols)))
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.StartDragging (selectedSymbols, snapToGrid p))))
                Cmd.ofMsg (Wire (BusWire.UnselectAll))
            ]
        | (None, None, Some wId, _) ->
            { model with
                Selection = SelectionState.Wire wId
                DragState = DragState.Wire wId
            }, Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                Cmd.ofMsg (Wire (BusWire.Select wId))
                Cmd.ofMsg (Wire (BusWire.StartDrag (wId, snapToGrid p)))
            ]
        | (None, None, None, Control) ->
            { model with
                DragState=AreaSelect (p, p, true)
            }
            , Cmd.ofMsg (Wire (BusWire.UnselectAll))
        | (None, None, None, NoModifier) ->
            { model with
                Selection = Empty
                DragState = AreaSelect (p, p, false)
            }, Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                Cmd.ofMsg (Wire (BusWire.UnselectAll))
            ]

    let getHighlightingAfterUndoAndRedo model =
        match model.Selection with
        | Symbols sIdLst ->
            Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select sIdLst)))
                Cmd.ofMsg (Wire (BusWire.UnselectAll))
            ]
        | SelectionState.Wire wId ->
            Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                Cmd.ofMsg (Wire (BusWire.Select wId))
            ]
        | SelectionState.Empty ->
            Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                Cmd.ofMsg (Wire (BusWire.UnselectAll))
            ]

    let handleKeyPress key =
        match key with
        | AltA ->
            let selectedSymbols = Symbol.getAllSymbols model.Wire.Symbol

            { model with Selection = Symbols selectedSymbols }
            , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select selectedSymbols)))
        | Escape ->
            match model.DragState with
            | NotDragging ->
                { model with Selection = Empty }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                    Cmd.ofMsg (Wire (BusWire.UnselectAll))
                ]
            | DragState.Symbol (prevWireModel, _) ->
                {model with
                    Wire=prevWireModel
                    DragState=NotDragging
                }, Cmd.none
            | WireCreation _ -> model, Cmd.none
            | _ -> {model with DragState=NotDragging}, Cmd.none
        | CtrlShiftEqual | CtrlMinus | CtrlEqual ->
            let multiplier =
                match key with
                | CtrlShiftEqual -> 1.05
                | CtrlMinus -> 0.95
                | CtrlEqual -> 1. / model.Zoom
                | _ -> failwithf "This can't happen"

            let multiplier =
                match multiplier * model.Zoom with
                | z when z > 3. -> 3. / model.Zoom
                | z when z < 0.5 -> 0.5 / model.Zoom
                | _ -> multiplier

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
        | DEL ->
            { model with Selection = Empty;DragState=NotDragging }
            , match model.Selection with
              | SelectionState.Wire wId ->
                  Cmd.batch [
                      Cmd.ofMsg (Wire (BusWire.Delete wId))
                      Cmd.ofMsg <| SaveState model.Wire
                  ]
              | Symbols sIdLst ->
                  Cmd.batch [
                      Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.DeleteSymbols sIdLst)))
                      Cmd.ofMsg <| SaveState model.Wire
                  ]
              | Empty -> Cmd.none
        | AltC ->
            match model.Selection with
            | Symbols sIdLst ->
                let sIdAndPosLst = 
                    sIdLst
                    |> List.map (fun sId ->
                        (Symbol.symbolType model.Wire.Symbol sId,
                         (Symbol.symbolBBox model.Wire.Symbol sId).Pos)
                    )

                let topLeftOfSymbols =
                    let middle lst = List.min lst + (List.max lst - List.min lst) / 2.

                    sIdAndPosLst
                    |> List.map (fun (_, p) -> (p.X, p.Y))
                    |> List.fold (fun (xs, ys) (x, y) -> (x :: xs, y :: ys)) ([], [])
                    |> fun (xs, ys) -> posOf (middle xs) (middle ys)
                    
                let sIdAndPosLst = 
                    sIdAndPosLst
                    |> List.map (fun (sId, p) -> (sId, posDiff p topLeftOfSymbols))

                { model with CopyState=CopyState.Copied sIdAndPosLst }
            | _ -> model
            , Cmd.none
        | AltV ->
            model
            , match model.CopyState with
              | Copied sIdLst ->
                  Cmd.batch (
                      List.map (fun (sType, p) ->
                          Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.AddSymbol (sType, posAdd p model.MousePosition))))
                      ) sIdLst
                      @ [Cmd.ofMsg (SaveState model.Wire)]
                  )
              | Uninitialized -> Cmd.none
        | AltZ ->
            match model.UndoLocation with
            | 0 when model.UndoList.Length > 0 ->
                let portsNearMouse = Symbol.portsInRange model.Wire.Symbol model.MousePosition 100.
                let newWire = List.head model.UndoList

                { model with
                    Wire=newWire
                    UndoList=model.Wire :: model.UndoList
                    UndoLocation=1
                }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts portsNearMouse))) 
                    getHighlightingAfterUndoAndRedo model
                ]
            | loc when loc + 1 < model.UndoList.Length ->
                let portsNearMouse = Symbol.portsInRange model.Wire.Symbol model.MousePosition 100.
                let newWire = List.item (loc + 1) model.UndoList

                { model with
                    Wire=newWire
                    UndoLocation=loc + 1
                }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts portsNearMouse))) 
                    getHighlightingAfterUndoAndRedo model
                ]
            | _ -> model, Cmd.none
        | AltShiftZ ->
            match model.UndoLocation with
            | 0 -> model, Cmd.none
            | 1 ->
                let portsNearMouse = Symbol.portsInRange model.Wire.Symbol model.MousePosition 100.
                let newWire = List.head model.UndoList

                { model with
                    Wire=newWire
                    UndoList=List.tail model.UndoList
                    UndoLocation=0
                }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts portsNearMouse))) 
                    getHighlightingAfterUndoAndRedo model
                ]
            | loc ->
                let portsNearMouse = Symbol.portsInRange model.Wire.Symbol model.MousePosition 100.
                let newWire = List.item (loc - 1) model.UndoList

                { model with
                    Wire=newWire
                    UndoLocation=loc - 1
                }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts portsNearMouse))) 
                    getHighlightingAfterUndoAndRedo model
                ]

    match msg with
    | SaveState savedWire ->
        let undoList = savedWire :: match model.UndoLocation with
                                    | 0 -> model.UndoList
                                    | loc -> List.skip (loc + 1) model.UndoList
        let undoList = List.truncate undoHistorySize undoList
        { model with UndoList=undoList;UndoLocation=0 }, Cmd.none
    | Wire wMsg ->
        let wModel, wCmd = BusWire.update wMsg model.Wire
        { model with Wire = wModel }
        , Cmd.map Wire wCmd
    | KeyPress k -> handleKeyPress k
    | MouseMsg (mT, modifier) ->
        match (mT.Op, mT.Pos, modifier) with
        | (Down, p, m) ->
            match mT.Button with
            | MouseButton.Left -> handleLeftClick p m
            | MouseButton.Right ->
                { model with Selection=Empty;DragState=NotDragging }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
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
                    Cmd.ofMsg (Wire (BusWire.UnselectAll))
                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                ]
            | _ ->
                { model with Selection=Empty;DragState=NotDragging }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
                    Cmd.ofMsg (Wire (BusWire.UnselectAll))
                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                ]
        | (Drag, p, _) ->
            let portsNearMouse = Symbol.portsInRange model.Wire.Symbol p 100.

            let model = { model with MousePosition=p}

            match model.DragState with
            | AreaSelect (start, _, additive) ->
                { model with DragState=AreaSelect (start, p, additive)}
                , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts portsNearMouse)))
            | DragState.Symbol (prevWire, _) ->
                let selectedSymbols =
                    match model.Selection with
                    | Symbols s -> s
                    | _ -> failwithf "We only drag if there is a selection"

                { model with DragState=DragState.Symbol (prevWire, true) }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Dragging (selectedSymbols, snapToGrid p))))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts portsNearMouse))) 
                ]
            | DragState.Wire wId ->
                model, Cmd.ofMsg (Wire (BusWire.Dragging (wId, snapToGrid p)))
            | WireCreation (pId, _) ->
                { model with DragState=WireCreation (pId, p) }
                , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts portsNearMouse))) 
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
                        let selectedSymbols =
                            match model.Selection with
                            | Symbols s -> s
                            | _ -> []

                        selectedSymbols
                        |> Set.ofList
                        |> Set.union (Set.ofList (Symbol.getSymbolsInTargetArea model.Wire.Symbol area))
                        |> Set.toList
                    else
                        Symbol.getSymbolsInTargetArea model.Wire.Symbol area

                {model with
                    DragState = NotDragging
                    Selection = Symbols selectedSymbols
                }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select selectedSymbols)))
                ]
            | DragState.Wire _ ->
                { model with DragState = NotDragging }
                , Cmd.ofMsg (Wire (BusWire.EndDrag))
            | DragState.Symbol (prevWireModel, didDrag) ->
                if didDrag then
                    { model with DragState = NotDragging }
                    , Cmd.batch [
                        Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
                        Cmd.ofMsg (SaveState prevWireModel)
                    ]
                else
                    { model with DragState = NotDragging }
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
                          Cmd.ofMsg (Wire (BusWire.AddWire (pIdStart, pIdEnd)))
                          Cmd.ofMsg (SaveState model.Wire)
                      ]
                  | _ -> Cmd.none
            | DragState.Pan (origPan, panStart, panEnd) when mT.Button = MouseButton.Middle ->
                { model with
                    DragState=NotDragging
                    PanX = origPan.X + (panEnd.X - panStart.X) * model.Zoom
                    PanY = origPan.Y + (panEnd.Y - panStart.Y) * model.Zoom
                }, Cmd.none
            | _ -> model, Cmd.none
        | (Leave, _, _) ->
            let newModel = {model with DragState=NotDragging}
            match model.DragState with
            | AreaSelect _ ->
                {newModel with DragState=NotDragging}, Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts [])))
            | DragState.Wire _ ->
                newModel, Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts [])))
                    Cmd.ofMsg (SaveState model.Wire)
                ]
            | DragState.Symbol (prevWireModel, _) ->
                newModel, Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts [])))
                    Cmd.ofMsg (SaveState prevWireModel)
                ]
            | DragState.WireCreation _ ->
                { newModel with Selection = Empty }
                , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts [])))
            | DragState.Pan (origPan, pStart, panEnd) ->
                { newModel with
                    PanX = origPan.X + (panEnd.X - pStart.X)
                    PanY = origPan.Y + (panEnd.Y - pStart.Y)
                }, Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts [])))
            | NotDragging -> newModel, Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts [])))
        | (Move, p, _) ->
            let portsNearMouse = Symbol.portsInRange model.Wire.Symbol p 100.

            { model with MousePosition=p}
            , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts portsNearMouse)))

let init () =
    let model, cmds = (BusWire.init 40) ()
    {
        Wire = model
        Selection = Empty
        DragState = NotDragging
        CopyState = Uninitialized
        MousePosition = posOf 0. 0.
        PanX = 0.
        PanY = 0.
        Zoom = 1.
        Size = 1000.
        UndoList = []
        UndoLocation = 0
    }, Cmd.map Wire cmds
