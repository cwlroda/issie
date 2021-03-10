module Sheet

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers
open BBox

type DragState =
    | Wire of bool * BusWire.Model
    | Symbol of bool * BusWire.Model
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
    Width: float
    Height: float
    UndoList: BusWire.Model list
    RedoList: BusWire.Model list
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

/// Constants that will be turned into settings at a later date
let gridSize = 10
let undoHistorySize = 50
let portHighlightRange = 100.

let discretizeToGrid v =
    let gridSize = float gridSize
    let leftDist = v % gridSize
    if leftDist < gridSize - leftDist then v - leftDist else v + gridSize - leftDist

let displaySvgWithZoom (model: Model) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =
    let borderSize = 3.
    let widthInPixels = sprintf "%.2fpx" ((model.Width))
    let heightInPixels = sprintf "%.2fpx" ((model.Height))
    let mDown (ev: Types.MouseEvent) = ev.buttons <> 0.

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
                /// Have to adjust the mouse position because of the border
                /// to ensure that the top left is (0, 0) for the mouse
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
        let width = int (ceil (model.Width / model.Zoom))
        let height = int (ceil (model.Height / model.Zoom))

        let getGridCoords size =
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

        let createHalfGrid size offset lineFun =
            getGridCoords size
            |> List.map (lineFun >> (fun f -> f offset))

        g [] (
            [
                createHalfGrid width panX (fun x -> makeLine x -gridSize x height <| getColorAndOpacity x)
                createHalfGrid height panY (fun y -> makeLine -gridSize y width y <| getColorAndOpacity y)
            ]
            |> List.concat
        )

    let actionOverlay =
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
                SVGAttr.Stroke "green"
                SVGAttr.StrokeWidth "3px"
                SVGAttr.StrokeOpacity 1
                SVGAttr.StrokeDasharray "5, 3"
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

    div [ Style [
              Height heightInPixels
              Width widthInPixels
              Border (sprintf "%fpx solid green" borderSize)
              CSSProp.OverflowX OverflowOptions.Hidden
              CSSProp.OverflowY OverflowOptions.Hidden
          ]
    ] [ svg [ Style [
                  Height heightInPixels
                  Width widthInPixels
              ]
              OnMouseDown(fun ev -> (mouseOp Down ev))
              OnMouseUp(fun ev -> (mouseOp Up ev))
              OnMouseMove(fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
              OnMouseLeave(fun ev -> mouseOp Leave ev)] [
            g [ Style [ Transform (sprintf "translate(%fpx,%fpx) scale(%f)" model.PanX model.PanY model.Zoom) ] ] [  // top-level transform style attribute for zoom
                    gridlines
                    svgReact
                    actionOverlay
                    errorOverlay
            ]
        ]
    ]


let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let snapToGrid p =
        posOf (discretizeToGrid p.X) (discretizeToGrid p.Y)

    let deselectSymbolsCmd =
        Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.SetSelected [])))

    let highlightPortsNearCmd p =
        let currentType = 
                match model.DragState with
                | WireCreation (pId, _) -> Some <| Symbol.portType model.Wire.Symbol pId
                | _ -> None
        let portsNearMouse =
            Symbol.portsInRange model.Wire.Symbol p portHighlightRange
            |> List.filter (fun pId ->
                match currentType with
                | Some pType -> pType <> Symbol.portType model.Wire.Symbol pId
                | None -> true
            )
        Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts portsNearMouse)))

    let saveStateIfDraggedCmd didDrag prevWireModel =
        if didDrag then
            Cmd.ofMsg (SaveState prevWireModel)
        else
            Cmd.none

    let updatePan model origPan panStart panEnd =
        let diffX = (panEnd.X - panStart.X)
        let diffY = (panEnd.Y - panStart.Y)
        { model with
            PanX = origPan.X + diffX * model.Zoom
            PanY = origPan.Y + diffY * model.Zoom
            MousePosition = posDiff model.MousePosition <| posOf diffX diffY
        }

    let handleInterruptAction model =
        let newModel = {model with DragState=NotDragging}

        match model.DragState with
        | AreaSelect _ -> newModel, Cmd.none
        | DragState.Wire (didDrag, prevWireModel) ->
            newModel, Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.EndDrag))
                saveStateIfDraggedCmd didDrag prevWireModel
            ]
        | DragState.Symbol (didDrag, prevWireModel) ->
            newModel, Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
                saveStateIfDraggedCmd didDrag prevWireModel
            ]
        | DragState.WireCreation _ ->
            { newModel with Selection = Empty }
            , Cmd.none
        | DragState.Pan (origPan, panStart, panEnd) ->
            updatePan model origPan panStart panEnd
            , Cmd.none
        | NotDragging -> newModel, Cmd.none

    let handleLeftClick model p m =
        let targetedPort = Symbol.getTargetedPort model.Wire.Symbol p
        let selectedWire = BusWire.getTargetedWire model.Wire p
        let selectedSymbol = Symbol.getTargetedSymbol model.Wire.Symbol p

        match (targetedPort, selectedSymbol, selectedWire, m) with
        | (Some pId, _, _, _) ->
            { model with DragState=WireCreation (pId, p) }
            , deselectSymbolsCmd
        | (None, Some sym, _, m) ->
            let isSelected =
                match model.Selection with
                | Symbols sIdLst -> Set.contains sym (Set.ofList sIdLst)
                | _ -> false

            let selectedSymbols =
                match (model.Selection, isSelected, m) with
                | (Symbols sIdLst, true, _) -> sIdLst
                | (Symbols sIdLst, _, Control) ->
                    (sym, Set.ofList sIdLst)
                    ||> Set.add
                    |> Set.toList
                | _ -> [sym]

            { model with
                Selection = Symbols selectedSymbols
                DragState = DragState.Symbol (false, model.Wire)
            }, Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.SetSelected selectedSymbols)))
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.StartDragging (selectedSymbols, snapToGrid p))))
                Cmd.ofMsg (Wire (BusWire.UnselectAll))
            ]
        | (None, None, Some wId, _) ->
            { model with
                Selection = SelectionState.Wire wId
                DragState = DragState.Wire (false, model.Wire)
            }, Cmd.batch [
                deselectSymbolsCmd
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
                deselectSymbolsCmd
                Cmd.ofMsg (Wire (BusWire.UnselectAll))
            ]

    let handleKeyPress key =
        let highlightingAfterUndoAndRedoCmd model =
            Cmd.batch [
                highlightPortsNearCmd model.MousePosition
                match model.Selection with
                | Symbols sIdLst ->
                    Cmd.batch [
                        Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.SetSelected sIdLst)))
                        Cmd.ofMsg (Wire (BusWire.UnselectAll))
                    ]
                | SelectionState.Wire wId ->
                    Cmd.batch [
                        deselectSymbolsCmd
                        Cmd.ofMsg (Wire (BusWire.Select wId))
                    ]
                | SelectionState.Empty ->
                    Cmd.batch [
                        deselectSymbolsCmd
                        Cmd.ofMsg (Wire (BusWire.UnselectAll))
                    ]
            ]

        match key with
        | AltA ->
            let selectedSymbols = Symbol.getAllSymbols model.Wire.Symbol

            { model with Selection = Symbols selectedSymbols }
            , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.SetSelected selectedSymbols)))
        | Escape ->
            match model.DragState with
            | NotDragging ->
                { model with Selection = Empty }
                , Cmd.batch [
                    deselectSymbolsCmd
                    Cmd.ofMsg (Wire (BusWire.UnselectAll))
                ]
            | DragState.Symbol (_, prevWireModel) ->
                {model with
                    Wire=prevWireModel
                    DragState=NotDragging
                }, Cmd.none
            | WireCreation _ -> model, Cmd.none
            | _ -> {model with DragState=NotDragging}, Cmd.none
        | CtrlShiftEqual | CtrlMinus | CtrlEqual ->
            let newZoom =
                match key with
                | CtrlShiftEqual -> 1.05 * model.Zoom
                | CtrlMinus -> 0.95 * model.Zoom
                | CtrlEqual -> 1.
                | _ -> failwithf "This can't happen"

            let newZoom =
                match newZoom with
                | z when z > 3. -> 3.
                | z when z < 0.5 -> 0.5
                | z -> z

            let adjustPanValue pan mCoord =
                // For zooming "around" the mouse position
                // 
                // Proof:
                // Zn = scalar zoom value for case n
                // P = (panX, panY) for case n
                // S = (size, size)
                // M = (mouseX, mouseY)
                // Sn is the screen coordinate of M at Zn and Pn
                // Based on the screen covering -P/Z to (S-P)/Z and
                // Thus Sn = Zn * M + Pn
                // And then you can derive the P2 that satisfies
                // S1 = S2
                (model.Zoom - newZoom) * mCoord + pan

            let (panX, panY) =
                adjustPanValue model.PanX model.MousePosition.X
                , adjustPanValue model.PanY model.MousePosition.Y

            { model with
                Zoom = newZoom
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
                          Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.AddSymbol (sType, snapToGrid (posAdd p model.MousePosition)))))
                      ) sIdLst
                      @ [Cmd.ofMsg (SaveState model.Wire)]
                  )
              | Uninitialized -> Cmd.none
        | AltZ ->
            match model.UndoList with
            | [] -> model, Cmd.none
            | newWire :: undoList ->
                { model with
                    Wire=newWire
                    UndoList=undoList
                    RedoList= model.Wire :: model.RedoList
                }
                , highlightingAfterUndoAndRedoCmd model
        | AltShiftZ ->
            match model.RedoList with
            | [] -> model, Cmd.none
            | newWire :: redoList ->
                { model with
                    Wire=newWire
                    UndoList=model.Wire :: model.UndoList
                    RedoList=redoList
                }
                , highlightingAfterUndoAndRedoCmd model

    let handleMouseMsg mT modifier =
        match (mT.Op, mT.Pos, modifier) with
        | (Down, p, mods) ->
            let discardSelectionsCmd =
                Cmd.batch [
                    deselectSymbolsCmd
                    Cmd.ofMsg (Wire (BusWire.UnselectAll))
                ]
            
            let (model, cmds) = handleInterruptAction model

            match mT.Button with
            | MouseButton.Left ->
                let model, clickCmds = handleLeftClick model model.MousePosition mods

                model, Cmd.batch [
                    cmds
                    clickCmds
                ]
            | MouseButton.Right ->
                { model with Selection=Empty }
                , Cmd.batch [
                    cmds
                    discardSelectionsCmd
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.AddSymbol (CommonTypes.ComponentType.And, snapToGrid model.MousePosition))))
                    Cmd.ofMsg (SaveState model.Wire)
                ]
            | MouseButton.Middle ->
                { model with DragState = Pan (posOf model.PanX model.PanY, p, p) }
                , cmds
            | _ ->
                { model with Selection=Empty }
                , Cmd.batch [
                    cmds
                    discardSelectionsCmd
                ]
        | (Drag, p, _) ->
            let model = { model with MousePosition=p}

            let (model, cmd) =
                match model.DragState with
                | AreaSelect (start, _, additive) ->
                    { model with DragState=AreaSelect (start, p, additive)}, Cmd.none
                | DragState.Symbol (_, prevWire) ->
                    let selectedSymbols =
                        match model.Selection with
                        | Symbols s -> s
                        | _ -> failwithf "Can only drag if there is a selection"

                    { model with DragState=DragState.Symbol (true, prevWire) }
                    , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Dragging (selectedSymbols, snapToGrid p))))
                | DragState.Wire (_, prevWireModel) ->
                    match model.Selection with
                    | SelectionState.Wire wId ->
                        { model with DragState=DragState.Wire (true, prevWireModel) }
                        , Cmd.ofMsg (Wire (BusWire.Dragging (wId, snapToGrid p)))
                    | _ -> failwithf "Can only drag if there is a selection"
                | WireCreation (pId, _) ->
                    { model with DragState=WireCreation (pId, p) }, Cmd.none
                | Pan (origPan, panStart, _) ->
                    { model with
                        DragState=Pan (origPan, panStart, p)
                        PanX = origPan.X + (p.X - panStart.X) * model.Zoom
                        PanY = origPan.Y + (p.Y - panStart.Y) * model.Zoom
                    }, Cmd.none
                | NotDragging -> model, Cmd.none

            model, Cmd.batch [
                cmd
                highlightPortsNearCmd p
            ]
        | (Up, _, _) ->
            let newModel = { model with DragState=NotDragging }

            match model.DragState with
            | AreaSelect (p1, p2, additive) ->
                let area = pointsToBBox p1 p2
                let selectedSymbols =
                    let targetedSymbols = Symbol.getSymbolsInTargetArea model.Wire.Symbol area

                    if additive then
                        let selectedSymbols =
                            match model.Selection with
                            | Symbols s -> s
                            | _ -> []

                        (Set.ofList selectedSymbols, Set.ofList targetedSymbols)
                        ||> Set.union
                        |> Set.toList
                    else
                        targetedSymbols

                { newModel with Selection = Symbols selectedSymbols }
                , Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.SetSelected selectedSymbols)))
            | DragState.Wire (didDrag, prevWireModel) ->
                newModel, Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                    saveStateIfDraggedCmd didDrag prevWireModel
                ]
            | DragState.Symbol (didDrag, prevWireModel) ->
                newModel, Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
                    saveStateIfDraggedCmd didDrag prevWireModel
                ]
            | DragState.WireCreation (pIdStart, p) ->
                let targetedPort = Symbol.getTargetedPort model.Wire.Symbol p
                { newModel with Selection = Empty }
                , match targetedPort with
                  | Some pIdEnd when pIdEnd <> pIdStart ->
                      Cmd.batch [
                          Cmd.ofMsg (Wire (BusWire.AddWire (pIdStart, pIdEnd)))
                          Cmd.ofMsg (SaveState model.Wire)
                      ]
                  | _ -> Cmd.none
            | DragState.Pan (origPan, panStart, panEnd) ->
                updatePan newModel origPan panStart panEnd
                , Cmd.none
            | _ -> newModel, Cmd.none
        | (Leave, _, _) ->
            let (model, cmd) = handleInterruptAction model

            model, Cmd.batch [
                cmd
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.HighlightPorts [])))
            ]
        | (Move, p, _) ->
            { model with MousePosition=p}
            , highlightPortsNearCmd p

    match msg with
    | SaveState savedWire ->
        { model with
            UndoList=List.truncate undoHistorySize <| savedWire :: model.UndoList
            RedoList=[]
        }, Cmd.none
    | Wire wMsg ->
        let wModel, wCmd = BusWire.update wMsg model.Wire

        { model with Wire = wModel }
        , Cmd.map Wire wCmd
    | KeyPress k -> handleKeyPress k
    | MouseMsg (mT, modifier) -> handleMouseMsg mT modifier


let view (model: Model) (dispatch: Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom model wireSvg dispatch


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
        Width = 1000.
        Height = 700.
        UndoList = []
        RedoList = []
    }, Cmd.map Wire cmds
