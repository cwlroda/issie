module Sheet

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type SubModel = BusWire.Model * Symbol.Model

type CreateElements =
    {
        Symbols: (CommonTypes.Component) list
        Wires: (CommonTypes.PortId * CommonTypes.PortId) list
    }

type DragState =
    | Wire of bool * SubModel
    | Symbol of bool * SubModel
    | AreaSelect of XYPos * XYPos * bool
    | WireCreation of CommonTypes.PortId * XYPos
    | Pan of XYPos * XYPos * XYPos
    | NotDragging

type SelectionState =
    | Wire of CommonTypes.ConnectionId
    | Symbols of CommonTypes.ComponentId list
    | Empty

type CopyState =
    | Copied of CreateElements * int
    | Uninitialized
    
type Model = {
    Wire: BusWire.Model
    Symbol: Symbol.Model
    DragState: DragState
    Selection: SelectionState
    CopyState: CopyState
    MousePosition: XYPos
    ClickPosition: XYPos
    PanX: float
    PanY: float
    Zoom: float
    Width: float
    Height: float
    UndoList: SubModel list
    RedoList: SubModel list
    PrevPortType: CommonTypes.PortType Option
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
    | INS
    | AltShiftD

type Modifier =
    | Control
    | NoModifier

type Msg =
    | UpdateSize of float * float
    | SaveState of SubModel
    | CreateObjects of CreateElements
    | Wire of BusWire.Msg
    | Symbol of Symbol.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT * Modifier

/// Constants that will be turned into settings at a later date
let gridSize = 10
let undoHistorySize = 50

let displaySvgWithZoom (model: Model) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =
    let borderSize = 3.
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
                Button =
                    match ev.button with
                    | 0. -> MouseButton.Left
                    | 1. -> MouseButton.Middle
                    | 2. -> MouseButton.Right
                    | _ -> MouseButton.Unknown
                Op = op
                /// Have to adjust the mouse position because of the border
                /// to ensure that the top left is (0, 0) for the mouse
                Pos =
                    {
                        X = (ev.clientX - borderSize - panX) / model.Zoom
                        Y = (ev.clientY - borderSize - panY) / model.Zoom
                    } 
            },
            if ev.ctrlKey then
                Control
            else
                NoModifier
        )

    let updateSize (rect: Types.ClientRect) =
        dispatch
        <| UpdateSize (System.Math.Round rect.width, System.Math.Round rect.height)

    let gridlines =
        let pan =
            snapToGrid {
                X = model.PanX / model.Zoom
                Y = model.PanY / model.Zoom
            }

        let width = int (ceil (model.Width / model.Zoom))
        let height = int (ceil (model.Height / model.Zoom))

        let getGridCoords size =
            [0..gridSize..size+2*gridSize]

        let getColorAndOpacity gc offset =
            if (gc - offset) % (gridSize * 10) = 0 then
                "grey", 1.
            else
                "lightgrey", 0.5

        let makeLine x0 y0 x1 y1 colorAndOpacity offset =
            let (color, opacity) = colorAndOpacity offset
            line [
                X1 (x0 - int pan.X)
                Y1 (y0 - int pan.Y)
                X2 (x1 - int pan.X)
                Y2 (y1 - int pan.Y)
                SVGAttr.Stroke color
                SVGAttr.StrokeWidth "1px"
                SVGAttr.StrokeOpacity opacity
            ] []

        let createHalfGrid size offset lineFun =
            getGridCoords size
            |> List.map (lineFun >> (fun f -> f offset))

        g [] (
            [
                createHalfGrid width (int pan.X) (fun x -> makeLine x -gridSize x (height+gridSize) <| getColorAndOpacity x)
                createHalfGrid height (int pan.Y) (fun y -> makeLine -gridSize y (width+gridSize) y <| getColorAndOpacity y)
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
            let portPos = Symbol.portPos model.Symbol pId

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
            BusWire.getErrors model.Wire model.Symbol
            |> List.filter (fun e ->
                match BusWire.getTargetedWire model.Wire model.MousePosition with
                | Some id when id = e.Id -> true
                | _ -> false
            )
            |> List.map (fun e ->
                // Approximate the width of the text, does not work very well
                // the good solutions are all very hacky, will look at more in
                // the team phase per piazza
                let textWidth = 5.9 * float e.Msg.Length
                g [] [
                    rect [
                        X model.MousePosition.X
                        Y (model.MousePosition.Y - 20.)
                        SVGAttr.Width (textWidth + 20.)
                        SVGAttr.Height "20"
                        SVGAttr.Fill "#a11"
                        SVGAttr.Stroke "black"
                        SVGAttr.StrokeWidth "2px"
                    ] []
                    text [ // "Bus Decoder" header
                        X (model.MousePosition.X + 5.)
                        Y (model.MousePosition.Y - 7.)
                        Style [
                            UserSelect UserSelectOptions.None
                            TextAnchor "left"
                            DominantBaseline "baseline"
                            FontFamily "Monospace"
                            FontSize "10px"
                            FontWeight "Bold"
                            Fill "White"
                        ]
                    ] [str e.Msg]
                ]
            )
        )

    let cursorType =
        match model.DragState with
        | DragState.Symbol (_, _) -> "grabbing"
        | DragState.Wire (_, _) -> "grabbing"
        | DragState.Pan (_, _, _) -> "grabbing"
        | DragState.WireCreation (_, _) -> "crosshair"
        | DragState.AreaSelect (_, _, _) -> "crosshair"
        | _ -> "grab"
    
    div [ Style [
        Position PositionOptions.Fixed
        Height "100%"
        Width "100%"
        CSSProp.Top 0.
        CSSProp.Bottom 0.
        CSSProp.Left 0.
        CSSProp.Right 0.
        Border (sprintf "%fpx solid green" borderSize)
        CSSProp.OverflowX OverflowOptions.Hidden
        CSSProp.OverflowY OverflowOptions.Hidden
        CSSProp.Cursor cursorType
      ]
    ] [ svg [
            Style [
                Height "100%"
                Width "100%"
            ]
            Ref (fun html ->
                if html = null then
                    ()
                else
                    (updateSize (html.getBoundingClientRect()))
                )
            OnMouseDown(fun ev -> (mouseOp Down ev))
            OnMouseUp(fun ev -> (mouseOp Up ev))
            OnMouseMove(fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
            OnMouseLeave(fun ev -> mouseOp Leave ev)
        ] [
            g [ Style [ Transform (sprintf "translate(%fpx,%fpx) scale(%f)" model.PanX model.PanY model.Zoom) ] ] [  // top-level transform style attribute for zoom
                gridlines
                svgReact
                actionOverlay
                errorOverlay
            ]
        ]
    ]


    
let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let saveStateIfDraggedCmd didDrag prevWireModel =
        if didDrag then
            Cmd.ofMsg (SaveState prevWireModel)
        else
            Cmd.none

    let processDrag p =
        let model = { model with MousePosition=p}

        match model.DragState with
        | AreaSelect (start, _, additive) ->
            { model with DragState=AreaSelect (start, p, additive)}, Cmd.none
        | DragState.Symbol (_, prevWire) ->
            let selectedSymbols =
                match model.Selection with
                | SelectionState.Wire wId ->
                    let currentMousePos = model.MousePosition
                    let w = BusWire.findWire model.Wire wId
                    let selSeg = w.SelectedSegment
                    let prevPortType = 
                        match selSeg with
                        | x when x = 0 -> 
                            Some (CommonTypes.PortType.Output)
                        | x when x = w.Segments.Length - 1 -> 
                            Some (CommonTypes.PortType.Input)
                        |_ -> None 
                    { model with 
                        DragState=DragState.Wire (true, prevWireModel)
                        PrevPortType = prevPortType 
                    }
                    , Cmd.ofMsg (Wire (BusWire.Dragging (wId, snapToGrid p)))
                | _ -> failwithf "Can only drag if there is a selection"

            { model with DragState=DragState.Symbol (true, prevWire) }
            , Cmd.batch [
                Cmd.ofMsg (Symbol (Symbol.Dragging (selectedSymbols, snapToGrid p)))
                Cmd.ofMsg (Wire (BusWire.DraggingSymbols selectedSymbols))
            ]
        | DragState.Wire (_, prevWireModel) ->
            match model.Selection with
            | SelectionState.Wire wId ->
                let currentMousePos = model.MousePosition
                let w = BusWire.findWire model.Wire wId
                let selSeg = w.SelectedSegment
                let offset = 
                    match selSeg with
                    | x when x = 0 -> 
                        Some ((posDiff currentMousePos (List.item x w.Segments).StartPos),CommonTypes.PortType.Output)
                    | x when x = w.Segments.Length - 1 -> 
                        Some ((posDiff (List.item x w.Segments).EndPos currentMousePos),CommonTypes.PortType.Input)
                    |_ -> None 
                { model with 
                    DragState=DragState.Wire (true, prevWireModel)
                    Offset = offset 
                }
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
        | _ -> model, Cmd.none

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
        | DragState.Wire (didDrag, prevSubModel) ->
            
            newModel, Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.EndDrag))
                saveStateIfDraggedCmd didDrag prevSubModel
            ]

        | DragState.Symbol (didDrag, (prevWire, prevSymbol)) ->
                let selectedSymbols =
                    match model.Selection with
                    | Symbols s -> s
                    | _ -> failwithf "Can only stop dragging if there is a selection"
                if Symbol.symbolsCollide selectedSymbols model.Symbol then
                    { newModel with
                        Wire = prevWire
                        Symbol = prevSymbol
                    } , Cmd.none
                else
                    newModel, Cmd.batch [
                        Cmd.ofMsg (Symbol (Symbol.EndDragging))
                        saveStateIfDraggedCmd didDrag (prevWire, prevSymbol)
                    ]
        | DragState.WireCreation _ ->
            { newModel with Selection = Empty }
            , Cmd.none
        | DragState.Pan (origPan, panStart, panEnd) ->
            updatePan model origPan panStart panEnd
            , Cmd.none
        | _ -> newModel, Cmd.none

    let handleLeftClick model p m =
        let targetedPort = Symbol.getTargetedPort model.Symbol p
        let selectedWire = BusWire.getTargetedWire model.Wire p
        let selectedSymbol = Symbol.getTargetedSymbol model.Symbol p

        match (targetedPort, selectedSymbol, selectedWire, m) with
        | (Some pId, _, _, _) ->
            { model with DragState=WireCreation (pId, p) }
            , Cmd.none
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
                DragState = DragState.Symbol (false, (model.Wire, model.Symbol))
            }, Cmd.batch [
                Cmd.ofMsg (Symbol (Symbol.StartDragging (selectedSymbols, snapToGrid p)))
            ]
        | (None, None, Some wId, _) ->
            { model with
                Selection = SelectionState.Wire wId
                DragState = DragState.Wire (false, (model.Wire, model.Symbol))
            }, Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.StartDrag (wId, p)))

            ]
        | (None, None, None, Control) ->
            { model with
                DragState = AreaSelect (p, p, true)
            }, Cmd.none
        | (None, None, None, NoModifier) ->
            { model with
                Selection = Empty
                DragState = AreaSelect (p, p, false)
                ClickPosition = p
            }, Cmd.none

    let rec batchInfer (symModel:Symbol.Model) (pIdStart:CommonTypes.PortId) (pIdEnd:CommonTypes.PortId) (createOrDelete:CommonTypes.CreateOrDelete) (visited:CommonTypes.PortId list) (iterations:int): Symbol.Model =
        if iterations > 100 then symModel else
        let createMsg = Symbol.CreateInference (pIdStart,pIdEnd)
        let deleteMsg = Symbol.DeleteInference (pIdStart,pIdEnd)
        let createDeleteMsg = 
            match createOrDelete with
            | CommonTypes.CreateOrDelete.Create -> createMsg
            | CommonTypes.CreateOrDelete.Delete -> deleteMsg

        let tgtSymbol = 
            Symbol.findPort symModel pIdEnd
            |> Symbol.findSymbolFromPort symModel

        let listOfConnections = 
            tgtSymbol.Component.OutputPorts
            |> Map.toList
            |> List.map (fun (newSrcPId, _) -> 
                newSrcPId
                |> BusWire.getAllPidEnds model.Wire
                |> List.filter (fun newTgtPort -> 
                    let newTgtSymbol = 
                        Symbol.findPort symModel newTgtPort
                        |> Symbol.findSymbolFromPort symModel
                    match newTgtSymbol.Component.Type with
                    | CommonTypes.SplitWire _ | CommonTypes.BusSelection _  -> true
                    | comp when comp = CommonTypes.MergeWires -> true
                    | comp when comp = CommonTypes.IOLabel -> true
                    | _ -> false
                )
                |> List.map (fun newTgtPId -> (newSrcPId, newTgtPId))
            )
            |> List.toSeq
            |> List.concat
        match listOfConnections with
        | [] -> fst (Symbol.update createDeleteMsg symModel) 
        | _ -> 
            match (List.tryFind (fun i -> i=pIdStart) visited) with
            |None ->
                match tgtSymbol.Component.Type with
                | CommonTypes.MergeWires | CommonTypes.IOLabel ->
                    let firstUpdatedModel = fst (Symbol.update createDeleteMsg symModel)
                    let finalUpdatedModel = 
                        List.fold (fun acc (newPIdStart, newPIdEnd) -> 
                            batchInfer acc  newPIdStart newPIdEnd CommonTypes.CreateOrDelete.Create (visited @ [pIdStart]) (iterations + 1)
                        ) firstUpdatedModel listOfConnections
                    finalUpdatedModel
                | CommonTypes.SplitWire _ ->
                    let firstUpdatedModel = fst (Symbol.update createDeleteMsg symModel)
                    let finalUpdatedModel = 
                        List.fold (fun acc (newPIdStart, newPIdEnd) -> 
                            let pStart = Symbol.findPort symModel newPIdStart 
                            if pStart.PortNumber <> Some (CommonTypes.PortNumber 1) then
                                batchInfer acc newPIdStart newPIdEnd CommonTypes.CreateOrDelete.Create (visited @ [pIdStart]) (iterations + 1)
                            else acc   
                        ) firstUpdatedModel listOfConnections
                    finalUpdatedModel
                |CommonTypes.BusSelection _ -> 
                    let firstUpdatedModel = fst (Symbol.update createDeleteMsg symModel)
                    let finalUpdatedModel = 
                        List.fold (fun acc (newPIdStart, newPIdEnd) -> 
                            let pStart = Symbol.findPort symModel newPIdStart 
                            if pStart.PortNumber <> Some (CommonTypes.PortNumber 0) then
                                batchInfer acc newPIdStart newPIdEnd CommonTypes.CreateOrDelete.Create (visited @ [pIdStart]) (iterations + 1)
                            else acc   
                        ) firstUpdatedModel listOfConnections
                    finalUpdatedModel
                | _ -> symModel
            |Some _ ->
                
                let port = Symbol.findPort symModel pIdStart
                if port.Width <> (CommonTypes.PortWidth 0) then
                    let sym = Symbol.findSymbolFromPort symModel port
                    let newOutput = 
                        sym.Component.OutputPorts
                        |> Map.add pIdStart 
                            {port with
                                Width = CommonTypes.PortWidth 0
                            }
                    let newComp = 
                        {sym.Component with
                            OutputPorts = newOutput
                        }
                    let newModel = Symbol.updateSymbolModelWithComponent symModel newComp
                    batchInfer newModel pIdStart pIdEnd CommonTypes.CreateOrDelete.Create [] (iterations + 1)
                else symModel

    let handleKeyPress key =

        match key with
        | AltA ->
            let selectedSymbols = Symbol.getAllSymbols model.Symbol

            { model with Selection = Symbols selectedSymbols }
            , Cmd.none
        | Escape ->
            match model.DragState with
            | NotDragging ->
                { model with Selection = Empty }
                , Cmd.none
            | DragState.Symbol (_, (prevWireModel, prevSymbolModel)) ->
                {model with
                    Wire=prevWireModel
                    Symbol=prevSymbolModel
                    DragState=NotDragging
                }, Cmd.none
            | DragState.Wire (_, (prevWireModel, prevSymbolModel)) ->
                {model with
                    Wire=prevWireModel
                    Symbol=prevSymbolModel
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
            match model.Selection with
            | SelectionState.Wire wId ->
                let wire = (BusWire.findWire model.Wire wId)
                let inference = 
                    batchInfer model.Symbol wire.SrcPort wire.TargetPort CommonTypes.CreateOrDelete.Delete [] 0
                    
                { model with 
                    Selection = Empty;DragState=NotDragging 
                    Symbol = inference
                },
                Cmd.batch 
                        [
                            Cmd.ofMsg (Wire (BusWire.DeleteWire wId))
                            Cmd.ofMsg <| SaveState (model.Wire, model.Symbol)
                        ]
            | Symbols sIdLst ->
                let inference = 
                    BusWire.getWiresOfSymbols model.Wire model.Symbol sIdLst
                    |> Map.toList
                    |> List.fold (fun acc (_, wire) ->
                        batchInfer acc wire.SrcPort wire.TargetPort CommonTypes.CreateOrDelete.Delete [] 0
                    ) model.Symbol

                let remainingMsg = 
                    [
                        Cmd.ofMsg (Wire (BusWire.DeleteSymbols sIdLst))
                        Cmd.ofMsg (Symbol (Symbol.DeleteSymbols sIdLst))
                        Cmd.ofMsg (Wire (BusWire.RoutingUpdate))
                        Cmd.ofMsg <| SaveState (model.Wire, model.Symbol)
                    ]
                { model with 
                    Selection = Empty;DragState=NotDragging 
                    Symbol = inference
                },
                Cmd.batch (remainingMsg)
            | Empty -> 
                { model with 
                    Selection = Empty;DragState=NotDragging
                },
                Cmd.none
        | AltC ->
            match model.Selection with
            | Symbols sIdLst ->
                let sCopyDataLst = 
                    sIdLst
                    |> List.map (fun sId ->
                        (Symbol.getSymbolFromSymbolId model.Symbol sId).Component
                    )

                let pCopyDataLst =
                    BusWire.getConnectedWires model.Wire model.Symbol sIdLst
                    |> Map.toList
                    |> List.map (fun (_, w) -> w.SrcPort, w.TargetPort)

                { model with
                    CopyState =
                        Copied ({
                            Symbols = sCopyDataLst
                            Wires = pCopyDataLst
                        }, 0)
                }
            | _ -> model
            , Cmd.none
        | AltV ->
            match model.CopyState with
            | Copied ({Symbols=symbolData;Wires=wireData} as copyData, timesPasted) ->
                let sCopyDataLst, portConversionMap =
                    symbolData
                    |> List.map Symbol.createDeepCopyOfComponent
                    |> List.unzip

                let sCopyDataLst =
                    sCopyDataLst
                    |> List.map (fun comp ->
                        let p = posOf comp.X comp.Y
                        let label = comp.Label + "_" + (string <| timesPasted + 1)
                        Symbol.updateCompoment comp p label
                    )

                let portConversionMap =
                    portConversionMap
                    |> List.collect Map.toList
                    |> Map.ofList

                let pCopyDataLst =
                    wireData
                    |> List.map (fun (src, target) ->
                        Map.find src portConversionMap,
                        Map.find target portConversionMap
                    )

                { model with
                    CopyState = Copied (copyData, timesPasted + 1)
                }, Cmd.ofMsg (CreateObjects {Symbols=sCopyDataLst;Wires=pCopyDataLst})
            | Uninitialized -> model , Cmd.none
        | AltZ ->
            match model.UndoList with
            | [] -> model, Cmd.none
            | (newWire, newSymbol) :: undoList ->
                { model with
                    Wire=newWire
                    Symbol=newSymbol
                    UndoList=undoList
                    RedoList= (model.Wire, model.Symbol) :: model.RedoList
                }
                , Cmd.none
        | AltShiftZ ->
            match model.RedoList with
            | [] -> model, Cmd.none
            | (newWire, newSymbol) :: redoList ->
                { model with
                    Wire=newWire
                    Symbol=newSymbol
                    UndoList=(model.Wire, model.Symbol) :: model.UndoList
                    RedoList=redoList
                }
                , Cmd.none
        | INS ->
            { model with Selection = Empty;DragState=NotDragging },
            Cmd.batch [
                let rng = System.Random 0
                let inList, outList =
                    Symbol.allPortsInModel model.Symbol
                    |> Map.toList
                    |> List.partition (fun (_, p) -> p.PortType = CommonTypes.PortType.Output)

                let inList, outList =
                    inList |> List.map fst,
                    outList |> List.map fst

                let n = min inList.Length outList.Length

                let s1, s2 =
                    outList.[rng.Next(0, n-1)],
                    inList.[rng.Next(0, n-1)]
                
                Cmd.ofMsg (Wire (BusWire.AddWire (s1, s2)))
                Cmd.ofMsg <| SaveState (model.Wire, model.Symbol)
            ]
        | AltShiftD ->
            model, Cmd.ofMsg (Wire (BusWire.Debug))

    let handleMouseMsg mT modifier =
        match (mT.Op, mT.Pos, modifier) with
        | (Down, p, mods) ->
            match mT.Button with
            | MouseButton.Left ->
                match model.DragState with
                | DragState.Symbol _ ->
                    model, Cmd.none
                | _ ->
                    let (newModel, cmds) = handleInterruptAction model
                    let model, clickCmds = handleLeftClick newModel newModel.MousePosition mods

                    model, Cmd.batch [
                        cmds
                        clickCmds
                    ]
            | MouseButton.Middle ->
                let (newModel, cmds) = handleInterruptAction model

                { newModel with DragState = Pan (posOf model.PanX model.PanY, p, p) }
                , cmds
            | _ ->
                let (newModel, cmds) = handleInterruptAction model

                { newModel with Selection=Empty }
                , cmds
        | (Drag, p, _) ->
            processDrag p
        | (Up, _, _) ->
            let newModel = {model with DragState=NotDragging }

            match model.DragState with
            | AreaSelect (p1, p2, additive) ->
                let area = pointsToBBox p1 p2
                let selectedSymbols =
                    let targetedSymbols = Symbol.getSymbolsInTargetArea model.Symbol area

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
                , Cmd.none
            | DragState.Wire (didDrag, prevWireModel) ->
                let nullCase = 
                    newModel, Cmd.batch (
                        [
                            Cmd.ofMsg (Wire (BusWire.EndDrag))
                            saveStateIfDraggedCmd didDrag prevWireModel
                        ]
                    )
                let wireInQuestion =
                    match model.Selection with
                    |SelectionState.Wire x -> x
                    |_ -> failwithf "Not supposed to happen"            
                let oldModel = newModel.Wire.WX
                let oldWire = Map.find wireInQuestion oldModel
                let oldSrcPortId = oldWire.SrcPort
                let oldTgtPortId = oldWire.TargetPort
                let inferredDelete = 
                    batchInfer newModel.Symbol oldSrcPortId oldTgtPortId CommonTypes.CreateOrDelete.Delete [] 0

                match newModel.PrevPortType with
                | None -> nullCase
                | Some CommonTypes.PortType.Input ->
                    let findingPort = Symbol.getTargetedPort newModel.Symbol model.MousePosition
                    match findingPort with
                    | Some portId -> 
                        let foundPort = Symbol.findPort newModel.Symbol portId
                        match foundPort.PortType with
                        | CommonTypes.PortType.Input ->
                            let inferredCreate = 
                                batchInfer inferredDelete oldSrcPortId foundPort.PortId CommonTypes.CreateOrDelete.Create [] 0
                            let newerModel = 
                                {newModel with
                                    PrevPortType = None
                                    Symbol = inferredCreate
                                }
                            newerModel, Cmd.batch (
                                [
                                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                                ]  
                                @
                                [saveStateIfDraggedCmd didDrag prevWireModel]
                            )
                        | _ -> 
                            let newerModel = 
                                {newModel with
                                    PrevPortType = None
                                }
                            newerModel, Cmd.batch (
                                [
                                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                                ]  
                                @
                                [saveStateIfDraggedCmd didDrag prevWireModel]
                            )

                    | None -> nullCase
                | Some CommonTypes.PortType.Output ->
                    let findingPort = Symbol.getTargetedPort newModel.Symbol model.MousePosition
                    match findingPort with
                    | Some portId ->
                        let foundPort = Symbol.findPort newModel.Symbol portId
                        match foundPort.PortType with
                        | CommonTypes.PortType.Output ->
                            let inferredCreate = 
                                batchInfer inferredDelete foundPort.PortId oldTgtPortId CommonTypes.CreateOrDelete.Create [] 0
                            let newerModel = 
                                {newModel with
                                    PrevPortType = None
                                    Symbol = inferredCreate
                                }
                            newerModel, Cmd.batch (
                                
                                [
                                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                                ]  
                                @
                                [saveStateIfDraggedCmd didDrag prevWireModel]
                            )
                        | _ -> 
                            let newerModel = 
                                {newModel with
                                    PrevPortType = None
                                }
                            newerModel, Cmd.batch (
                                [
                                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                                ]  
                                @
                                [saveStateIfDraggedCmd didDrag prevWireModel]
                            )
                    | None -> nullCase
            | DragState.Symbol (didDrag, prevWireModel) ->
                let selectedSymbols =
                    match model.Selection with
                    | Symbols s -> s
                    | _ -> failwithf "Can only stop dragging if there is a selection"
                if Symbol.symbolsCollide selectedSymbols model.Symbol then model, Cmd.none else
                    newModel, Cmd.batch [
                        Cmd.ofMsg (Symbol (Symbol.EndDragging))
                        // Cmd.ofMsg (Wire (BusWire.EndDragSymbols))
                        saveStateIfDraggedCmd didDrag prevWireModel
                    ]
            | DragState.WireCreation (pIdStart, p) ->
                let targetedPort = Symbol.getTargetedPort newModel.Symbol p
                
                match targetedPort with
                | Some pIdEnd when pIdEnd <> pIdStart ->
                    let inferred = 
                        batchInfer newModel.Symbol pIdStart pIdEnd CommonTypes.CreateOrDelete.Create [] 0
                        
                    { newModel with 
                        Selection = Empty
                        Symbol = inferred 
                    },
                    Cmd.batch 
                        ([
                            Cmd.ofMsg (Wire (BusWire.AddWire (pIdStart, pIdEnd)))
                            Cmd.ofMsg (SaveState (model.Wire, model.Symbol))
                        ])

                | _ -> 
                    { newModel with 
                        Selection = Empty
                    },Cmd.none
            | DragState.Pan (origPan, panStart, panEnd) ->
                updatePan newModel origPan panStart panEnd
                , Cmd.none
            | _ -> newModel, Cmd.none
        | (Leave, _, _) -> handleInterruptAction model
        | (Move, p, _) -> processDrag p

    match msg with
    | UpdateSize (w, h) ->
        { model with
            Width=w
            Height=h
        }, Cmd.none
    | SaveState savedSubModel ->
        { model with
            UndoList = List.truncate undoHistorySize <| savedSubModel :: model.UndoList
            RedoList = []
        }, Cmd.none
    | CreateObjects {Symbols=symData;Wires=wireData} -> 
        let addSymbolCommand =
            let middleOfSymbols =
                let middle cs = List.min cs + (List.max cs - List.min cs) / 2.

                symData
                |> List.map Symbol.componentBBox
                |> List.map (fun bb ->
                    (bb.Pos.X, bb.Pos.X + bb.Width, bb.Pos.Y, bb.Pos.Y + bb.Height))
                |> List.fold (fun (xs, ys) (x1, x2, y1, y2) -> (x1 :: x2 :: xs, y1 :: y2 :: ys)) ([], [])
                |> fun (xs, ys) -> posOf (middle xs) (middle ys)

            symData
            |> List.map (fun comp ->
                let p =
                    middleOfSymbols
                    |> posDiff (posOf comp.X comp.Y)
                    |> posAdd model.MousePosition
                    |> snapToGrid

                printfn "%A" p

                Symbol.updateCompoment comp p comp.Label
            )
            |> List.map (Symbol.AddSymbol >> Symbol >> Cmd.ofMsg)
            |> Cmd.batch

        let addWireCommand =
            wireData
            |> List.map (BusWire.AddWire >> Wire >> Cmd.ofMsg)
            |> Cmd.batch

        let createdSymbols =
            symData
            |> List.map (fun comp -> comp.Id)

        { model with
            DragState=DragState.Symbol (true, (model.Wire, model.Symbol))
            Selection=SelectionState.Symbols createdSymbols
        }, Cmd.batch [
            addSymbolCommand
            addWireCommand
            Cmd.ofMsg (Symbol (Symbol.StartDragging (createdSymbols, snapToGrid model.MousePosition)))
        ]
    | Symbol sMsg ->
        let sModel, sCmd = Symbol.update sMsg model.Symbol

        { model with Symbol = sModel }
        , Cmd.map Symbol sCmd
    | Wire wMsg ->
        let wModel, wCmd = BusWire.update wMsg model.Wire model.Symbol

        { model with Wire = wModel }
        , Cmd.map Wire wCmd
    | KeyPress k -> handleKeyPress k
    | MouseMsg (mT, modifier) -> handleMouseMsg mT modifier


let view (model: Model) (dispatch: Msg -> unit) =
    let sDispatch sMsg = dispatch (Symbol sMsg)
    let selectedSymbols =
        match model.Selection with
        | SelectionState.Symbols sIdLst -> Some sIdLst
        | _ -> None

    let portTypeToNotHighlight =
        match model.DragState with
        | DragState.WireCreation (pId, _) ->
            Some <| Symbol.portType model.Symbol pId
        | _ -> None

    let symbolSvg = Symbol.view model.Symbol selectedSymbols model.MousePosition portTypeToNotHighlight sDispatch

    let wDispatch wMsg = dispatch (Wire wMsg)
    let selectedWire =
        match model.Selection with
        | SelectionState.Wire wId -> Some wId
        | _ -> None

    let wireSvg = BusWire.view model.Wire selectedWire model.Symbol wDispatch
    let symbolsAndWiresSvg =
        g [] [
            wireSvg
            symbolSvg
        ]

    displaySvgWithZoom model symbolsAndWiresSvg dispatch


let init () =
    let sModel, sCmds = Symbol.init ()
    let wModel, wCmds = (BusWire.init) ()
    {
        Wire = wModel
        Symbol = sModel
        Selection = Empty
        DragState = NotDragging
        CopyState = Uninitialized
        MousePosition = posOf 0. 0.
        ClickPosition = posOf 0. 0.
        PanX = 0.
        PanY = 0.
        Zoom = 1.
        Width = 1000.
        Height = 1000.
        UndoList = []
        RedoList = []
        PrevPortType = None
    }, Cmd.batch [
        Cmd.map Symbol sCmds
        Cmd.map Wire wCmds
    ]


