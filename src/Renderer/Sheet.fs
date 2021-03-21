﻿module Sheet

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type SubModel = BusWire.Model * Symbol.Model

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
    | Copied of (CommonTypes.ComponentType * XYPos * string) list
    | Uninitialized
    
type Model = {
    Wire: BusWire.Model
    Symbol: Symbol.Model
    DragState: DragState
    Selection: SelectionState
    CopyState: CopyState
    MousePosition: XYPos
    PanX: float
    PanY: float
    Zoom: float
    Width: float
    Height: float
    UndoList: SubModel list
    RedoList: SubModel list
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
    | SaveState of SubModel
    | Wire of BusWire.Msg
    | Symbol of Symbol.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT * Modifier

/// Constants that will be turned into settings at a later date
let gridSize = 10
let undoHistorySize = 50
let portHighlightRange = 50.

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
        let pan =
            snapToGrid {
                X = model.PanX / model.Zoom
                Y = model.PanY / model.Zoom
            }

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
                createHalfGrid width (int pan.X) (fun x -> makeLine x -gridSize x height <| getColorAndOpacity x)
                createHalfGrid height (int pan.Y) (fun y -> makeLine -gridSize y width y <| getColorAndOpacity y)
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
    let deselectSymbolsCmd =
        Cmd.ofMsg (Symbol (Symbol.SetSelected []))

    let highlightPortsNearCmd p =
        let currentType = 
                match model.DragState with
                | WireCreation (pId, _) -> Some <| Symbol.portType model.Symbol pId
                | _ -> None
        let portsNearMouse =
            Symbol.portsInRange model.Symbol p portHighlightRange
            |> List.filter (fun pId ->
                match currentType with
                | Some pType -> pType <> Symbol.portType model.Symbol pId
                | None -> true
            )
        Cmd.ofMsg (Symbol (Symbol.HighlightPorts portsNearMouse))

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
        | DragState.Wire (didDrag, prevSubModel) ->
            newModel, Cmd.batch [
                Cmd.ofMsg (Wire (BusWire.EndDrag))
                saveStateIfDraggedCmd didDrag prevSubModel
            ]
        | DragState.Symbol (didDrag, prevSubModel) ->
            newModel, Cmd.batch [
                Cmd.ofMsg (Symbol (Symbol.EndDragging))
                saveStateIfDraggedCmd didDrag prevSubModel
            ]
        | DragState.WireCreation _ ->
            { newModel with Selection = Empty }
            , Cmd.none
        | DragState.Pan (origPan, panStart, panEnd) ->
            updatePan model origPan panStart panEnd
            , Cmd.none
        | NotDragging -> newModel, Cmd.none

    let handleLeftClick model p m =
        let targetedPort = Symbol.getTargetedPort model.Symbol p
        let selectedWire = BusWire.getTargetedWire model.Wire p
        let selectedSymbol = Symbol.getTargetedSymbol model.Symbol p

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
                DragState = DragState.Symbol (false, (model.Wire, model.Symbol))
            }, Cmd.batch [
                Cmd.ofMsg (Symbol (Symbol.SetSelected selectedSymbols))
                Cmd.ofMsg (Symbol (Symbol.StartDragging (selectedSymbols, snapToGrid p)))
                Cmd.ofMsg (Wire (BusWire.UnselectAll))
            ]
        | (None, None, Some wId, _) ->
            { model with
                Selection = SelectionState.Wire wId
                DragState = DragState.Wire (false, (model.Wire, model.Symbol))
            }, Cmd.batch [
                deselectSymbolsCmd
                Cmd.ofMsg (Wire (BusWire.SetSelected wId))
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
                        Cmd.ofMsg (Symbol (Symbol.SetSelected sIdLst))
                        Cmd.ofMsg (Wire (BusWire.UnselectAll))
                    ]
                | SelectionState.Wire wId ->
                    Cmd.batch [
                        deselectSymbolsCmd
                        Cmd.ofMsg (Wire (BusWire.SetSelected wId))
                    ]
                | SelectionState.Empty ->
                    Cmd.batch [
                        deselectSymbolsCmd
                        Cmd.ofMsg (Wire (BusWire.UnselectAll))
                    ]
            ]

        match key with
        | AltA ->
            let selectedSymbols = Symbol.getAllSymbols model.Symbol

            { model with Selection = Symbols selectedSymbols }
            , Cmd.ofMsg (Symbol (Symbol.SetSelected selectedSymbols))
        | Escape ->
            match model.DragState with
            | NotDragging ->
                { model with Selection = Empty }
                , Cmd.batch [
                    deselectSymbolsCmd
                    Cmd.ofMsg (Wire (BusWire.UnselectAll))
                ]
            | DragState.Symbol (_, (prevWireModel, prevSymbolModel)) ->
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
            { model with Selection = Empty;DragState=NotDragging }
            , match model.Selection with
              | SelectionState.Wire wId ->
                  Cmd.batch [
                      Cmd.ofMsg (Wire (BusWire.DeleteWire wId))
                      Cmd.ofMsg <| SaveState (model.Wire, model.Symbol)
                  ]
              | Symbols sIdLst ->
                  Cmd.batch [
                      Cmd.ofMsg (Wire (BusWire.DeleteSymbols sIdLst))
                      Cmd.ofMsg (Symbol (Symbol.DeleteSymbols sIdLst))
                      Cmd.ofMsg <| SaveState (model.Wire, model.Symbol)
                  ]
              | Empty -> Cmd.none
        | AltC ->
            match model.Selection with
            | Symbols sIdLst ->
                let sCopyDataLst = 
                    sIdLst
                    |> List.map (fun sId ->
                        (
                            Symbol.symbolType model.Symbol sId,
                            (Symbol.symbolBBox model.Symbol sId).Pos,
                            (Symbol.symbolLabel model.Symbol sId) + "_copy"
                        )
                    )

                let topLeftOfSymbols =
                    let middle lst = List.min lst + (List.max lst - List.min lst) / 2.

                    sCopyDataLst
                    |> List.map (fun (_, p, _) -> (p.X, p.Y))
                    |> List.fold (fun (xs, ys) (x, y) -> (x :: xs, y :: ys)) ([], [])
                    |> fun (xs, ys) -> posOf (middle xs) (middle ys)
                    
                let sCopyDataLst = 
                    sCopyDataLst
                    |> List.map (fun (sId, p, l) -> (sId, posDiff p topLeftOfSymbols, l))

                { model with CopyState=CopyState.Copied sCopyDataLst }
            | _ -> model
            , Cmd.none
        | AltV ->
            model
            , match model.CopyState with
              | Copied sIdLst ->
                  Cmd.batch (
                      List.map (fun (sType, p, sLabel) ->
                          Cmd.ofMsg (Symbol (Symbol.AddSymbol (sType, snapToGrid (posAdd p model.MousePosition), sLabel)))
                      ) sIdLst
                      @ [Cmd.ofMsg (SaveState (model.Wire, model.Symbol))]
                  )
              | Uninitialized -> Cmd.none
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
                , highlightingAfterUndoAndRedoCmd model
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
                    Cmd.ofMsg (Symbol (Symbol.AddSymbol (CommonTypes.ComponentType.And, snapToGrid model.MousePosition, "and_01")))
                    Cmd.ofMsg (SaveState (model.Wire, model.Symbol))
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
                    , Cmd.batch [
                        Cmd.ofMsg (Symbol (Symbol.Dragging (selectedSymbols, snapToGrid p)))
                        Cmd.ofMsg (Wire (BusWire.DraggingSymbols selectedSymbols))
                    ]
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
                , Cmd.ofMsg (Symbol (Symbol.SetSelected selectedSymbols))
            | DragState.Wire (didDrag, prevWireModel) ->
                newModel, Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.EndDrag))
                    saveStateIfDraggedCmd didDrag prevWireModel
                ]
            | DragState.Symbol (didDrag, prevWireModel) ->
                newModel, Cmd.batch [
                    Cmd.ofMsg (Symbol (Symbol.EndDragging))
                    saveStateIfDraggedCmd didDrag prevWireModel
                ]
            | DragState.WireCreation (pIdStart, p) ->
                let targetedPort = Symbol.getTargetedPort model.Symbol p
                { newModel with Selection = Empty }
                , match targetedPort with
                  | Some pIdEnd when pIdEnd <> pIdStart ->
                      Cmd.batch [
                          Cmd.ofMsg (Wire (BusWire.AddWire (pIdStart, pIdEnd)))
                          Cmd.ofMsg (SaveState (model.Wire, model.Symbol))
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
                Cmd.ofMsg (Symbol (Symbol.HighlightPorts []))
            ]
        | (Move, p, _) ->
            { model with MousePosition=p}
            , highlightPortsNearCmd p

    match msg with
    | SaveState savedSubModel ->
        { model with
            UndoList=List.truncate undoHistorySize <| savedSubModel :: model.UndoList
            RedoList=[]
        }, Cmd.none
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
    let symbolSvg = Symbol.view model.Symbol sDispatch
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire model.Symbol wDispatch
    let symbolsAndWiresSvg =
        g [] [
            wireSvg
            symbolSvg
        ]

    displaySvgWithZoom model symbolsAndWiresSvg dispatch


let init () =
    let sModel, sCmds = Symbol.init ()
    let wModel, wCmds = (BusWire.init sModel) ()
    {
        Wire = wModel
        Symbol = sModel
        Selection = Empty
        DragState = NotDragging
        CopyState = Uninitialized
        MousePosition = posOf 0. 0.
        PanX = 0.
        PanY = 0.
        Zoom = 1.
        Width = 1000.
        Height = 500.
        UndoList = []
        RedoList = []
    }, Cmd.batch [
        Cmd.map Symbol sCmds
        Cmd.map Wire wCmds
    ]
