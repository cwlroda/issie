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
    | NotDragging

type SelectionState =
    | Wire of CommonTypes.ConnectionId
    | Symbols of CommonTypes.ComponentId list
    | Empty

type Model = {
    Wire: BusWire.Model
    DragState: DragState
    Selection: SelectionState
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

type Modifier =
    | Control
    | NoModifier

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT * Modifier

/// Determines top-level zoom, > 1 => magnify.
/// This should be moved into the model as state
let zoom = 1.0

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (model: Model) (zoom: float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))
    /// Is the mouse button currently down?
    let mDown (ev: Types.MouseEvent) = ev.buttons <> 0.
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev: Types.MouseEvent) =
        dispatch
        <| MouseMsg(
            { Op = op
              Pos =
                  { X = (-3. + ev.clientX) / zoom
                    Y = (-3. + ev.clientY) / zoom } },
            if ev.ctrlKey then
                Control
            else
                NoModifier
        )

    div [ Style [ Height "100vh"
                  MaxWidth "100vw"
                  CSSProp.OverflowX OverflowOptions.Auto
                  CSSProp.OverflowY OverflowOptions.Auto ]
          OnMouseDown(fun ev -> (mouseOp Down ev))
          OnMouseUp(fun ev -> (mouseOp Up ev))
          OnMouseMove(fun ev -> mouseOp (if mDown ev then Drag else Move) ev) ] [
        svg [ Style [ Border "3px solid green"
                      Height sizeInPixels
                      Width sizeInPixels ] ] [
            g [ Style [ Transform(sprintf "scale(%f)" zoom) ] ] [  // top-level transform style attribute for zoom
                match model.DragState with
                | AreaSelect (p1, p2, additive) ->
                    let area = pointsToBBox p1 p2
                    let color =
                        if additive then
                            "Green"
                        else
                            "Blue"

                    svgReact // the application code
                    rect [
                        X area.Pos.X
                        Y area.Pos.Y
                        SVGAttr.Width area.Bounds.Width
                        SVGAttr.Height area.Bounds.Height
                        SVGAttr.Fill color
                        SVGAttr.Stroke color
                        SVGAttr.StrokeWidth "1px"
                        SVGAttr.FillOpacity 0.3
                    ] []
                | _ -> svgReact
            ]
        ] // top-level transform style attribute for zoom
    ] // top-level transform style attribute for zoom



/// for the demo code
let view (model: Model) (dispatch: Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom model zoom wireSvg dispatch


let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Wire wMsg ->
        let wModel, wCmd = BusWire.update wMsg model.Wire
        { model with Wire = wModel }, Cmd.map Wire wCmd
    | MouseMsg (mT, modifier) ->
        match (mT.Op, mT.Pos, modifier) with
        | (Down, p, m) ->
            let selectedWire = BusWire.getTargetedWire model.Wire p
            let selectedSymbol =
                Symbol.getTargetedSymbol model.Wire.Symbol p

            match (selectedSymbol, selectedWire, m) with
            | (Some sym, _, m) ->
                let isSelected =
                    match model.Selection with
                    | Symbols sIdLst ->
                        sIdLst
                        |> Set.ofList
                        |> Set.contains sym.Id
                    | _ -> false

                let selectedSymbols =
                    if isSelected then
                        match model.Selection with
                        | Symbols sIdLst -> sIdLst
                        | _ -> [sym.Id]
                    else if m = Control then
                        match model.Selection with
                        | Symbols sIdLst ->
                            sIdLst
                            |> Set.ofList
                            |> Set.add sym.Id
                            |> Set.toList
                        | _ -> [sym.Id]
                    else
                        [sym.Id]

                { model with
                    Selection = Symbols selectedSymbols
                    DragState=DragState.Symbol
                }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select selectedSymbols)))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.StartDragging (selectedSymbols, p))))
                    Cmd.ofMsg (Wire (BusWire.UnselectAll))
                ]
            | (_, Some wId, _) ->
                { model with
                    Selection = SelectionState.Wire wId
                    DragState = DragState.Wire wId
                }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                    Cmd.ofMsg (Wire (BusWire.Select wId))
                    Cmd.ofMsg (Wire (BusWire.StartDrag (wId, p)))
                ]
            | (None, None, Control) ->
                { model with
                    DragState=AreaSelect (p, p, true)
                }, Cmd.ofMsg (Wire (BusWire.UnselectAll))
            | (None, None, NoModifier) ->
                { model with
                    Selection = Empty
                    DragState = AreaSelect (p, p, false)
                }, Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select [])))
                    Cmd.ofMsg (Wire (BusWire.UnselectAll))
                ]
        | (Drag, p, _) ->
            // TODO: Send StartDrag
            match model.DragState with
            | AreaSelect (start, _, additive) ->
                { model with DragState=AreaSelect (start, p, additive)}
                , Cmd.none
            | DragState.Symbol ->
                let selectedSymbols = match model.Selection with
                                      | Symbols s -> s
                                      | _ -> failwithf "We only drag if there is a selection"
                model,
                Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Dragging (selectedSymbols, p))))
            | DragState.Wire wId ->
                model,
                Cmd.ofMsg (Wire (BusWire.Dragging (wId, p)))
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
                model, Cmd.ofMsg (Wire (BusWire.EndDrag))
            | DragState.Symbol ->
                model, Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
            | _ -> model, Cmd.none
        | _ -> model, Cmd.none
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
                Cmd.ofMsg (Wire (BusWire.UnselectAll))
            ]
        | _ ->
            {model with DragState=NotDragging}
            , Cmd.none
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
    let model, cmds = (BusWire.init 500) ()
    {
        Wire = model
        Selection = Empty
        DragState = NotDragging
    }, Cmd.map Wire cmds
