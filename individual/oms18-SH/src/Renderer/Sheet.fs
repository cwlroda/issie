module Sheet

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type Model = { Wire: BusWire.Model }

type KeyboardMsg =
    | CtrlS
    | AltC
    | AltV
    | AltZ
    | AltShiftZ
    | DEL

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
let displaySvgWithZoom (zoom: float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))
    /// Is the mouse button currently down?
    let mDown (ev: Types.MouseEvent) = ev.buttons <> 0.
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev: Types.MouseEvent) =
        // TODO: pageX is offset by the border....
        dispatch
        <| MouseMsg(
            { Op = op
              Pos =
                  { X = -3. + ev.pageX / zoom
                    Y = -3. + ev.pageY / zoom } },
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
                svgReact // the application code
            ]
        ] // top-level transform style attribute for zoom
    ] // top-level transform style attribute for zoom



/// for the demo code
let view (model: Model) (dispatch: Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom zoom wireSvg dispatch


let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | MouseMsg (mT, modifier) ->
        match (mT.Op, mT.Pos, modifier) with
        | (Down, p, m) ->
            let selectedSymbol =
                Symbol.getTargetedSymbol model.Wire.Symbol p

            match (selectedSymbol, m) with
            | (Some sym, Control) ->  model, Cmd.ofMsg (Wire(BusWire.Symbol(Symbol.ToggleSelect sym.Id)))
            | (Some sym, NoModifier) ->
                if sym.IsSelected then
                    model, Cmd.ofMsg (Wire(BusWire.Symbol(Symbol.Select sym.Id)))
                else
                    model, Cmd.ofMsg (Wire(BusWire.Symbol(Symbol.SelectOnly sym.Id)))
            | (None, Control) -> model, Cmd.none
            | (None, NoModifier) -> model, Cmd.ofMsg (Wire(BusWire.Symbol(Symbol.DeselectAll)))
        | (Drag, p, _) -> model, Cmd.ofMsg (Wire(BusWire.Symbol(Symbol.Dragging p)))
        | (Up, _, _) -> model, Cmd.ofMsg (Wire(BusWire.Symbol(Symbol.EndDragging)))
        | _ -> model, Cmd.none
    | Wire wMsg ->
        let wModel, wCmd = BusWire.update wMsg model.Wire
        { model with Wire = wModel }, Cmd.map Wire wCmd
    | KeyPress AltShiftZ ->
        printStats () // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey

        printfn "Key:%A" c
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)

let init () =
    let model, cmds = (BusWire.init 400) ()
    { Wire = model }, Cmd.map Wire cmds
