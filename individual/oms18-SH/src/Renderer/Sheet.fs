module Sheet

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers
open BBox

type Model = {
    Wire: BusWire.Model
    SelectedSymbols: CommonTypes.ComponentId list
    AreaSelectStart: (XYPos * XYPos) option
    AdditiveSelect: bool
}

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
let displaySvgWithZoom (model: Model) (zoom: float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =
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
                  { X = ev.clientX / zoom
                    Y = ev.clientY / zoom } },
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
                match model.AreaSelectStart with
                | Some (p1, p2) ->
                    let area = pointsToBBox p1 p2
                    let color =
                        if model.AdditiveSelect then
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
                | None -> svgReact
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
    | MouseMsg (mT, modifier) ->
        match (mT.Op, mT.Pos, modifier) with
        | (Down, p, m) ->
            let selectedSymbol =
                Symbol.getTargetedSymbol model.Wire.Symbol p

            match (selectedSymbol, m) with
            | (Some sym, m) ->
                let isSelected =
                    model.SelectedSymbols
                    |> Set.ofList
                    |> Set.contains sym.Id

                let selectedSymbols =
                    if isSelected then
                        model.SelectedSymbols
                    else
                        if m = Control then
                            model.SelectedSymbols
                            |> Set.ofList
                            |> Set.add sym.Id
                            |> Set.toList
                        else
                            [sym.Id]

                { model with SelectedSymbols = selectedSymbols}
                , Cmd.ofMsg (Wire(BusWire.Symbol(Symbol.Select selectedSymbols)))
            | (None, Control) ->
                { model with
                    AreaSelectStart=Some (p, p)
                    AdditiveSelect=true
                }, Cmd.none
            | (None, NoModifier) ->
                { model with
                    SelectedSymbols=[]
                    AreaSelectStart=Some (p, p)
                    AdditiveSelect=false
                }, Cmd.ofMsg (Wire(BusWire.Symbol(Symbol.Select [])))
        | (Drag, p, _) ->
            match model.AreaSelectStart with
            | Some (start, _) ->
                { model with AreaSelectStart=Some(start, p)}
                , Cmd.none
            | None -> model, Cmd.ofMsg (Wire(BusWire.Symbol(Symbol.Dragging p)))
        | (Up, _, _) ->
            match model.AreaSelectStart with
            | Some (p1, p2) ->
                let area = pointsToBBox p1 p2
                let selectedSymbols =
                    if model.AdditiveSelect then
                        model.SelectedSymbols
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
                    AreaSelectStart=None
                    SelectedSymbols=selectedSymbols
                }
                , Cmd.batch [
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.EndDragging)))
                    Cmd.ofMsg (Wire (BusWire.Symbol (Symbol.Select selectedSymbols)))
                ]
            | None ->
                {model with AreaSelectStart=None}
                , Cmd.ofMsg (Wire(BusWire.Symbol(Symbol.EndDragging)))
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
    {
        Wire = model
        SelectedSymbols = []
        AreaSelectStart=None
        AdditiveSelect=false
    }, Cmd.map Wire cmds
