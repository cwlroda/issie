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


type Model = {
    Wire: BusWire.Model
    SelectedSymbols: CommonTypes.ComponentId list
    MouseIsDown: bool
    SelectionBox: SelectionBox
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL | CtrlA

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseDown of XYPos * bool
    | MouseMove of XYPos
    | MouseUp of XYPos * bool
    | Symbol of Symbol.Msg

/// Determines top-level zoom, > 1 => magnify.
/// This should be moved into the model as state
let zoom = 1.0    



/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (selectionBox: SelectionBox) (dispatch: Dispatch<Msg>)=
    
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false

    let bodyLst = [svgReact]

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

    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ] 
          OnMouseDown (fun ev -> 
            MouseDown((posOf ev.pageX ev.pageY), ev.shiftKey)
            |> dispatch
          )

          OnMouseMove (fun ev -> 
            MouseMove(posOf ev.pageX ev.pageY)
            |> dispatch
          )
          
          OnMouseUp (fun ev -> 
            MouseUp((posOf ev.pageX ev.pageY), ev.shiftKey)
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
                [ Style [Transform (sprintf "scale(%f)" zoom)]] // top-level transform style attribute for zoom
                bodyLst
            ]
        ]



/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    let selectionBox = model.SelectionBox
    displaySvgWithZoom zoom wireSvg selectionBox dispatch
       

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    | Symbol sMsg ->
        let sModel, sCmd = Symbol.update sMsg model.Wire.Symbol
        let wModel = {model.Wire with Symbol = sModel}
        {model with Wire = wModel}, Cmd.map Symbol sCmd
    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress CtrlA ->
        let idLst = Symbol.getAllSymbols model.Wire.Symbol
        {model with SelectedSymbols = idLst},
        Cmd.ofMsg (Symbol <| Symbol.SetSelected idLst)
    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey
        printfn "Key:%A" c
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)
    | MouseDown (pos, isShift) ->
        let idLst, onObj = match Symbol.getTargetedSymbol model.Wire.Symbol pos with
                        | Some targetedId ->
                            (if List.contains targetedId model.SelectedSymbols then
                                if isShift then
                                    List.filter (fun el -> el <> targetedId) model.SelectedSymbols
                                else
                                    model.SelectedSymbols
                            else
                                if isShift then List.append model.SelectedSymbols [targetedId]
                                else [targetedId]
                            ),
                            true
                        | None -> [], false
        
        {model with
            MouseIsDown = true;
            SelectedSymbols = idLst;
            SelectionBox = {
                model.SelectionBox with
                    FixedCorner = pos
                    MovingCorner = pos
                    Show = not onObj
            }
        },
        Cmd.batch[
            Cmd.ofMsg (Symbol <| Symbol.StartDragging (idLst, pos));
            Cmd.ofMsg (Symbol <| Symbol.SetSelected idLst);
        ]
    | MouseUp (pos, isShift) ->
        let symbolsInSelectionBox =
            let c1 = model.SelectionBox.FixedCorner
            let c2 = model.SelectionBox.MovingCorner
            let selectionBBox = 
                let x =  if c1.X < c2.X then c1.X else c2.X
                let y = if c1.Y < c2.Y then c1.Y else c2.Y
                let h = if c1.Y - c2.Y > 0. then c1.Y - c2.Y else c2.Y - c1.Y
                let w = if c1.X - c2.X > 0. then c1.X - c2.X else c2.X - c1.X
                {
                    XYPos = posOf x y
                    Height = h
                    Width = w
                }
            Symbol.getSymbolsInTargetArea model.Wire.Symbol selectionBBox

        let selectedSymbols = 
            if List.length symbolsInSelectionBox > 1 then symbolsInSelectionBox
            else model.SelectedSymbols

        {
            model with
                SelectedSymbols = selectedSymbols
                MouseIsDown = false
                SelectionBox = {
                    model.SelectionBox with
                        Show = false
                }
        }, 
        Cmd.batch[Cmd.ofMsg (Symbol <| Symbol.EndDragging);Cmd.ofMsg (Symbol <| Symbol.SetSelected selectedSymbols)]
    | MouseMove pos ->
        if model.MouseIsDown then
            {
                model with
                    SelectionBox = {
                        model.SelectionBox with
                            MovingCorner = pos
                    }
            },
            Cmd.ofMsg (Symbol <| Symbol.Dragging (model.SelectedSymbols, pos))
        else
            model, Cmd.none
    | _ -> failwithf "Sheet - message not implemented"

let init() = 
    let model,cmds = (BusWire.init 400)()
    {
        Wire = model
        SelectedSymbols = []
        MouseIsDown = false
        SelectionBox = {
            FixedCorner = posOf 0.0 0.0
            MovingCorner = posOf 0.0 0.0
            Show = false
        }
    }, Cmd.map Wire cmds
