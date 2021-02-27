module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type Model = {
    Wire: BusWire.Model
    SelectedSymbols: CommonTypes.ComponentId list
    MouseIsDown: bool
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseDown of XYPos
    | MouseMove of XYPos
    | MouseUp of XYPos
    | Symbol of Symbol.Msg

/// Determines top-level zoom, > 1 => magnify.
/// This should be moved into the model as state
let zoom = 1.0    



/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>)=
    
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ] 
        //   OnMouseDown (fun ev -> (mouseOp Down ev))
        //   OnMouseUp (fun ev -> (mouseOp Up ev))
        //   OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
          OnMouseDown (fun ev -> 
            MouseDown(posOf ev.pageX ev.pageY)
            |> dispatch
          )

          OnMouseMove (fun ev -> 
            MouseMove(posOf ev.pageX ev.pageY)
            |> dispatch
          )
          
          OnMouseUp (fun ev -> 
            MouseUp(posOf ev.pageX ev.pageY)
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
                [ 
                    text [ // a demo text svg element
                        X 500; 
                        Y 50; 
                        Style [
                            TextAnchor "middle" // horizontal algnment vs (X,Y)
                            DominantBaseline "middle" // vertical alignment vs (X,Y)
                            FontSize "40px"
                            FontWeight "Bold"
                            Fill "Green" // font color
                        ]
                    ] [str "sample text"]

                    svgReact // the application code

                    polygon [ // a demo svg polygon triangle written on top of the application
                        SVGAttr.Points "10,10 900,900 10,900"
                        SVGAttr.StrokeWidth "5px"
                        SVGAttr.Stroke "Black"
                        SVGAttr.FillOpacity 0.1
                        SVGAttr.Fill "Blue"] []
                ]
            ]
        ]



/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom zoom wireSvg dispatch
       

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
    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey
        printfn "Key:%A" c
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)
    | MouseDown pos ->
        let idlst = match Symbol.getTargetedSymbol model.Wire.Symbol pos with
                    | None -> []
                    | Some id -> [id]
        {model with MouseIsDown = true; SelectedSymbols = idlst},
        Cmd.batch[
            Cmd.ofMsg (Symbol <| Symbol.StartDragging (idlst, pos));
            Cmd.ofMsg (Symbol <| Symbol.SetSelected idlst);
        ]
    | MouseUp pos ->
        {model with MouseIsDown = false}, 
        Cmd.ofMsg (Symbol <| Symbol.EndDragging)
    | MouseMove pos ->
        if model.MouseIsDown then
            model, Cmd.ofMsg (Symbol <| Symbol.Dragging (model.SelectedSymbols, pos))
        else
            model, Cmd.none
    | _ -> failwithf "Sheet - message not implemented"

let init() = 
    let model,cmds = (BusWire.init 400)()
    {
        Wire = model
        SelectedSymbols = []
        MouseIsDown = false
    }, Cmd.map Wire cmds
