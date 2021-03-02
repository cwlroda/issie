module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type Model = {
    Wire: BusWire.Model
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg

/// Determines top-level zoom, > 1 => magnify.
/// This should be moved into the model as state
let zoom = 1.0

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (model: Model) (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev:Types.MouseEvent) =
        dispatch <| Wire (BusWire.MouseMsg {Op = op ; Pos = { X = ev.clientX / zoom ; Y = ev.clientY / zoom}})
        match op with
        | Down ->  
            match BusWire.getTargetedWire model.Wire {X =  ev.clientX / zoom; Y = ev.clientY / zoom} with
            | Some w -> 
                dispatch <| Wire (BusWire.SetSelected w) 
                dispatch <| Wire (BusWire.StartDragging (w,  {X =  ev.clientX / zoom; Y = ev.clientY / zoom}))
            | None ->   
                match Symbol.getTargetedSymbol model.Wire.Symbol {X =  ev.clientX / zoom; Y = ev.clientY / zoom} with
                | Some s ->  dispatch <| Wire (BusWire.Symbol (Symbol.StartDragging (s.Id, {X =  ev.clientX / zoom; Y = ev.clientY / zoom})))
                | None -> () 
                
            
        | Drag ->
            match BusWire.getTargetedWire model.Wire {X =  ev.clientX / zoom; Y = ev.clientY / zoom} with
            | Some w -> 
                dispatch <| Wire (BusWire.Dragging (w,  {X =  ev.clientX / zoom; Y = ev.clientY / zoom}))
            | None -> 
                match Symbol.getTargetedSymbol model.Wire.Symbol {X =  ev.clientX / zoom; Y = ev.clientY / zoom} with
                | Some s ->  dispatch <| Wire (BusWire.Symbol ( Symbol.Dragging (s.Id, {X =  ev.clientX / zoom; Y = ev.clientY / zoom})))
                | None -> ()

        | Move -> ()  
        | Up ->
            dispatch <| Wire (BusWire.EndDragging)
           (*  match Symbol.getTargetedSymbol model.Wire.Symbol {X =  ev.clientX / zoom; Y = ev.clientY / zoom} with
            | Some s ->  dispatch <| Wire (BusWire.Symbol ( Symbol.EndDragging s.Id))
            | None -> ()
 *)

        
        
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ] 
          OnMouseDown (fun ev -> (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
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
                   

                    svgReact // the application code

                    (* polygon [ // a demo svg polygon triangle written on top of the application
                        SVGAttr.Points "10,10 900,900 10,900"
                        SVGAttr.StrokeWidth "5px"
                        SVGAttr.Stroke "Black"
                        SVGAttr.FillOpacity 0.1
                        SVGAttr.Fill "Blue"] [] *)
                ]
            ]
        ]



/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom model zoom wireSvg dispatch
       
let makeRandomPortConnection (symbols)=
    let symIds =
        List.map (fun (sym: Symbol.Symbol) -> sym.Id) symbols

    let rng = System.Random 0
    let n = symIds.Length

    let s1, s2 =
        match rng.Next(0, n - 1), rng.Next(0, n - 2) with
        | r1, r2 when r1 = r2 -> symbols.[r1], symbols.[n - 1]

        | r1, r2 -> symbols.[r1], symbols.[r2]
    let port1N = s1.Ports.Length
    let port2N = s1.Ports.Length
    let (p1, p2) =
        let p1 =
            s1.Ports.[rng.Next(0,port1N-1)]
            

        let p2 =
            s2.Ports.[rng.Next(0,port2N-1)]
        (p1.Id, p2.Id)

    (p1, p2)


let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress CtrlS ->
        let wModel, wCmd = BusWire.update BusWire.UnselectAll model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    | KeyPress AltZ -> 
        let wModel, wCmd = BusWire.update BusWire.EndDragging model.Wire    
        {model with Wire = wModel}, Cmd.map Wire wCmd
    | KeyPress DEL ->
        let wId = 
            Map.toList model.Wire.WX 
            |> List.head
            |> fst
        let wModel, wCmd = BusWire.update (BusWire.DeleteWire wId) model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    | KeyPress AltC -> 
        let pIdPair = makeRandomPortConnection model.Wire.Symbol
        let wModel, wCmd = BusWire.update (BusWire.AddWire pIdPair) model.Wire
        let updatedModel = {model with Wire = wModel}
        let allErr = BusWire.getErrors updatedModel.Wire
        printf $"{allErr}"
        updatedModel, Cmd.map Wire wCmd
    | KeyPress AltV -> 
        let wModel, wCmd = BusWire.update (BusWire.ToggleAnnotations) model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    
let init() = 
    let model,cmds = (BusWire.init 400)()
    {
        Wire = model
    }, Cmd.map Wire cmds
    