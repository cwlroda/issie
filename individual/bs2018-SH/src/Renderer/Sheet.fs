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
    | MouseDown of XYPos * bool
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
          OnMouseDown (fun ev -> 
            MouseDown((posOf ev.pageX ev.pageY), ev.shiftKey)
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
                    

                    svgReact // the application code
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
    | MouseDown (pos, isShift) ->
        let idLst = match Symbol.getTargetedSymbol model.Wire.Symbol pos with
                        | Some targetedId ->
                            if List.contains targetedId model.SelectedSymbols then
                                if isShift then
                                    List.filter (fun el -> el <> targetedId) model.SelectedSymbols
                                else
                                    model.SelectedSymbols
                            else
                                if isShift then List.append model.SelectedSymbols [targetedId]
                                else [targetedId]
                        | None -> []
        
        {model with MouseIsDown = true; SelectedSymbols = idLst},
        Cmd.batch[
            Cmd.ofMsg (Symbol <| Symbol.StartDragging (idLst, pos));
            Cmd.ofMsg (Symbol <| Symbol.SetSelected idLst);
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
