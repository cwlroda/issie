module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open CommonTypes
open Helpers
//open Symbol
type Model = {
    Wire: BusWire.Model
  //  SymbolId: string
    
    }

type BBox = {
    Pos : XYPos
    Width: float
    Height: float
}

type PortPos =
    {
        X: float
        Y: float
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ |  CtrlZ |CtrlD |CtrlShiftS | CtrlP | CtrlC | CtrlShiftC

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg

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
    let mouseOp op (ev:Types.MouseEvent) = 
        dispatch <| Wire (BusWire.MouseMsg {Op = op ; Pos = { X = ev.clientX / zoom ; Y = ev.clientY / zoom}})
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ] 
          OnMouseDown (fun ev -> 
            
            (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
        ]
        [ svg
            [ Style 
                [
                    Border "3px solid blue"
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
                            Fill "Blue" // font color
                        ]
                    ] [str "DEMO"]

                    svgReact // the application code

                    // polygon [ // a demo svg polygon triangle written on top of the application
                    //     SVGAttr.Points "10,10 900,900 10,900"
                    //     SVGAttr.StrokeWidth "5px"
                    //     SVGAttr.Stroke "Black"
                    //     SVGAttr.FillOpacity 0.1
                    //     SVGAttr.Fill "Blue"] []
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
    | KeyPress AltShiftZ -> 
  //  | KeyPress CtrlS ->
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged

    // | KeyPress CtrlA ->
    //     let idLst = Symbol.getAllSymbols model.Symbol
    //     {model with SelectedSymbols = idLst},
    //     Cmd.ofMsg (Symbol <| Symbol.SetSelected idLst)
    |  KeyPress CtrlD -> //this should get and delete the value I just created with CtrlZ
        let symbolMessage =
            let id = Symbol.getTargetedSymbol (model.Wire.Symbol) ({X=10. ;Y=10.}) //works well for both options  
            match id with 
            | Some a -> Symbol.DeleteSymbolSingle a
            | None -> Symbol.DoNothing
        model, Cmd.ofMsg (Wire <| BusWire.Symbol (symbolMessage) ) 

    | KeyPress CtrlZ -> //works 
        model, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.AddSymbol ({X=0.;Y=0.},And))) 

    | KeyPress CtrlS -> //works
        let getIdList = Symbol.getSymbolsInTargetArea (model.Wire.Symbol) ({Pos = {X =0.;Y=0.}; Width = 700. ;Height = 200. })
        model, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.SetSelected getIdList))
    
    |KeyPress CtrlShiftS -> //works
        model, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.SetSelected []))

    | KeyPress CtrlC ->
// ------------------------------works: highlights ports of target symbol----------------------------------
        let symbolId = Symbol.getTargetedSymbol (model.Wire.Symbol) {X=60.;Y= 110.}
        let message =
            match symbolId with 
                | Some a -> Symbol.HighlightPorts (Symbol.getPortsOfSymbol (model.Wire.Symbol) a)
                | None -> Symbol.DoNothing
      
        model, Cmd.ofMsg (Wire <| BusWire.Symbol (message))

    | KeyPress CtrlShiftC -> //works 
        model, Cmd.ofMsg (Wire <| BusWire.Symbol Symbol.UnhighlightPorts)

    | KeyPress CtrlP ->  
//------------------------------works: highlights ports of target symbol----------------------------------
        // let symbolId = Symbol.getTargetedSymbol (model.Wire.Symbol) {X=10.;Y= 10.}
        // let message =
        //     match symbolId with 
        //         | Some a -> Symbol.HighlightPorts (Symbol.getPortsOfSymbol (model.Wire.Symbol) a)
        //         | None -> Symbol.DoNothing
      
        // model, Cmd.ofMsg (Wire <| BusWire.Symbol (message))

//----------------------------works: gets all symbols and deletes them------------------------------------------------------------
        
        // let Idlist =Symbol.getAllSymbols (model.Wire.Symbol)
        // model, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.DeleteSymbols Idlist))
        

//-------------------------------------------------------------------------------------------------------------

//----------------------------works: Just to test that the port related interfaces work ------------------------------------------------
        let idList = Symbol.getTargetedPortDemo (model.Wire.Symbol) {X = 25.; Y=25.}
        let portId = List.item 1 idList
        let getportType pid= Symbol.portType (model.Wire.Symbol) pid
        let getPortPos pid = Symbol.portPos (model.Wire.Symbol) pid
        let TargetPortId =  Symbol.getTargetedPort (model.Wire.Symbol) (getPortPos portId)
        let Hostid = Symbol.getHostId (model.Wire.Symbol) portId
        let symbolId = Symbol.getTargetedSymbol (model.Wire.Symbol) {X=60.;Y= 110.}
        printfn "This is the port ID I see: %A" idList
        printfn "This is the port ID I see: %A" portId
        printfn " This is the returned by getTargetPort: %A " <| TargetPortId
        printfn "This is the port position I get: %A" <| getPortPos portId
        printfn "This is the port type I get: %A" <| getportType portId
        printfn "This is the port width I get: %A" <| Symbol.portWidth (model.Wire.Symbol) portId
        printfn "list of ports in range: %A" <| Symbol.portsInRange (model.Wire.Symbol) {X = 50.; Y=100.} 100. 
        printfn "This is the hostID of a component with a particular port: %A" <| Hostid
        printfn "This is the type of that component: %A" <| Symbol.symbolType (model.Wire.Symbol) Hostid
        match symbolId with 
                | Some a -> printfn "This is the symbol bounding box I get: %A" <|  Symbol.symbolBBox (model.Wire.Symbol) a
                | None -> printfn "No symbol "
        
        model,Cmd.none
     

    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey
        printfn "Key:%A" c
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)




let init() = 
    let model,cmds = (BusWire.init 0)() //I changed it from 400 to 0 
    {
        Wire = model
     
    }, Cmd.map Wire cmds
