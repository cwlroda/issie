module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//


/// Model to generate one symbol (skeleton). Id is a unique Id 
/// for the symbol shared with Issie Component type.
/// The real type will obviously be much larger.
/// Complex information that never changes (other than Id) should 
/// probably not be here, but looked up via some function
/// from a more compact form, so that comparison of two Symbols to
/// determine are they the same is fast.
type Symbol =
    {
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        Ports: CommonTypes.Port list
        W: int
        H: int   
    }


type Model = Symbol list


//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    /// coords not adjusted for top-level zoom
    | StartDragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    | Dragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | EndDragging of sId : CommonTypes.ComponentId
    | AddCircle of pos:XYPos * label : string * nrPort : (int*int)// used by demo code to add a circle
    | DeleteSymbol of sId:CommonTypes.ComponentId 
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface


//---------------------------------helper types and functions----------------//



let boxDef (pos:XYPos) (w:int) (h:int): string =
    let tL = pos
    let tR = posOf (pos.X+(float w)) (pos.Y)
    let bR = posOf (pos.X+(float w )) (pos.Y - (float h))
    let bL = posOf (pos.X) (pos.Y - (float h))
    $"{bL.X},{bL.Y} {tL.X},{tL.Y} {tR.X},{tR.Y} {bR.X},{bR.Y}"


//-----------------------------Skeleton Model Type for symbols----------------//

//-----------------------Skeleton Message type for symbols---------------------//
let createPorts (sId: CommonTypes.ComponentId) ((nInP, nOutP):int *int) =
    let rnd = System.Random 0
    let createPort inOut nr =
        let pId  = 
            function
                | CommonTypes.PortType.Input -> (CommonTypes.InputPortId (Helpers.uuid())).ToString()
                | CommonTypes.PortType.Output -> (CommonTypes.OutputPortId (Helpers.uuid())).ToString()
        {
            CommonTypes.Port.Id = (pId inOut)
            CommonTypes.Port.PortNumber = Some nr
            CommonTypes.Port.PortType = inOut
            CommonTypes.Port.HostId = sId.ToString()
            CommonTypes.Port.PortWidth = Some (rnd.Next(1,3))
        }

    let inPortLst = List.map (createPort CommonTypes.PortType.Input) [0..nInP-1]
    let outPortLst = List.map (createPort CommonTypes.PortType.Output) [0..nOutP-1]
    inPortLst@outPortLst

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (pos:XYPos)  (label: string)  (nInP, nOutP)  =
    let sId =  CommonTypes.ComponentId (Helpers.uuid())
    let lenLabel = Seq.length label
    let portLst = createPorts sId (nInP,nOutP)
    let nrPorts = List.length portLst
    {   
        Pos = pos
        LastDragPos = {X=0.; Y=0.}
        IsDragging = false
        Id =sId
        Ports = portLst
        W = 60
        H = 10*(nrPorts + 2)   
    }

let portOpt (pId: CommonTypes.PortId) (pLst: CommonTypes.Port list) = 
    List.tryFind (fun (p:CommonTypes.Port) -> p.Id = pId) pLst  

let portType (model: Model) (portId: CommonTypes.PortId) : CommonTypes.PortType =
    match List.tryPick (fun sym -> (portOpt portId) sym.Ports) model with
    | Some p -> p.PortType
    | None -> failwithf "Invalid id"

let portWidth (model: Model) (portId: CommonTypes.PortId) : int Option =
    match List.tryPick (fun sym -> (portOpt portId) sym.Ports) model with
    | Some port -> port.PortWidth
    | None -> failwithf "Invalid id"

let portPos (model: Model) (pId: CommonTypes.PortId) : XYPos = 
    let getPos (pType) (symId) (pNr) =
        let sym = List.find (fun sym -> sym.Id.ToString() = symId) model
        match pNr with
        | Some nr when pType = CommonTypes.PortType.Input -> posAdd sym.Pos (posOf 0.0 (-10.*float (nr+2)))
        | Some nr -> posAdd sym.Pos (posOf (float sym.W) (-10.*float (nr+2)))
        | None -> failwithf "Has no possition"

    match List.tryPick (fun sym -> (portOpt pId) sym.Ports) model with
    | Some p -> getPos p.PortType p.HostId p.PortNumber
    | None -> failwithf "Invalid Id"
    
/// Dummy function for test. The real init would probably have no symbols.
let init () =
    List.allPairs [1..2] [1..2]
    |> List.map (fun (x,y) -> ({X = float (x*100+30); Y=float (y*100+30)}, x,y))
    |> List.mapi (fun i (pos, x, y) ->  (createNewSymbol  pos $"Test {i}"  (x,y)))
    , Cmd.none
    

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddCircle (pos, label, ports)   -> 
        createNewSymbol pos label ports :: model, Cmd.none
    | DeleteSymbol sId -> 
        List.filter (fun sym -> sym.Id <> sId) model, Cmd.none
    | StartDragging (sId, pagePos) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                { sym with
                    LastDragPos = pagePos
                    IsDragging = true
                }
        )
        , Cmd.none

    | Dragging (rank, pagePos) ->
        model
        |> List.map (fun sym ->
            if rank <> sym.Id then
                sym
            else
                let diff = posDiff pagePos sym.LastDragPos
                printf $"symPorts: {sym.Ports}"
                { sym with
                    Pos = posAdd sym.Pos diff
                    LastDragPos = pagePos
                }
                
        )
        , Cmd.none

    | EndDragging sId ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then 
                sym
            else
                { sym with
                    IsDragging = false 
                }
        )
        , Cmd.none
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags
    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderSymbolProps =
    {
        sym: Symbol
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }
type private RenderPortProps = 
    {
        Key: string
        Pos: XYPos
    }
let singleBox sym =
    let color =
        if sym.IsDragging then
            "lightblue"
        else
            "grey"
    polygon
        [ 
            SVGAttr.Points (boxDef sym.Pos sym.W sym.H)
            SVGAttr.StrokeWidth "1px"
            SVGAttr.Stroke "Black"
            SVGAttr.FillOpacity 0.5
            SVGAttr.Fill "Grey"
            
        ][]

/// View for one symbol with caching for efficient execution when input does not change
let private renderSymbol =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            singleBox props.sym) 

let private renderPort =
    FunctionComponent.Of (
            fun (props: RenderPortProps) -> 
            circle [
                    Cx props.Pos.X
                    Cy props.Pos.Y
                    R 2.
                    SVGAttr.Fill "Black"
                    SVGAttr.Stroke "Black"
                    SVGAttr.StrokeWidth 1
                
            ] []
    )

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    let sym = 
        model
        |> List.map (fun ({Id = CommonTypes.ComponentId id} as s) ->
            renderSymbol 
                {
                    sym = s
                    Dispatch = dispatch
                    key = id
                }
        )
        |> ofList
    let ports = 
        List.fold (fun lst sym -> sym.Ports@lst) [] model
        |> List.map (fun port -> 
            let props =  
                {
                    Key = port.Id
                    Pos = (portPos model port.Id)
                }
            renderPort props
        )

    g [] [
        (g [] ports)
        sym
    ]

//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)

let symbolBBox (symModel: Model) (symId: CommonTypes.ComponentId): BBox =  
    let sym = List.find (fun sym ->  sym.Id = symId) symModel
    (makeBBox sym.Pos (float sym.W)  (float sym.H))
    
let getTargetedSymbol (symModel: Model) (pos: XYPos) =
    List.tryFind (fun (sym: Symbol) ->  ptInBB pos (makeBBox sym.Pos  (float sym.W) (float sym.H))) symModel
    
let getPortsOfSymbol (symModel: Model) (symId: CommonTypes.ComponentId) =
    let sym = List.find (fun sym -> sym.Id = symId) symModel
    sym.Ports |> List.map (fun p -> p.Id)
     
let getHostId (model: Model) (portId: CommonTypes.PortId) : CommonTypes.ComponentId =
    let sym = List.find (fun (sym:Symbol) -> List.exists (fun (p: CommonTypes.Port) -> p.Id = portId) sym.Ports) model
    sym.Id

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"

/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth 
        (wId: CommonTypes.ConnectionId) 
        (outputPortNumber: int) 
        (inputPortWidths: int option list) : int option =
    failwithf "Not implemented"
    
        
    

    
//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"
