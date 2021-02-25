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

let createBBox (sym: Symbol) =
    {
        Pos = sym.Pos
        W = sym.W
        H = sym.H
    }

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}



let ptInBB (pt: XYPos) (bb: BBox): bool =
    printf $"Pos: {pt}"
    printf $"BBox.pos {bb.Pos}, bbox.W {bb.W}, bbox.h {bb.H}"
    let diffX =  pt.X - bb.Pos.X
    let diffY =  bb.Pos.Y - pt.Y
    printf $"DiffX: {diffX} Diff {diffY}"

    match diffX, diffY with
    | x, _ when (x > (float bb.W)) ->
        false
    | _, y when (y > (float bb.H)) -> 
        false
    | _ -> 
        true

let boxDef (pos:XYPos) (w:int) (h:int): string =
    let tL = pos
    let tR = posOf (pos.X+(float w)) (pos.Y)
    let bR = posOf (pos.X+(float w )) (pos.Y - (float h))
    let bL = posOf (pos.X) (pos.Y - (float h))
    $"{bL.X},{bL.Y} {tL.X},{tL.Y} {tR.X},{tR.Y} {bR.X},{bR.Y}"


//-----------------------------Skeleton Model Type for symbols----------------//

//-----------------------Skeleton Message type for symbols---------------------//
let createPorts (sId: CommonTypes.ComponentId) ((nInP, nOutP):int *int) =
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
        W = 10*(lenLabel+2)
        H = 10*(nrPorts + 2)   
    }


/// Dummy function for test. The real init would probably have no symbols.
let init () =
    List.allPairs [1..2] [1..2]
    |> List.map (fun (x,y) -> ({X = float (x*64+30); Y=float (y*64+30)}, x,y))
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

/// View for one symbol with caching for efficient execution when input does not change
let private renderSymbol =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.sym.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )

            let color =
                if props.sym.IsDragging then
                    "lightblue"
                else
                    "grey"

            
            polygon
                [
                    OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.sym.Id
                        |> props.Dispatch
                    )
                    OnMouseDown (fun ev -> 
                        // See note above re coords wrong if zoom <> 1.0
                        StartDragging (props.sym.Id, posOf ev.pageX ev.pageY)
                        |> props.Dispatch
                        document.addEventListener("mousemove", handleMouseMove.current)
                    )

                    
                    SVGAttr.Points (boxDef props.sym.Pos props.sym.W props.sym.H)
                    SVGAttr.StrokeWidth "1px"
                    SVGAttr.Stroke "Black"
                    SVGAttr.FillOpacity 0.5
                    SVGAttr.Fill "Grey"
                    
                ]
                [ ]
               
    , "Symbols"
    , equalsButFunctions
    )

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
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


//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)

let getTargetedSymbol (symModel: Model) (pos: XYPos) =
    
    let sym = List.tryFind (fun sym ->  ptInBB pos (createBBox sym)) symModel
    match sym with  
    | Some sym -> Some {Pos = sym.Pos; W = sym.W; H = sym.H}
    | None -> None

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

let portPos (model: Model) (portID: string) : XYPos =
    let inPortLst (pLst: CommonTypes.Port list) =
        List.tryFind (fun (p:CommonTypes.Port) -> p.Id = portID) pLst
        
    
    let portOpt = List.tryPick (fun s -> inPortLst s.Ports) model
    let (sym, port) = 
        match portOpt with
        | Some port -> ((List.find (fun s -> s.Id.ToString() = port.HostId) model), port)
        | None -> failwithf "Invalid port id sent"
    
    match port.PortNumber with
    | Some nr when port.PortType = CommonTypes.PortType.Input -> posOf (sym.Pos.X) (sym.Pos.Y - 10.*(float nr+1.))
    | Some nr -> posOf (sym.Pos.X + (float sym.W)) (sym.Pos.Y - 10.* (float nr+1.))
    | None -> failwithf "The port does not exist"
    
//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"
