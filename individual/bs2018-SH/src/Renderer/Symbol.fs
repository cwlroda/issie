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
    /// 
type Port = {
    Id : CommonTypes.PortId
    Type : CommonTypes.PortType
    RelPos : XYPos
    Highlighted : bool
}

type Symbol =
    {
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        Component : CommonTypes.Component
        Selected : bool
        Ports : Port list
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
    | StartDragging of idLst : CommonTypes.ComponentId list * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    | Dragging of idLst : CommonTypes.ComponentId list * pagePos: XYPos
    | EndDragging
    | AddSymbol of CommonTypes.ComponentType * XYPos
    | DeleteSymbol of sId:CommonTypes.ComponentId 
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface
    | SetSelected of CommonTypes.ComponentId list
    | HighlightPort of CommonTypes.PortId list
    | UnhighlightPorts



//---------------------------------helper types and functions----------------//

let getTargetedSymbol (symModel: Model) (pos:XYPos) : CommonTypes.ComponentId Option = 
    let clickInSym sym =
        let bb = {
            Pos = sym.Pos
            Height = float sym.Component.H
            Width = float sym.Component.W
        }
        containsPoint pos bb

    match (symModel |> List.tryFind clickInSym) with
    | None -> None
    | Some sym -> Some sym.Id

let getPortsOfSymbol (symModel : Model) (symId : CommonTypes.ComponentId) : CommonTypes.PortId List = 
    (
        List.find (fun sym -> sym.Id = symId) symModel
    ).Ports
    |> List.map (fun p -> p.Id)

let getAllPortsWithSymbol (symModel : Model) = 
    let insertSymToPortList (pl : Port list) (sym : Symbol) =
        pl
        |> List.map (fun el -> (el, sym))

    symModel
    |> List.map (fun sym -> (sym, sym.Ports))
    |> List.collect (fun (sym, pl) -> insertSymToPortList pl sym)

let portsInRange (symModel : Model) (pos : XYPos) (range : float) : CommonTypes.PortId list =
    getAllPortsWithSymbol symModel
    |> List.filter (fun (p,sym) -> (distanceBetweenPoints (posAdd p.RelPos sym.Pos) pos) <= range)
    |> List.map (fun (p,_) -> p.Id)

let portPos (symModel : Model) (pId : CommonTypes.PortId) : XYPos =
    let res = 
        getAllPortsWithSymbol symModel
        |> List.find (fun (p,_) -> p.Id = pId)
    match res with
    | (p, s) -> posOf (p.RelPos.X+s.Pos.X) (p.RelPos.Y+s.Pos.Y)

let portType (symModel : Model) (pId : CommonTypes.PortId) : CommonTypes.PortType =
    (
        getAllPortsWithSymbol symModel
        |> List.map fst
        |> List.find (fun p -> p.Id = pId)
    ).Type
    

let getTargetedPort (symModel : Model) (pos : XYPos) : CommonTypes.PortId Option = 
    let posInPort (sym: Symbol) (port : Port) : bool =
        let bb = {
            Pos = posOf (port.RelPos.X + sym.Pos.X - 2.5) (port.RelPos.Y + sym.Pos.Y - 2.5)
            Height = 10.
            Width = 10.
        }
        containsPoint pos bb

    let res =
        getAllPortsWithSymbol symModel
        |> List.tryFind (fun (port, sym) -> posInPort sym port)

    match res with
    | Some (p,_) -> Some p.Id
    | None -> None

let getSymbolsInTargetArea (symModel: Model) (bb:BBox) : CommonTypes.ComponentId list =
    symModel
    |> List.filter (fun el -> containsPoint el.Pos bb)
    |> List.map (fun el -> el.Id)

let getAllSymbols (symModel: Model) : CommonTypes.ComponentId list =
    symModel
    |> List.map (fun el-> el.Id)

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}


//-----------------------------Skeleton Model Type for symbols----------------//




//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (pos:XYPos) =
    {
        Pos = pos
        LastDragPos = {X=0. ; Y=0.} // initial value can always be this
        IsDragging = false // initial value can always be this
        Id = CommonTypes.ComponentId (Helpers.uuid()) // create a unique id for this symbol
        Component = {
            Id = uuid()
            Type = CommonTypes.And
            Label = ""
            InputPorts = []
            OutputPorts = []
            X = int pos.X
            Y = int pos.Y
            H = 40
            W = 40
        }
        Selected = false
        Ports = [
            {
                Id = CommonTypes.PortId ( uuid() )
                Type = CommonTypes.Input
                RelPos = posOf 0.0 20.0
                Highlighted = false
            }
            {
                Id = CommonTypes.PortId ( uuid() )
                Type = CommonTypes.Output
                RelPos = posOf 40.0 20.0
                Highlighted = false
            }
        ]
    }




/// Dummy function for test. The real init would probably have no symbols.
let init () =
    []
    , Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (_, pos) -> 
        createNewSymbol pos :: model, Cmd.none
    | DeleteSymbol sId -> 
        List.filter (fun sym -> sym.Id <> sId) model, Cmd.none
    | StartDragging (sLst, pagePos) ->
        model
        |> List.map (fun sym ->
            if List.contains sym.Id sLst then
                { sym with
                    LastDragPos = pagePos
                    //IsDragging = true
                }
            else
                sym
        )
        , Cmd.none

    | Dragging (sLst, pagePos) ->
        model
        |> List.map (fun sym ->
            if List.contains sym.Id sLst then
                let diff = posDiff pagePos sym.LastDragPos
                { sym with
                    Pos = posAdd sym.Pos diff
                    LastDragPos = pagePos
                }
            else
                sym
        )
        , Cmd.none

    | EndDragging ->
        model
        // |> List.map (fun sym ->
        //     { sym with
        //         IsDragging = false 
        //     }
        // )
        , Cmd.none
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags
    | SetSelected symlst ->
        model
        |> List.map (fun sym ->
            if List.contains sym.Id symlst then 
                { sym with
                    IsDragging = true 
                }
            else
                { sym with
                    IsDragging = false 
                }
        )
        , Cmd.none
    | HighlightPort ports ->
        model
        |> List.map (fun sym -> 
            let ports = sym.Ports
                            |> List.map (fun port ->
                                if List.contains port.Id ports then {port with Highlighted = true} else port
                            )
            {sym with Ports = ports}
        ), Cmd.none
    | UnhighlightPorts ->
        model
        |> List.map (fun sym -> 
            let ports = sym.Ports
                            |> List.map (fun port ->
                                {port with Highlighted = false}
                            )
            {sym with Ports = ports}
        ), Cmd.none
    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderCircleProps =
    {
        Circle : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

/// View for one symbol with caching for efficient execution when input does not change
let private renderCircle =
    FunctionComponent.Of(
        fun (props : RenderCircleProps) ->
            // let handleMouseMove =
            //     Hooks.useRef(fun (ev : Types.Event) ->
            //         let ev = ev :?> Types.MouseEvent
            //         // x,y coordinates here do not compensate for transform in Sheet
            //         // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
            //         Dragging(props.Circle.Id, posOf ev.pageX ev.pageY)
            //         |> props.Dispatch
            //     )

            let color =
                if props.Circle.IsDragging then
                    "lightblue"
                else
                    "grey"

            polygon
                [ 
                    // OnMouseUp (fun ev -> 
                    //     document.removeEventListener("mousemove", handleMouseMove.current)
                    //     EndDragging props.Circle.Id
                    //     |> props.Dispatch
                    // )
                    // OnMouseDown (fun ev -> 
                    //     // See note above re coords wrong if zoom <> 1.0
                    //     StartDragging (props.Circle.Id, posOf ev.pageX ev.pageY)
                    //     |> props.Dispatch
                    //     document.addEventListener("mousemove", handleMouseMove.current)
                    // )
                    Cx props.Circle.Pos.X
                    Cy props.Circle.Pos.Y
                    SVGAttr.Points (polygonPointsString props.Circle.Pos (posOf (props.Circle.Pos.X + 40.) (props.Circle.Pos.Y + 40.)))
                    SVGAttr.Fill color
                    SVGAttr.Stroke color
                    SVGAttr.StrokeWidth 1
                ]
                [ ]
    , "Circle"
    , equalsButFunctions
    )

let renderPorts (ports : Port list) (symPos : XYPos) = 
    ports
    |> List.map (fun port ->
        let color = if port.Highlighted then "orange" else "black" 
        circle [
            Cx (symPos.X + port.RelPos.X)
            Cy (symPos.Y + port.RelPos.Y)
            R 5.
            SVGAttr.Fill color
        ] []
    )

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun sym ->
        let sid = match sym.Id with
                    | CommonTypes.ComponentId id -> id
        [renderCircle 
            {
                Circle = sym
                Dispatch = dispatch
                key = sid
            }]
        |> List.append (renderPorts sym.Ports sym.Pos)
    )
    |> List.collect id
    |> ofList


//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)



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
