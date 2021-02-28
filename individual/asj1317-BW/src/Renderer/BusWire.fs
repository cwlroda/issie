module BusWire

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers


//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

type Orientation =
    | V
    | H

type WireSegment =
    { 
      Id: CommonTypes.ComponentId
      StartPt: XYPos //Left/bottom
      EndPt: XYPos
      LastPos: XYPos  //Top/Right
      Orien: Orientation }
/// type for buswires
/// for demo only. The real wires will
/// connect to Ports - not symbols, where each symbol has
/// a number of ports (see Issie Component and Port types) and have
/// extra information for highlighting, width, etc.
/// NB - how you define Ports for drawing - whether they correspond to
/// a separate datatype and Id, or whether port offsets from
/// component coordinates are held in some other way, is up to groups.
type Wire =
    { Id: CommonTypes.ConnectionId
      SrcPort: CommonTypes.PortId
      TargetPort: CommonTypes.PortId
      WireSegments: WireSegment list
      WireColor: CommonTypes.HighLightColor
      WireWidth: int
}

let BBoxSize = 2

type Model = { Symbol: Symbol.Model; WX: Map<CommonTypes.ConnectionId, Wire> }


//----------------------------Message Type-----------------------------------//

/// Messages to update buswire model
/// These are OK for the demo - but not the correct messages for
/// a production system. In the real system wires must connection
/// to ports, not symbols. In addition there will be other changes needed
/// for highlighting, width inference, etc
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (CommonTypes.ConnectionId * CommonTypes.ConnectionId)
    | SetColor of CommonTypes.HighLightColor
    | MouseMsg of MouseT
    | SetSelected of CommonTypes.ConnectionId
    | UnselectAll
    | StartDragging of wId: CommonTypes.ConnectionId list* pos: XYPos
    | Dragging of wId: CommonTypes.ConnectionId list* pos: XYPos
    | EndDragging

///--------------------Helpers Functions -------------------------------------///
/// Useful helper functions
let posOf x y = { X = x; Y = y }

/// Takes a set of coordinates and returns a new set of cooridantes adjusted according to the adjPos floats given
/// each in the X and Y respectively
let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

//Given two points returns the coordinates for the point in the middle inbetween the given points
let midPt (aPos: XYPos) (bPos: XYPos): XYPos =
    { X = (aPos.X + bPos.X) / 2.
      Y = (aPos.X + bPos.Y) / 2. }



    





/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    match Map.tryFind wId wModel.WX with
    | Some vWire -> vWire
    | None -> failwithf "Invalid Id passed"

/// Takes a wire segment and the width of the wire as input and using its oritenation returns the correct BBox
let createSegBB (wireW: int) (seg: WireSegment): BBox =
    let adjTopLeft = {X = float -BBoxSize ; Y =  float BBoxSize}
    let posDiff = posDiff seg.StartPt seg.EndPt
    let topLeftCorner =
        match seg.Orien with
        | V when posDiff.Y < 0. -> posAdd seg.EndPt adjTopLeft
        | V -> posAdd seg.StartPt adjTopLeft
        | H when posDiff.X < 0. -> posAdd seg.StartPt adjTopLeft
        | H -> posAdd seg.EndPt adjTopLeft

    {Pos = topLeftCorner; W = abs (int posDiff.X) + 2*BBoxSize; H = abs (int posDiff.Y) + 2*BBoxSize}


        
///Takes a point and a BBox and checks if the point are within the bounderies of the box
/// Returns true if the points is within the bounderires of the BBox
/// Otherwise returns false

let ptInBB (pt: XYPos) (bb: BBox): bool =
    let diffX =  pt.X - bb.Pos.X
    let diffY =  bb.Pos.Y - pt.Y

    match diffX, diffY with
    | x, _ when (x > (float bb.W)) ->
        false
    | _, y when (y > (float bb.H)) -> 
        false
    | _ -> 
        true


///Takes as input a wire and a point and checks if the pt is within the BBox of any of the segments
/// Returns true if it is close to one of the segments
/// Returns false otherwise
let ptCloseToWire (pt: XYPos) (wire: Wire) =
    let ptCloseToSeg (seg: WireSegment): bool =
        seg 
        |> createSegBB wire.WireWidth |> (ptInBB pt)

    List.tryFind ptCloseToSeg wire.WireSegments


//Calculates the distance between a point and a wire segment
let distPtToWire (pt: XYPos) (wSeg: WireSegment) =
    let ptToPtA =
        posOf (pt.X - wSeg.EndPt.X) (pt.Y - wSeg.EndPt.Y)

    let ptAToB =
        posOf (wSeg.EndPt.X - wSeg.StartPt.X) (wSeg.EndPt.Y - wSeg.StartPt.Y)

    let magPtAToPtB =
        ((ptAToB.X) ** 2.) + ((ptAToB.Y) ** 2.) ** 0.5

    let crossProd =
        ((ptAToB.X) * (ptToPtA.Y)) - ((ptToPtA.X) * (ptAToB.Y))

    abs (crossProd) / magPtAToPtB

let createWireSeg (startPos: XYPos) (endPos: XYPos) (O: Orientation) : WireSegment =
    {
        Id= CommonTypes.ComponentId(uuid ())
        StartPt = startPos
        EndPt = endPos
        LastPos = midPt startPos endPos
        Orien = O
    }

let roundSym (startPt: XYPos) (relShift: XYPos) (endPt: XYPos) =
    let midVert = 
        posAdd startPt relShift
    let orLst = [H; V; H]
    [startPt; midVert; endPt]
    |> List.pairwise
    |> List.zip orLst
    |> List.map (fun (o, (p1, p2)) -> createWireSeg p1 p2 o)


///Creates the wireSegement list which represents the verticies of the wire between the two points
let autoRoute (wModel: Model) (startId: CommonTypes.PortId) (endId: CommonTypes.PortId): WireSegment list =
    let startPos = Symbol.portPos wModel.Symbol startId
    let endPos = Symbol.portPos wModel.Symbol endId
    let midPos = midPt startPos endPos
    

    let wireDir = posDiff endPos startPos
   
    let initialSegs,finalSegs = 
        match wireDir.X with
        | x when x > 0. -> [startPos; {X=midPos.X ; Y = startPos.Y}],[{X=midPos.X ; Y = endPos.Y}; endPos ]
        | _ -> [startPos; {X=startPos.X+5.0; Y = startPos.Y}; {X=startPos.X + 5.; Y=midPos.Y}], [{X =endPos.X-5. ; Y=midPos.Y}; {X = endPos.X-5.; Y= endPos.Y}; endPos]
    

    let segLst= initialSegs@[initialSegs.Item(initialSegs.Length-1); finalSegs.Head]@finalSegs
    let nrSegs = List.length segLst

    [1..nrSegs]
    |> List.map (fun el -> if el%2 = 0 then V else H)
    |> List.zip (List.pairwise segLst)
    |> List.map (fun ((p1, p2),o) -> createWireSeg p1 p2 o)


let manuelRouting (wModel: Model) (wireId: CommonTypes.ConnectionId) (pos:XYPos) =
(*     let wire = wire wModel wireId
    let isTarget wWidth seg =
        seg |> (createSegBB wWidth) |> (ptInBB pos)
    
    let movingSeg = 
        List.Find (fun seg -> (isTarget wire.WireWidth) seg) wire.WireSegments
 *)
    failwithf "Not Implemented"
    
       
    



let addWire (wModel: Model)(port1: CommonTypes.PortId) (port2: CommonTypes.PortId) : Wire =
    let getPortType = Symbol.portType wModel.Symbol
    let src, tgt, colour =
        match getPortType port1, getPortType port2 with
        | p1, p2 when p1 = p2 -> port2, port1, CommonTypes.HighLightColor.Red
        | CommonTypes.PortType.Input, CommonTypes.PortType.Output -> port2, port1, CommonTypes.HighLightColor.Grey
        | _,_ -> port1, port2, CommonTypes.HighLightColor.Grey

    let w = 
        match Symbol.portWidth wModel.Symbol src with
        | Some w -> w
        | None -> 4
    {
        Id = CommonTypes.ConnectionId(uuid ())
        SrcPort = src
        TargetPort = tgt
        WireSegments = autoRoute wModel src tgt  
        WireColor = colour
        WireWidth = w
    } 

let updateSelectedWires  (wModel: Model) (wIdLst: CommonTypes.ConnectionId list) =
    let updateModel wId w =      
        Map.add wId {w with WireSegments = autoRoute wModel w.SrcPort w.TargetPort} wModel.WX
        
    let foldfunc wxModel wId =
        match Map.tryFind wId wxModel with
        | Some w -> 
            updateModel wId w
        | None -> 
            wxModel 
    
    List.fold foldfunc wModel.WX wIdLst

    



type SegRenderProps =
    { 
      Key: CommonTypes.ComponentId
      Seg: WireSegment
      ColorP: string
      StrokeWidthP: string 
      }

/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.
///




let singleWireView =
    FunctionComponent.Of
        (fun (props: SegRenderProps) ->
            line [ 
                X1 props.Seg.StartPt.X
                Y1 props.Seg.StartPt.Y
                X2 props.Seg.EndPt.X
                Y2 props.Seg.EndPt.Y
           // Qualify these props to avoid name collision with CSSProp
                SVGAttr.Stroke props.ColorP
                SVGAttr.StrokeWidth props.StrokeWidthP ] []
            )
let setColor (wModel: Model) (c: CommonTypes.HighLightColor) =
    Map.map (fun wId w -> {w with WireColor = c}) wModel.WX

let setSelectedColor (wModel: Model) (wID: CommonTypes.ConnectionId) =
    let w = wire wModel wID
    wModel.WX.Add(wID, {w with WireColor = CommonTypes.HighLightColor.Blue})

let setUnselectedColor (wModel: Model) =
    setColor wModel CommonTypes.HighLightColor.Blue



let view (model: Model) (dispatch: Dispatch<Msg>) =
    let wires =
        model.WX
        |> Map.toList
        |> List.fold (fun lst (wId, w) ->  
            List.map (fun (seg: WireSegment) ->
                    let props =
                        {
                            Key = seg.Id
                            Seg = seg
                            ColorP = w.WireColor.ToString()
                            StrokeWidthP = "2px"
                        }
                    singleWireView props)  w.WireSegments @ lst) []
                    
                  
         

    let symbols =
        Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    printf $"{wires}"
    g [] [ (g [] wires); symbols ]

/// dummy init for testing: real init would probably start with no wires.
/// this initialisation is not realistic - ports are not used
/// this initialisation depends on details of Symbol.Model type.
let init n () =
    let symbols, cmd = Symbol.init ()

    let symIds =
        List.map (fun (sym: Symbol.Symbol) -> sym.Id) symbols

    let rng = System.Random 0
    let model = { Symbol =symbols; WX = Map.empty}
    let makeRandomWire () =
        let n = symIds.Length

        let s1, s2 =
            match rng.Next(0, n - 1), rng.Next(0, n - 2) with
            | r1, r2 when r1 = r2 -> 
                symbols.[r1], symbols.[n - 1]
      
            | r1, r2 -> 
                symbols.[r1], symbols.[r2]
                
        let (p1,p2) =
            let p1 =
                s1.Ports
                |> List.find (fun p -> p.PortType = CommonTypes.PortType.Input)
            let p2 =
                s2.Ports
                |> List.find (fun p -> p.PortType = CommonTypes.PortType.Output)
            (p1.Id,p2.Id)
        
        let w = addWire model p1 p2
        printf$"{w}"
        [(w.Id, w)]         

    List.fold (fun lst i -> makeRandomWire ()@lst) [] [1]
    |> Map.ofList
    |> (fun wires -> {model with WX = wires}, Cmd.none)

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        let sm, sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol = sm }, Cmd.map Symbol sCmd
    | AddWire _ -> failwithf "Not implemented"
    | SetColor c -> {model with WX = (setColor model c)}, Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol(Symbol.MouseMsg mMsg))
    | SetSelected wMsg -> 
        let wxUpdated = (setSelectedColor model wMsg)
        {model with WX = wxUpdated}, Cmd.none
    | UnselectAll -> {model with WX = setUnselectedColor model}, Cmd.none
    | Dragging (wMsgId, wMsgPos)-> 
        let wxUpdated = updateSelectedWires model wMsgId
        {model with WX = wxUpdated}, Cmd.none
    | StartDragging (wMsgId, wMsgPos) -> failwithf "Not Implemented"
    | EndDragging -> 
        let wxModel =updateSelectedWires model (Map.toList model.WX |> List.map (fun (a,b)-> a))
        {model with WX = wxModel}, Cmd.none
        
        


//---------------Other interface functions--------------------//

/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Used
/// to determine which wire (if any) to select on a mouse click

let getTargetedWire (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId Option  =
    let isTarget w =
        match ptCloseToWire pos w with
        | Some seg -> [(w.Id, seg)]
        | None -> []
       
    let closestWire lst =
        List.map (fun (wId, seg) -> (wId, distPtToWire pos seg)) lst
        |> List.maxBy (fun (w, dist) -> -dist)
        |> fst
    
    let wiresCloseToPt = Map.fold (fun lst kWire vWire -> (isTarget vWire)@lst) [] wModel.WX
    
    match wiresCloseToPt with
    | [(wId, seg)] -> 
        Some wId
    | [] -> None
    | lst -> 
        Some (closestWire lst)


let getWiresInTargetBBox (wModel: Model) (bbox: BBox) : CommonTypes.ConnectionId list =
    let wireInBBox w =
        List.exists (fun seg ->
            ptInBB seg.StartPt bbox || ptInBB seg.EndPt bbox) w.WireSegments

    Map.fold (fun lst k w -> if wireInBBox w then [k]@lst else lst) [] wModel.WX
//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId: CommonTypes.ComponentId): CommonTypes.Component = failwithf "Not implemented"

let extractWires (wModel: Model): CommonTypes.Component list = failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp: CommonTypes.Component) = failwithf "Not Implemented"
