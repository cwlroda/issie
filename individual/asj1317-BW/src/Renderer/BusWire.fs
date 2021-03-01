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
      EndPt: XYPos  //Top/Right
      Orien: Orientation
      ColourSeg: CommonTypes.HighLightColor }
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
      LastPos: XYPos
      //WireSegments: WireSegment list //When update to use PolyLine SVGAttr.Points (boxDef sym.Pos sym.W sym.H)
      WireSegments: XYPos list
      WireColor: CommonTypes.HighLightColor
      WireWidth: int
}

let BBoxSize = 4

type Model = { Symbol: Symbol.Model; WX: Map<CommonTypes.ConnectionId, Wire> ; BB: Helpers.BBox list; SelectedWire : CommonTypes.ConnectionId Option}


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
    | StartDragging of wId: CommonTypes.ConnectionId * pos: XYPos
    | Dragging of wId: CommonTypes.ConnectionId * pos: XYPos
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

let lineDef (verts: XYPos list) = 
    printf $" Verticies: {verts}"
    
    List.map (fun pos-> $"{pos.X},{pos.Y}") verts

    |> List.fold (fun outstr str -> outstr + " " + str) ""

let boxDef (pos:XYPos) (w:int) (h:int): string =
    let tL = pos
    let tR = posOf (pos.X+(float w)) (pos.Y)
    let bR = posOf (pos.X+(float w )) (pos.Y - (float h))
    let bL = posOf (pos.X) (pos.Y - (float h))
    $"{bL.X},{bL.Y} {tL.X},{tL.Y} {tR.X},{tR.Y} {bR.X},{bR.Y}"
//Given two points returns the coordinates for the point in the middle inbetween the given points
let midPt (aPos: XYPos) (bPos: XYPos): XYPos =
    let diff =   posDiff bPos aPos
    posAdd aPos (posOf (diff.X/2.) (diff.Y/2.))

/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    match Map.tryFind wId wModel.WX with
    | Some vWire -> vWire
    | None -> failwithf "Invalid Id passed"

/// Takes a wire segment and the width of the wire as input and using its oritenation returns the correct BBox
let createSegBB (wireW: int) ((startPt, endPt): XYPos*XYPos) = // (seg: WireSegment): BBox =
    
    let posDiff = posDiff startPt endPt
    let adjTopLeft = 
        {X = float -BBoxSize ; Y =  float BBoxSize}
    let wHCalc (diff:float) =
        match abs(diff) < 1.0 with
        | true -> 2*BBoxSize
        | _ -> int(diff) + 2*BBoxSize
    let topLeftCorner =
        match abs(posDiff.X) < abs(posDiff.Y) with
        | true -> posAdd startPt adjTopLeft
        | _ -> posAdd endPt adjTopLeft
    {Pos = topLeftCorner; W = (wHCalc posDiff.X) ; H = (wHCalc posDiff.Y)}


        
///Takes a point and a BBox and checks if the point are within the bounderies of the box
/// Returns true if the points is within the bounderires of the BBox
/// Otherwise returns false

let ptInBB (pt: XYPos) (bb: BBox): bool =
    let bRCorner = posAdd bb.Pos {X=float bb.W; Y = float -bb.H}
    printf $"pos: {pt}; bb.Pos {bb.Pos}; brConrer {bRCorner}"
    match pt.X, pt.Y with
    | x, _ when x < bb.Pos.X || x > bRCorner.X -> 
        printf "Fails on X"
        false

    | _, y when y > bb.Pos.Y || y < bRCorner.Y -> 
        printf "Fails on y"
        false
    | _ -> true
        
    


///Takes as input a wire and a point and checks if the pt is within the BBox of any of the segments
/// Returns true if it is close to one of the segments
/// Returns false otherwise
let ptCloseToWire (pt: XYPos) (wire: Wire) =
    let ptCloseToSeg segPts: bool = 
      
         createSegBB wire.WireWidth segPts
        |> (ptInBB pt)
    
    List.pairwise wire.WireSegments
    |> List.exists ptCloseToSeg 
    


//Calculates the distance between a point and a wire segment
let distPtToSeg (pt: XYPos) ((startPt, endPt): XYPos*XYPos) = //(wSeg: WireSegment) =
    let ptToPtA =
        posOf (pt.X - endPt.X) (pt.Y - endPt.Y)

    let ptAToB =
        posOf (endPt.X - startPt.X) (endPt.Y - startPt.Y)

    let magPtAToPtB =
        ((ptAToB.X) ** 2.) + ((ptAToB.Y) ** 2.) ** 0.5

    let crossProd =
        ((ptAToB.X) * (ptToPtA.Y)) - ((ptToPtA.X) * (ptAToB.Y))

    abs (crossProd) / magPtAToPtB

    
let distPtToWire (pt:XYPos) (wire: Wire) =
    List.pairwise wire.WireSegments
    |> List.map (fun verts -> distPtToSeg pt verts) 
    |> List.maxBy (fun dist -> -dist)

(* let createWireSeg (startPos: XYPos) (endPos: XYPos) (O: Orientation) : WireSegment =
    {
        Id= CommonTypes.ComponentId(uuid ())
        StartPt = startPos
        EndPt = endPos
        Orien = O
        ColourSeg = CommonTypes.HighLightColor.Blue
    } *)

let roundSym (startPt: XYPos) (relShift: XYPos) (endPt: XYPos) =
    let midVert = 
        posAdd startPt relShift
    let orLst = [H; V; H]
    [startPt; midVert; endPt]
    (*|> List.pairwise
    |> List.zip orLst
    |> List.map (fun (o, (p1, p2)) -> createWireSeg p1 p2 o) *)

(* let updateMovedWire (w: Wire) (movedSeg: WireSegment) : Wire =
    let segIdx = List.findIndex (fun (seg:WireSegment) -> seg.Id = movedSeg.Id) w.WireSegments
    let updateSegLst = 
        if segIdx = 0 || segIdx = (List.length w.WireSegments)-1
        then w.WireSegments
        else
            List.mapi (fun i seg ->
                match i with
                |  idx when idx = segIdx -> movedSeg
                | idx when (idx-1) = segIdx -> {seg with EndPt = movedSeg.StartPt}
                | idx when (idx+1) = segIdx -> {seg with StartPt = movedSeg.EndPt}
                | _ -> seg
                ) w.WireSegments
    {w with WireSegments= updateSegLst} *)

///Creates the wireSegement list which represents the verticies of the wire between the two points
let autoRoute (wModel: Model) (startId: CommonTypes.PortId) (endId: CommonTypes.PortId)  =
    let startPos = Symbol.portPos wModel.Symbol startId
    let endPos = Symbol.portPos wModel.Symbol endId
    let midPos = midPt startPos endPos
    

    let wireDir = posDiff endPos startPos
   
    let initialSegs,finalSegs = 
        match wireDir.X with
        | x when x > 0. -> [startPos; {X=midPos.X ; Y = startPos.Y}],[{X=midPos.X ; Y = endPos.Y}; endPos ]
        | _ -> [startPos; {X=startPos.X+5.0; Y = startPos.Y}; {X=startPos.X + 5.; Y=midPos.Y}], [{X =endPos.X-5. ; Y=midPos.Y}; {X = endPos.X-5.; Y= endPos.Y}; endPos]
    

    initialSegs@finalSegs
(*     let nrSegs = List.length segLst

    [1..nrSegs]
    |> List.map (fun el -> if el%2 = 0 then V else H)
    |> List.zip (List.pairwise segLst)
    |> List.map (fun ((p1, p2),o) -> createWireSeg p1 p2 o) *)
let updateVerts w (idx1, idx2) move orien=  
    printf$"Do I get here: {idx1} {idx2}"
    let relMove = 
        match orien.X, orien.Y with
        | x,y when abs(x)<abs(y) -> {X=move.X; Y = 0.0}
        | _-> {X= 0.0; Y = move.Y}
    List.mapi (fun idx vert ->
                    match idx with  
                    | idx when idx1 = idx -> 
                        printf $"Updated vertices at {idx} with relMove: {relMove}"
                        posDiff vert relMove
                    | idx when idx2 = idx ->
                        printf $"Updated vertices at {idx} with relMove: {relMove}"
                        posDiff vert relMove
                    | _ -> vert ) 
                w.WireSegments

let manuelRouting (wModel: Model) (wireId: CommonTypes.ConnectionId) (pos:XYPos) = 
    let w = wire wModel wireId
    let isTarget wWidth segPts =
        segPts |> (createSegBB wWidth) |> (ptInBB w.LastPos)
   
    let segIdx = 
        List.pairwise w.WireSegments
        |> List.findIndex (isTarget w.WireWidth) 
    let move = posDiff w.LastPos  pos
    printf $"SegIdx: {segIdx}"
    printf $"RelMovements: {move}"
    let updatedWireSegs = 
        match segIdx with 
        //| idx when  idx = 0 || idx = (List.length w.WireSegments) -> w.WireSegments
        | idx -> 
            let orien = posDiff w.WireSegments.[idx] w.WireSegments.[idx+1]
            updateVerts w (idx, idx+1) move orien
       
    printf $"{updatedWireSegs}"               
    Map.add wireId {w with WireSegments = updatedWireSegs; LastPos = pos} wModel.WX    

    
let addWire (wModel: Model)(port1: CommonTypes.PortId) (port2: CommonTypes.PortId) : Wire =
    let getPortType = Symbol.portType wModel.Symbol
    let src, tgt, colour =
        match getPortType port1, getPortType port2 with
        | p1, p2 when p1 = p2 -> port2, port1, CommonTypes.HighLightColor.Red
        | p1, _ when p1 = CommonTypes.PortType.Input -> port2, port1, CommonTypes.HighLightColor.Grey
        | _,_ -> port1, port2, CommonTypes.HighLightColor.Grey

    let w = 
        match Symbol.portWidth wModel.Symbol src with
        | Some w -> w
        | None -> 4

    let wSegLst = autoRoute wModel src tgt
    {
        Id = CommonTypes.ConnectionId(uuid ())
        SrcPort = src
        TargetPort = tgt
         // Default
        WireSegments =  wSegLst
        LastPos = wSegLst.[0] 
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

let setColor (wModel:Model) (c: CommonTypes.HighLightColor) (wId) =
    let updateWire = {(Map.find wId wModel.WX) with WireColor = c}
    Map.add wId updateWire wModel.WX
    

let setSelectedColor (wModel: Model) (wID: CommonTypes.ConnectionId) =
    let w = wire wModel wID
    wModel.WX.Add(wID, {w with WireColor = CommonTypes.HighLightColor.Blue})

let setUnselectedColor (wModel: Model) =
    Map.map (fun wId w -> {w with WireColor = CommonTypes.HighLightColor.Blue}) wModel.WX

let endDrag (wModel: Model) =
    setUnselectedColor wModel

let startDrag (wModel: Model) (wId: CommonTypes.ConnectionId) (pos: XYPos) =
    let wx = Map.add wId {(wire wModel wId) with LastPos= pos} wModel.WX
    setColor {wModel with WX = wx} CommonTypes.HighLightColor.Green wId
        



type SegRenderProps =
    { 
      Key: CommonTypes.ConnectionId
      Seg: XYPos list
      ColorP: string
      StrokeWidthP: string 
      }

/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.
///




let singleWireView =
    FunctionComponent.Of
        (fun (props: SegRenderProps) ->
            polyline 
                [
                    SVGAttr.Points (lineDef props.Seg)
                    SVGAttr.Stroke props.ColorP
                    SVGAttr.StrokeWidth props.StrokeWidthP            
                    SVGAttr.FillOpacity 0.0
                    
                    
                ] []
            )

    
    

let singleBBox box =
        
    polygon
        [ 
            SVGAttr.Points (boxDef box.Pos box.W box.H)
            SVGAttr.StrokeWidth "1px"
            SVGAttr.Stroke "Black"
            SVGAttr.FillOpacity 0.2
            SVGAttr.Fill "Blue"
            
        ][]

type RenderBBoxProps = 
    {
        Key: string
        Box: BBox

    }
let private renderBBox =
    FunctionComponent.Of(
        fun (props : RenderBBoxProps) ->
            singleBBox props.Box) 


let view (model: Model) (dispatch: Dispatch<Msg>) =
    printf $"{model.WX}"
    let wires =
        model.WX
        |> Map.toList
        |> List.map (fun (wId, w) ->  
                let props =
                    {
                        Key = wId
                        Seg = w.WireSegments
                        ColorP = w.WireColor.ToString()
                        StrokeWidthP = "2px"
                    }
                singleWireView props)          
    let bBoxs =
        model.BB
        |> List.map (fun bbox -> 
            let propsBB =
                {
                    Key = Helpers.uuid()
                    Box = bbox
                } 
            renderBBox propsBB)
    let symbols =
        Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    g [] [ (g [] wires);(g [] bBoxs) ; symbols ]

let createBB wLst =
    Map.toList wLst
    |> List.fold (fun lst (wId, w) -> (List.pairwise w.WireSegments |> List.map (fun seg -> (createSegBB w.WireWidth) seg))@lst) []
/// dummy init for testing: real init would probably start with no wires.
/// this initialisation is not realistic - ports are not used
/// this initialisation depends on details of Symbol.Model type.
let init n () =
    let symbols, cmd = Symbol.init ()

    let symIds =
        List.map (fun (sym: Symbol.Symbol) -> sym.Id) symbols

    let rng = System.Random 0
    let model = {Symbol =symbols; WX = Map.empty; BB = []; SelectedWire = None}
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
        [(w.Id, w)]         

    List.fold (fun lst i -> makeRandomWire ()@lst) [] [1]
    |> Map.ofList
    |> (fun wires -> {model with WX = wires; BB = createBB wires}, Cmd.none)

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        let sm, sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol = sm }, Cmd.map Symbol sCmd
    | AddWire _ -> failwithf "Not implemented"
    | SetColor c -> failwithf "Not implemented"
    | MouseMsg mMsg -> 
        match mMsg.Op with
        | Drag -> model, Cmd.ofMsg (Symbol(Symbol.MouseMsg mMsg))        
        | _ -> model, Cmd.ofMsg (Symbol(Symbol.MouseMsg mMsg))
    | SetSelected wMsg -> 
        let wxUpdated = (setSelectedColor model wMsg)
        {model with WX = wxUpdated}, Cmd.none
    | UnselectAll -> {model with WX = setUnselectedColor model}, Cmd.none
    | Dragging (wMsgId, wMsgPos)-> 
        let wxUpdated = manuelRouting model wMsgId wMsgPos
        //let wxUpdated = updateSelectedWires model wMsgId
        {model with WX = wxUpdated}, Cmd.none
    | StartDragging (wMsgId, wMsgPos) -> 
        let oldWX = model.WX
        let wxUpdated = 
            startDrag model wMsgId wMsgPos 
        printf $"{(oldWX <> wxUpdated)}"
        let bb = createBB wxUpdated
        {model with WX = wxUpdated; SelectedWire = Some wMsgId; BB = bb}, Cmd.none
    | EndDragging -> 
        let wXUpdated = endDrag model
        (* let wxModel = updateSelectedWires model (Map.toList model.WX |> List.map (fun (a,b)-> a) *)
        {model with WX = wXUpdated; SelectedWire = None }, Cmd.none
        
        


//---------------Other interface functions--------------------//

/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Used
/// to determine which wire (if any) to select on a mouse click

let getTargetedWire (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId Option  =
    let isTarget w =
        match ptCloseToWire pos w with
        | true -> [(w.Id, w)]
        | false -> []
       
    let closestWire (lst: (CommonTypes.ConnectionId * Wire) List): CommonTypes.ConnectionId =
        List.map (fun (wId, w) -> (wId, distPtToWire pos w)) lst
        |> List.maxBy (fun (w, dist) -> -dist)
        |> fst
    
    let wiresCloseToPt = Map.fold (fun lst kWire vWire -> (isTarget vWire)@lst) [] wModel.WX
    
    match wiresCloseToPt with
    | [(wId, w)] -> 
        Some wId
    | [] -> None
    | lst -> 
        Some (closestWire lst)


let getWiresInTargetBBox (wModel: Model) (bbox: BBox) : CommonTypes.ConnectionId list =
    let wireInBBox (w: Wire) =
        List.exists (fun vert ->
            ptInBB vert bbox) w.WireSegments

    Map.fold (fun lst k w -> if wireInBBox w then [k]@lst else lst) [] wModel.WX
//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId: CommonTypes.ComponentId): CommonTypes.Component = failwithf "Not implemented"

let extractWires (wModel: Model): CommonTypes.Component list = failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp: CommonTypes.Component) = failwithf "Not Implemented"
