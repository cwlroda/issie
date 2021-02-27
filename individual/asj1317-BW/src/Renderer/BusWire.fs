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
      StartPt: XYPos //Left/bottom
      EndPt: XYPos  //Top/Right
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
      SrcPort: string
      TargetPort: string
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




///Creates the wireSegement list which represents the verticies of the wire between the two points
let autoRoute (startPt: XYPos) (endPt: XYPos): WireSegment list =
    let mid = posOf endPt.X startPt.Y

    [ { StartPt = startPt
        EndPt = mid
        Orien = H }
      { StartPt = mid
        EndPt = endPt
        Orien = V } ]

/// Creates a wire instance given the starting Port and the ending Port
let createWire (symLst : Symbol.Model) (srcId: string) (tgtId:string): Wire =
    let srcP = Symbol.portPos symLst srcId
    let tgtP = Symbol.portPos symLst tgtId
    { Id = CommonTypes.ConnectionId(uuid ())
      SrcPort = srcId
      TargetPort = tgtId
      WireSegments = (autoRoute srcP tgtP)
      WireWidth = 2
      WireColor = CommonTypes.Red }


/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    match Map.tryFind wId wModel.WX with
    | Some vWire -> vWire
    | None -> failwithf "Invalid Id passed"
///Creates a BB type given two points and a width and height
/// Assumes that Pos is the mid point of the rectangule which the BBox represents
(* let createBoundingBox (w: int) (h: int) (pos: XYPos): BBox =
    let topLeftPos = 
        posAdd pos (posOf (float -w/2.) (float h/2.))
    {Pos =  topLeftPos; W = w; H = h }
 *)
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


///Takes as input a wire and a point and checks if the pt is within the BBox of any of the segments
/// Returns true if it is close to one of the segments
/// Returns false otherwise
let ptCloseToWire (pt: XYPos) (wire: Wire) =
    let ptCloseToSeg (seg: WireSegment): bool =
        printf $"seg, start pts: {seg.StartPt}, end pt {seg.EndPt}"
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

let updateWire (wModel: Model) (wire: Wire Option) =
    let newPortPos =
        Symbol.portPos wModel.Symbol
    match wire with
    | Some wire -> Some {wire with WireSegments = autoRoute (newPortPos wire.SrcPort) (newPortPos wire.TargetPort)}
    | None -> None 

let updateSelectedWires  (wModel: Model) (wIdLst: CommonTypes.ConnectionId list) (pos: XYPos) =
    let updateModel wId w = 
        match (updateWire wModel w) with
        | Some wire -> Map.add wId wire wModel.WX
        | None -> wModel.WX

    let foldfunc model wId =
        Map.tryFind wId wModel.WX
        |> updateModel wId
    
    List.fold foldfunc wModel.WX wIdLst

    


let updateSegPos (wModel: Model) (wIdLst: CommonTypes.ConnectionId list) (pos: XYPos) =
    failwithf "Not implemented"
    

type WireRenderProps =
    { key: CommonTypes.ConnectionId
      SegLst: WireSegment List
      SrcP: XYPos
      TgtP: XYPos
      ColorP: string
      StrokeWidthP: string }

/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.
///

let wireSegView (vert1: XYPos) (vert2: XYPos) (color: string) (w: string): ReactElement =
    line [ X1 vert1.X
           Y1 vert1.Y
           X2 vert2.X
           Y2 vert2.Y
           // Qualify these props to avoid name collision with CSSProp
           SVGAttr.Stroke color
           SVGAttr.StrokeWidth w ] []


let singleWireView =
    FunctionComponent.Of
        (fun (props: WireRenderProps) ->
            (List.map (fun el -> wireSegView el.StartPt el.EndPt props.ColorP props.StrokeWidthP) props.SegLst
            |> ofList))

let setSelectedColor (wModel: Model) (wID: CommonTypes.ConnectionId) =
    let w = wire wModel wID
    wModel.WX.Add(wID, {w with WireColor = CommonTypes.HighLightColor.Blue})

let setUnselectedColor (wModel: Model) =
    Map.map (fun wId wInst -> {wInst with WireColor = CommonTypes.HighLightColor.Grey}) wModel.WX

let view (model: Model) (dispatch: Dispatch<Msg>) =
    let wires =
        model.WX
        |> Map.toList
        |> List.map
            (fun (wId, w) ->
                let props =
                    { key = wId
                      SegLst = w.WireSegments
                      SrcP = Symbol.portPos model.Symbol w.SrcPort
                      TgtP = Symbol.portPos model.Symbol w.TargetPort
                      ColorP = w.WireColor.Text()
                      StrokeWidthP = "%d{w.WireWidth}px" }

                singleWireView props)

    let symbols =
        Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))

    g [] [ (g [] wires); symbols ]

/// dummy init for testing: real init would probably start with no wires.
/// this initialisation is not realistic - ports are not used
/// this initialisation depends on details of Symbol.Model type.
let init n () =
    let symbols, cmd = Symbol.init ()

    let symIds =
        List.map (fun (sym: Symbol.Symbol) -> sym.Id) symbols

    let rng = System.Random 0

    let makeRandomWire () =
        let n = symIds.Length

        let s1, s2 =
            match rng.Next(0, n - 1), rng.Next(0, n - 2) with
            | r1, r2 when r1 = r2 -> symbols.[r1].Ports, symbols.[n - 1].Ports // prevents wire target and source being same
            | r1, r2 -> symbols.[r1].Ports, symbols.[r2].Ports
        List.allPairs s1 s2
        |> List.fold (fun (lst: Wire list) (p1:CommonTypes.Port, p2:CommonTypes.Port) -> 
                                        match p1,p2 with
                                            | p1, p2 when p1.PortType= p2.PortType -> lst
                                            | p1, p2 when p1.PortType = CommonTypes.PortType.Input -> [createWire symbols p1.Id p2.Id]@lst
                                            | p1, p2 -> [createWire symbols p2.Id p1.Id]@lst
                        ) []
        |> List.map (fun w -> (w.Id, w))
        

         

    List.fold (fun lst i -> makeRandomWire ()@lst) [] [1]
    |> Map.ofList
    |> (fun wires -> { WX = wires; Symbol = symbols }, Cmd.none)

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        let sm, sCmd = Symbol.update sMsg model.Symbol
        { model with Symbol = sm }, Cmd.map Symbol sCmd
    | AddWire _ -> failwithf "Not implemented"
    | SetColor c -> failwithf "Not implemented \{model with Color = c\}, Cmd.none"
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol(Symbol.MouseMsg mMsg))
    | SetSelected wMsg -> 
        let wxUpdated = (setSelectedColor model wMsg)
        {model with WX = wxUpdated}, Cmd.none
    | UnselectAll -> {model with WX = setUnselectedColor model}, Cmd.none
    | StartDragging (wMsgId, wMsgPos)-> 
        let wxUpdated = updateSelectedWires model wMsgId wMsgPos
        {model with WX = wxUpdated}, Cmd.none
    | EndDragging -> failwithf "Not Implemented"
        
        


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
        printf $"wId" 
        Some wId
    | [] -> None
    | lst -> 
        printf $"{lst}"
        Some (closestWire lst)


let getWiresInTargetBBox (wModel: Model) (bbox: BBox) : CommonTypes.ConnectionId list =
    let wireInBBox w =
        List.exists (fun seg ->
            printf $"{seg.StartPt}"
            ptInBB seg.StartPt bbox) w.WireSegments

    Map.fold (fun lst k w -> if wireInBBox w then [k]@lst else lst) [] wModel.WX
//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId: CommonTypes.ComponentId): CommonTypes.Component = failwithf "Not implemented"

let extractWires (wModel: Model): CommonTypes.Component list = failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp: CommonTypes.Component) = failwithf "Not Implemented"
