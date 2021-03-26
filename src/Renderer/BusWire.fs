﻿module BusWire

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

type Error =
    {
        Id: ConnectionId
        Msg: string
        Pos: XYPos
    }

type Direction = 
    | Vertical
    | Horizontal

type WireSegment =
    {
        StartPos: XYPos
        EndPos: XYPos
        Direction: Direction
    }

type Wire =
    {
        Id: ConnectionId
        SrcPort: PortId
        TargetPort: PortId
        WireColor: HighLightColor
        WireWidth: int
        Error: string Option
        SelectedSegment: SegmentIndex
        LastDragPos: XYPos
        Segments: WireSegment list
        ManualOverride: bool
    }

type Model =
    {
        WX: Map<ConnectionId, Wire>
        WireAnnotation: bool
        Debug: bool
    }

type Msg =
    | DeleteSymbols of CommonTypes.ComponentId list             // Performs Wire Routings/Deletions when Symbols are being Deleted
    | DraggingSymbols of CommonTypes.ComponentId list * BBox    // Performs Wire Routings when Symbols are being Dragged
    | AddWire of (PortId * PortId)                              // Create a Wire between Two Ports (Must be Ports of Opposite Polarity)
    | DeleteWire of ConnectionId                                // Deletes a Wire
    | StartDrag of wId: ConnectionId * pos: XYPos               // Starts Dragging a Wire's Segment
    | Dragging of wId: ConnectionId * pos: XYPos                // Still Dragging the Wire's Segment
    | EndDrag                                                   // Complete the Dragging Process 
    | RoutingUpdate of BBox                                     // Refreshes Wire Routings
    | Debug                                                     // Enable Wire Bounding Boxes to be Seen (Debug Mode)


type WireRenderProps =
    {
        Key: ConnectionId
        SegPath: string
        WireColor: HighLightColor
        WireWidth: string
    }

type LabelRenderProps =
    {
        key: ConnectionId
        Label: string
        ColorLabel: string
        Pos: XYPos
        BusIdcWidth: float
    }

type SegBBRenderProps =
    {
        key: ConnectionId
        StartPos: XYPos
        EndPos: XYPos
    }

/// Takes as input a relative position between two points and outputs true if the two original points are horzontal and false otherwise
let isVertical (relPos: XYPos): bool =
    abs (relPos.X) <= abs (relPos.Y)

/// Creates a standard bounding box for the wire segements 
/// Takes the start and end position of the wires and widh and returns a bounding box
let createSegBB (startPos: XYPos) (endPos: XYPos) (width: float) : BBox =
    match posDiff endPos startPos with
    // left to right
    | x when x.X > 0. ->
        toBBox (startPos.X - width) (startPos.Y - width) (abs x.X + (width * 2.)) (width * 2.)
    // right to left
    | x when x.X < 0. ->
        toBBox (endPos.X - width) (endPos.Y - width) (abs x.X + (width * 2.)) (width * 2.)
    // top to bottom
    | x when x.Y > 0. ->
        toBBox  (startPos.X - width) (startPos.Y - width) (width * 2.) (abs x.Y + (width * 2.))
    // bottom to top
    | x when x.Y < 0. ->
        toBBox (endPos.X - width) (endPos.Y - width) (width * 2.) (abs x.Y + (width * 2.))
    // failsafe case with no bounding box
    | _ ->
        toBBox 0. 0. 0. 0.

/// checks the width of the given ports and checks if the connection is valid. 
/// Takes the port ids of the two ports of interest as well as the current symbol model. Returns a result of either the correct width or error string
let checkPortWidths (sModel: Symbol.Model) (srcPortId: PortId) (tgtPortId: PortId) : Result<int, string> =
    let srcSym =
        Symbol.findPort sModel srcPortId
        |> Symbol.findSymbolFromPort sModel

    let tgtSym = 
        Symbol.findPort sModel tgtPortId
        |> Symbol.findSymbolFromPort sModel

    let getWidth pId = Symbol.portWidth sModel pId
    let bits p = if p < 2 then $"{p} bit" else $"{p} bits"

    let widthMatching =
        match getWidth srcPortId, getWidth tgtPortId with
        | Some x, _ when x<=0 ->
            Error $"Invalid connection! Invalid Driver"
        | Some pW1, Some pW2 when pW1 <> pW2 ->
            Error $"Invalid connection! Mismatched wire widths [{bits pW1}, {bits pW2}]"
        | None, Some w ->
            Error $"Invalid connection! Mismatched wire widths [None, {bits w}]"
        | Some w, None ->
            Error $"Invalid connection! Mismatched wire widths [None, {bits w}]"
        | Some w, _ -> Ok w
        | _, _ -> failwithf "Should not occur"

    match tgtSym.Component.Type with
    | MergeWires | IOLabel ->
        match srcSym.Id with
        | x when x = tgtSym.Id -> Error $"Invalid connection! Can't connect to itself"
        | _ -> widthMatching   
    | SplitWire n ->
        let srcPortWidth = Symbol.portWidth sModel srcPortId
        
        match srcPortWidth with
        | Some width when width <= n -> 
            let m = n + 1
            Error $"Invalid connection! Input connection requires min. {bits m}"
        | _ -> widthMatching
    | _ -> widthMatching

///Takes the current wire model and a wire Id and returns the wire
let findWire (wModel: Model) (wId: ConnectionId): Wire =
    match Map.tryFind wId wModel.WX with
    | Some vWire -> vWire
    | None -> failwithf "Invalid Id passed"

/// Takes two position and returns ture if the positions are the same and false otherwise
let isSegmentAtPort (pos1: XYPos) (pos2: XYPos) =
    if pos1 = pos2 then true else false

/// Checkes if a position is inside a wire segement bounding box
/// Takes ```width``` for the desired width for the bounding box, the position of interest and the start and endpos of the wire segment
let ptCloseToSeg (width: float) (pos:XYPos) (startPt: XYPos) (endPt: XYPos): bool =
        createSegBB startPt endPt width |> (containsPoint pos)

/// finds the closest wire segment of a given wire to mouse position
/// Takes a wire and the position of interest and returns the index of the wire segement of the segment which is closest to the given position
/// NOTE: the function fails if the position is not inside the bounding box of any of the segements of the given wire 
/// - i.e. should only be called on a selected wire
let findClosestSegment (wire: Wire) (pos: XYPos) : SegmentIndex =
    let index =
        wire.Segments
        |> List.tryFindIndex (fun s -> ptCloseToSeg 5. pos s.StartPos s.EndPos )

    match index with
    | Some x ->  
        x
    | None ->      
        failwithf $"This shouldn't happen! - WireSegements: {wire.Segments} and pos: {pos}"

/// Creates deafult wire segment.
/// Given a start position and an end position returns a WireSegement record
let makeWireSegment (startPos: XYPos) (endPos: XYPos) : WireSegment =
    let direction =
        match isVertical (posDiff startPos endPos) with
        | true -> Vertical
        | false -> Horizontal
        
    {
        StartPos = startPos
        EndPos = endPos
        Direction = direction
    }

///Checks if two bounding boxes are vertically overlapping. 
/// Takes two boudning boxes and returns ```true``` if the y-coordinates of the two boxes intersect at any point
let verticalOverlap (box1: BBox) (box2: BBox) = 
    let isAbove (bb1: BBox) (bb2: BBox) = (bb1.Pos.Y + bb1.Height) <= bb2.Pos.Y
    not (isAbove box1 box2 || isAbove box2 box1)

/// Adjusted the start and end position of each wire segments so that the segment connects.
/// Given a segement list, it returns another segement list where start position of the next segement matches the end position of the current segement
/// (the first and last segement only connects at their end position and start position respectivly)
let autoConnect (segList: WireSegment list) : WireSegment list =
    let adjSeg (index: int) : WireSegment =
        match List.tryItem index segList with
        | Some x -> x
        | None -> failwithf "Indexing error!"

    segList
    |> List.mapi (fun i s ->
        match i with
        | x when x = 0 ->
            {s with EndPos = (adjSeg (i+1)).StartPos}
        | x when x = (segList.Length - 1) ->
            {s with StartPos = (adjSeg (i-1)).EndPos}
        | x when (segList.Length = 5) && (x = 1) ->
            {s with EndPos = posOf s.EndPos.X (adjSeg (i+1)).StartPos.Y}
        | x when (segList.Length = 5) && (x = 2) ->
            {s with
                StartPos = posOf (adjSeg (i-1)).EndPos.X s.StartPos.Y
                EndPos = posOf (adjSeg (i+1)).StartPos.X s.StartPos.Y
            }
        | x when (segList.Length = 5) && (x = 3) ->
            {s with StartPos = posOf s.StartPos.X (adjSeg (i-1)).EndPos.Y}
        | _ -> s
    )

/// Smart routing algorithm. 
/// Takes the current symbol model, wire of interest and segement list and uses a recursing algorithem
/// to find the short path while avoiding collsions with symbols. Returns the updated ```wireSegement lis``` 
let smartRouting (sModel: Symbol.Model) (wire: Wire) (segList: WireSegment list) : WireSegment list =
    let srcPortPos = Symbol.portPos sModel wire.SrcPort
    let tgtPortPos = Symbol.portPos sModel wire.TargetPort
    let avgPortPos = midPt srcPortPos tgtPortPos
    let halfPortDist = posHalve (posDiff tgtPortPos srcPortPos)

    // checks if bounding boxes of wire segment and symbol overlap
    let collision (seg: WireSegment) : bool =
        let segBBox = createSegBB seg.StartPos seg.EndPos (gridSize / 2.)

        Symbol.getAllSymbols sModel
        |> List.map (Symbol.getSymbolFromSymbolId sModel)
        |> List.exists (fun sym -> overlaps segBBox (Symbol.symbolBBox sModel sym.Id))

    /// reroute segments recursively
    let rec avoid (seg: WireSegment) (index: int) (dir: bool) (d: int) : WireSegment =
        let offset = if dir then gridSize else -gridSize

        match d with
        | x when x > 260 -> seg
        | _ ->
            match collision seg with
            | true ->
                let newSeg =
                    match seg.Direction with
                    | Horizontal ->
                        {seg with
                            StartPos = posAddY seg.StartPos offset
                            EndPos = posAddY seg.EndPos offset
                        }
                    | Vertical ->
                        {seg with
                            StartPos = posAddX seg.StartPos offset
                            EndPos = posAddX seg.EndPos offset
                        }

                avoid newSeg index dir (d + 1)
            | false -> seg

    /// bidirectional recursion to shorten path
    let rec routing (depth: int) (newSegList: WireSegment list) : WireSegment list =
        match depth with
        | x when x > 1 -> newSegList
        | _ ->
            match (List.exists (fun seg -> collision seg) newSegList) || (depth = 0) with
            | true ->
                newSegList
                |> List.mapi (fun i s ->
                    match i with
                    | x when (x = 0) || (x = newSegList.Length - 1) -> s
                    | 1 ->
                        match newSegList.Length with
                        | 3 ->
                            let newSeg =
                                {s with
                                    StartPos = snapToGrid (posOf avgPortPos.X s.StartPos.Y) 
                                    EndPos = snapToGrid (posOf avgPortPos.X s.EndPos.Y)
                                }

                            let segLeft = avoid newSeg i false 0
                            let segRight = avoid newSeg i true 0
                            let distLeft = abs (avgPortPos.X - segLeft.StartPos.X)
                            let distRight = abs (avgPortPos.X - segRight.StartPos.X)
                            let clearance = abs halfPortDist.X

                            match distLeft, distRight with
                            | x, y when (x > clearance) && (y > clearance) -> newSeg
                            | x, y when (x <= clearance) && (y > clearance) -> segLeft
                            | x, y when (x > clearance) && (y <= clearance) -> segRight
                            | _ ->
                                match collision segList.Head, collision (List.last segList) with
                                | true, false -> segLeft
                                | false, true -> segRight
                                | _ -> if distLeft <= distRight then segLeft else segRight

                        | _ ->
                            let newSeg =
                                {s with
                                    StartPos = posOf (srcPortPos.X + gridSize) s.StartPos.Y
                                    EndPos = posOf (srcPortPos.X + gridSize) s.EndPos.Y
                                }

                            avoid newSeg i true 0
                    | 2 ->
                        let newSeg =
                            {s with
                                StartPos = snapToGrid (posOf s.StartPos.X avgPortPos.Y)
                                EndPos = snapToGrid (posOf s.EndPos.X avgPortPos.Y)
                            }

                        let segUp = avoid newSeg i false 0
                        let segDown = avoid newSeg i true 0
                        let distUp = abs (avgPortPos.Y - segUp.StartPos.Y)
                        let distDown = abs (avgPortPos.Y - segDown.StartPos.Y)

                        if distUp <= distDown then segUp else segDown
                    | _ ->
                        let newSeg =
                            {s with
                                StartPos = posOf (tgtPortPos.X - gridSize) s.StartPos.Y
                                EndPos = posOf (tgtPortPos.X - gridSize) s.EndPos.Y
                            }

                        avoid newSeg i false 0
                )
                |> autoConnect
                |> routing (depth + 1)
            | false -> newSegList

    routing 0 segList
///Makes the inital auto routing of a wire.
///Takes the current symbol model and the wire and 
/// returns a list of wire segements which are defined based on the relative position between the wires input and output port
let autoRoute (sModel: Symbol.Model) (wire: Wire) : WireSegment list =
    let startPos = Symbol.portPos sModel wire.SrcPort
    let endPos = Symbol.portPos sModel wire.TargetPort
    let midPos = midPt startPos endPos

    let wireDir = posDiff endPos startPos

    let srcHost = Symbol.symbolBBox sModel (Symbol.getHostId sModel wire.SrcPort)
    let tgtHost = Symbol.symbolBBox sModel (Symbol.getHostId sModel wire.TargetPort)

    let vAdj = 
        match verticalOverlap srcHost tgtHost with
        | true when srcHost.Pos.Y <= tgtHost.Pos.Y -> tgtHost.Pos.Y + tgtHost.Height + 20.
        | true -> srcHost.Pos.Y + srcHost.Height + 20.
        | false -> midPos.Y

    let defSeg pos vPos hPos =
        match wireDir.X, wireDir.Y with
        | x, y when (x >= 40.) || (x < 40. && y = 0.) ->
            [
                pos
                posOf midPos.X pos.Y
            ]
            |> List.map snapToGrid
        | _ ->
            [
                pos
                posOf (pos.X + hPos) pos.Y
                posOf (pos.X + hPos) vPos
            ]
            |> List.map snapToGrid

    let initialSegs, finalSegs =
        defSeg startPos vAdj 20., List.rev (defSeg endPos vAdj -20.)

    initialSegs @ finalSegs
    |> List.pairwise
    |> List.map (fun (startPos, endPos) -> makeWireSegment startPos endPos)

///Takes two port ids and checks if they are of opposite type - i.e. one output and one input
/// Returns a result record of either a tuple with the given ids if the types are correct or an error string
let typesValid (port1: PortId, port2: PortId) (sModel: Symbol.Model) : Result<PortId * PortId, string> =
    let getType pId = (Symbol.portType sModel pId)

    match getType port1, getType port2 with
    | pT1, pT2 when pT1 = pT2 -> Error $"Invalid connection! Ports cannot both be {pT1}s."
    | PortType.Output, PortType.Input -> Ok (port1, port2)
    | PortType.Input, PortType.Output -> Ok (port2, port1)
    | _ -> failwithf "Invalid connection!"

/// Checks if an input has multiple wires connected to it.
/// Given the current wire model, a wire id and an id of the input port.
/// Returns true if there is already another wire connected and false otherwise
let notAvaliableInput (wModel: Model) (wId: ConnectionId) (inputId: PortId): bool =
    Map.exists (fun _ w -> (w.Id <> wId) && (w.TargetPort = inputId)) wModel.WX

/// Given the current wire and symbol model, two port ids and connection id option creates 
/// a wire if the connection is valid i.e. the two ports of are not of the same time otherwise returns None.
/// If suplied with an connection id it creates the wire with the given id (logically a wire which exist has been connected to a new port) otherwise generates a new id.
let createWire (wModel: Model) (sModel: Symbol.Model) (port1: PortId) (port2: PortId) (conId: ConnectionId Option) : Wire option =
    let createId =
        function
        | Some s -> s
        | None -> ConnectionId(uuid ())

    let wId = createId conId

    let widthValid = checkPortWidths sModel port1 port2 

    match typesValid (port1, port2) sModel with
    | Ok (s, t) ->
        let src, tgt, width, colour, err =
            match widthValid with
            | _ when (notAvaliableInput wModel wId t) ->
                s, t, 5, Red,
                Some "Invalid connection! Input ports cannot have multiple wires"
            | Ok w when w < 2 ->
                s, t, 3, Blue, None
            | Ok _ ->
                s, t, 5, Purple, None
            | Error errStr ->
                s, t, 5, Red, Some errStr

        Some {
            Id = wId
            SrcPort = src
            TargetPort = tgt
            Segments = []
            LastDragPos = posOf 0.0 0.0
            WireColor = colour
            WireWidth = width
            Error = err
            SelectedSegment = -1
            ManualOverride = false
        }
    | _ -> None

/// Takes current symbol and wire model and a bounding box and returns a list of 
/// all the connection ids of the wires which exist in the given boudning box
let wiresInScreen (wModel: Model) (sModel: Symbol.Model) (bbox: BBox) : ConnectionId list =
    wModel.WX
    |> Map.filter (fun _ w ->
        let srcPos = Symbol.portPos sModel w.SrcPort
        let tgtPos = Symbol.portPos sModel w.TargetPort

        (containsPoint srcPos bbox) || (containsPoint tgtPos bbox)
    )
    |> Map.toList
    |> List.map fst


/// Given the current wire and symbol models, a list of the symbols of interest and a bounding box
/// updates all wires which have not been manual modified previously (i.e. manual overrid set to false) in the given bounding box.
/// Also smartRoutes the wires which are connected to the symbol with the given ids, irrelevent if they have been manually modified (resets manual overrid to false).
/// Returns a list of the updated wire map 
let updateSymWires (wModel: Model) (sModel: Symbol.Model) (symIds: ComponentId list) (bbox: BBox) : Map<ConnectionId, Wire> =
    let pIds =
        symIds
        |> List.fold (fun acc symId -> acc @ Symbol.getPortsFromSymbols sModel [symId]) []

    let visibleWires = wiresInScreen wModel sModel bbox
    
    wModel.WX
    |> Map.map (fun _ w ->
        match List.contains w.Id visibleWires with
        | true ->
            match List.contains w.SrcPort pIds || List.contains w.TargetPort pIds with
            | true ->
                let segList = autoRoute sModel w
                {w with
                    Segments = smartRouting sModel w segList
                    ManualOverride = false
                }
            | false ->
                match w.ManualOverride with
                | true -> w
                | false -> {w with Segments = smartRouting sModel w w.Segments}
        | false -> w
    )

/// Given a wire returns the string which contains the points formated to give the apporiate SVG path element
let pathDefString (w: Wire) =  
    let relMove (startSeg: WireSegment) (endSeg: WireSegment) =
        let adjPos = 
            match posDiff startSeg.StartPos endSeg.EndPos with
            | relPos when ((abs relPos.Y) <1.) || ((abs relPos.X) <1.)->  [posOf 0. 0.; posOf 0. 0.]      
            | relPos when (startSeg.Direction = Horizontal) && (relPos.Y >0.) && (relPos.X > 0.) ->  [{X = -5.; Y = 0.};  {X = 0.; Y = 5.}]
            | relPos when (startSeg.Direction = Horizontal) && (relPos.Y > 0.) -> [{X = 5.; Y = 0.};  {X = 0.; Y = 5.}]
            | relPos when (startSeg.Direction = Horizontal) && (relPos.X > 0.) -> [{X = -5.; Y = 0.};  {X = 0.; Y = -5.}]
            | relPos when startSeg.Direction = Horizontal -> [{X = 5.; Y = 0.};  {X = 0.; Y = -5.}]
            | relPos when (relPos.Y > 0.) && (relPos.X > 0.) -> [{X = 0.; Y = -5.};  {X = 5.; Y = 0.}]           
            | relPos when (relPos.Y > 0.) -> [{X = 0.; Y = -5.}; {X = -5.; Y = 0.}]
            | relPos when relPos.X > 0. -> [{X = 0.; Y = 5.}; {X = 5.; Y = 0.}]
            | relPos ->  [{X = 0.; Y = 5.}; {X = -5.; Y = 0.}]
        List.map2 posDiff [startSeg.EndPos; endSeg.StartPos] adjPos

    let strLst =  
        w.Segments  
        |> List.mapi (fun idx (s: WireSegment) -> 
            match idx with 
            | idx when idx = 0  -> [s.StartPos]@ (relMove s w.Segments.[1]) @ [s.EndPos]
            | idx when idx = (List.length w.Segments - 1) ->  [s.EndPos]
            | idx ->  (relMove s w.Segments.[idx+1]) @ [s.EndPos]
            | _ -> failwithf "unexpected index {idx} list.length: {List.length w.Segments}"
            )  
        |> List.mapi (fun idx lstPos ->
            match lstPos with
            | [start; startCorner; endCorner; midPos] when idx = 0 -> $" M {start.X} {start.Y} L {startCorner.X} {startCorner.Y} Q {midPos.X} {midPos.Y} {endCorner.X} {endCorner.Y} L"
            | [endPos]  when idx = (List.length w.Segments - 1) -> $" {endPos.X} {endPos.Y} " 
            | [ startCorner; endCorner; midPos] -> $"{startCorner.X} {startCorner.Y} Q {midPos.X} {midPos.Y} {endCorner.X} {endCorner.Y} L "
            | _ -> failwithf "Something has gone wrong in coordinate generation"
            )
    
    List.fold (fun str s -> str + s) "" strLst

let singleWireView =
    FunctionComponent.Of
        (fun (props: WireRenderProps) ->
            g [] [
                path 
                    [ 
                        SVGAttr.D props.SegPath
                        SVGAttr.Stroke (props.WireColor.ToString())
                        SVGAttr.FillOpacity 0
                        SVGAttr.StrokeWidth props.WireWidth
                    ] []
            ]
        )
/// Given the current wire model, the id of the selected wire and the position updatesthe selected segements and the connected segements
/// according to the relative movement given by the ```pos``` and the ```lastPos```.
/// Returns a wire with the updated ```Segements```
let manualRouting (wModel: Model) (wId: ConnectionId) (pos: XYPos): Wire =
    let wire = findWire wModel wId
    let diff = snapToGrid (posDiff pos wire.LastDragPos)

    let seg =
        match List.tryItem wire.SelectedSegment wire.Segments with
        | Some x -> x
        | None -> failwithf "Invalid index!"

    let offset =
        match seg.Direction with
        | Horizontal -> posOf 0. diff.Y
        | Vertical -> posOf diff.X 0.

    let updatedSegs =
        wire.Segments
        |> List.mapi (fun i s ->
            match wire.SelectedSegment with
            | index when ((index = i) && (index = 0)) ->
                {s with
                    StartPos = posAdd seg.StartPos diff 
                    EndPos =  posAdd seg.EndPos offset 
                }
            | index when ((index = i) && (index = (wire.Segments.Length  - 1))) ->
                {s with
                    StartPos = posAdd seg.StartPos offset 
                    EndPos = posAdd seg.EndPos diff
                }
            | index when index = i ->
                {s with
                    StartPos = posAdd  seg.StartPos offset
                    EndPos = posAdd seg.EndPos offset
                }
            | index when (index - 1) = i ->
                {s with EndPos = posAdd seg.StartPos offset}
            | index when (index + 1) = i ->
                {s with StartPos = posAdd seg.EndPos offset}
            | _ -> s
        )

    {
        wire with
            Segments = updatedSegs
            LastDragPos = pos
            ManualOverride = true
    }

/// Given the current wire and symbol models, the ```index``` of the segement of interest, the current postion of the segments ending of interest,
/// the id of the port to which the connection should be and the wire. Returns an updated with with a segements adjusted to ensure that the endpoint of the wires sits on the port.
let fitConnection (wModel: Model) (sModel: Symbol.Model) (index: SegmentIndex) (segCurrentPos: XYPos) (newPortId: PortId)  (wire: Wire) : Wire = 
    let updatedWire = {wire with SelectedSegment = index; LastDragPos = segCurrentPos; Segments = wire.Segments}
    manualRouting {wModel with WX = Map.add updatedWire.Id updatedWire wModel.WX} updatedWire.Id (Symbol.portPos sModel newPortId)


/// Given the current wire and symbol model and a wire. Checks that the port ids of the wire are correctly updated to the current position of the wire endings.
/// Returns a wire with correctly updated ```portId``` and routed segments.
let checkPortConnections (wModel: Model) (sModel: Symbol.Model) (wire: Wire) : Wire =
    match wire.SelectedSegment with
    | x when x = 0 || x = wire.Segments.Length - 1 ->
        let srcSegId = 0
        let tgtSegId = wire.Segments.Length - 1

        match Symbol.getTargetedPort sModel (wire.Segments.[srcSegId]).StartPos, Symbol.getTargetedPort sModel (wire.Segments.[tgtSegId]).EndPos with
        | Some srcPId, Some tgtPId when srcPId = wire.SrcPort && tgtPId = wire.TargetPort ->
            fitConnection wModel sModel srcSegId (wire.Segments.[srcSegId]).StartPos srcPId wire
            |> fitConnection wModel sModel tgtSegId (wire.Segments.[tgtSegId]).EndPos tgtPId
        | Some srcPId, Some tgtPId ->
            let updatedModel = {wModel with WX = Map.remove wire.Id wModel.WX} //to ensure it does not get a too many wire for input port validation error triggered by itself
            
            match createWire updatedModel sModel srcPId tgtPId (Some wire.Id) with
            | Some updatedWire ->
                let updatedSegs = autoRoute sModel updatedWire
                {updatedWire with Segments = smartRouting sModel updatedWire updatedSegs}
            | None ->
                fitConnection wModel sModel srcSegId (wire.Segments.[srcSegId]).StartPos wire.SrcPort wire
                |> fitConnection wModel sModel tgtSegId (wire.Segments.[tgtSegId]).EndPos wire.TargetPort
        | None, Some _ ->
            fitConnection wModel sModel srcSegId (wire.Segments.[srcSegId]).StartPos wire.SrcPort wire
        | Some _, None ->
            fitConnection wModel sModel tgtSegId (wire.Segments.[tgtSegId]).EndPos wire.TargetPort wire
        | _ -> failwithf "Stray wire!"
    | _ -> wire

/// ???? 
let updateWireValidity (wModel: Model) (sModel: Symbol.Model) (wire: Wire): Wire =
    match createWire wModel sModel wire.SrcPort wire.TargetPort (Some wire.Id) with
    | Some newWire -> 
        {newWire with
            Segments = wire.Segments
            ManualOverride = wire.ManualOverride
        }
    | None -> wire

/// Takes the current wire and symbol model and goes through all the wire connection to update the wire instances for if manual reconnectins have been made
/// and ensure that all wires are have the correctly routed segements and valid connections.
let updateConnections (wModel: Model) (sModel: Symbol.Model) : Map<ConnectionId, Wire> =
    let updatedWX = Map.map (fun _ w -> checkPortConnections wModel sModel w) wModel.WX
    let updatedModel = {wModel with WX = updatedWX}

    updatedWX
    |> Map.map (fun _ w -> updateWireValidity updatedModel sModel w)

/// Given the current symbol and wire models and two port ids creates a new wire instance, routes it and checks the validitiy of the wire
/// Returns an updated ```Model.WX```  map which includes the new connection if the connection was valid.
let addWire (wModel: Model) (sModel: Symbol.Model) (port1: PortId) (port2: PortId) : Map<ConnectionId, Wire> =
    match createWire wModel sModel port1 port2 None with
    | Some wire -> 
        let updatedSegs = autoRoute sModel wire
        let updatedWX = Map.add wire.Id {wire with Segments = smartRouting sModel wire updatedSegs} wModel.WX
        let updatedModel = {wModel with WX = updatedWX}

        updatedWX
        |> Map.map (fun _ w -> updateWireValidity updatedModel sModel w)
    | None -> wModel.WX

///Given a connectionId deletes the given wire and returns the updated ``Model.WX```  map
let deleteWire (wModel: Model) (sModel: Symbol.Model) (wId: ConnectionId) : Map<ConnectionId, Wire> =
    let updatedModel = {wModel with WX = Map.remove wId wModel.WX}
    updatedModel.WX
    |> Map.map (fun _ w -> updateWireValidity updatedModel sModel w)

 ///  Given a list of symbol ids deletes all the wires connnected to the symbol, i.e. removes them from ``Model.WX```  map
let deleteWiresOfSymbols (wModel: Model) (sModel: Symbol.Model) (sIdLst: ComponentId list) : Map<ConnectionId, Wire> =
    let pIdList = Symbol.getPortsFromSymbols sModel sIdLst

    let updatedWX =
        wModel.WX
        |> Map.filter (fun _ v ->
            match List.contains v.SrcPort pIdList, List.contains v.TargetPort pIdList with
            | false, false -> true
            | _ -> false
        )

    let updatedModel = {wModel with WX = updatedWX}

    updatedWX
    |> Map.map (fun _ w -> updateWireValidity updatedModel sModel w)

 /// Given the wire and symbol model and a list of symbol ids returns a map of all wires (organised by ids) which has at least one end connected to one of the symbols by the given ```sIdLst``` ids  
let getWiresOfSymbols (wModel: Model) (sModel: Symbol.Model) (sIdLst: ComponentId list) : Map<ConnectionId, Wire> =
    let pIdList = Symbol.getPortsFromSymbols sModel sIdLst

    wModel.WX
    |> Map.filter (fun _ v ->
        match List.contains v.SrcPort pIdList, List.contains v.TargetPort pIdList with
        | false, false -> false
        | _ -> true
    )

 ///Given the wire and symbol models and symbol id list returns a map (organised by ids) of all the wires which create a connation between any two symbols (or one symbol connected to iteslf)
let getConnectedWires (wModel: Model) (sModel: Symbol.Model) (sIdLst: ComponentId list) : Map<ConnectionId, Wire> =
    let pIdList = Symbol.getPortsFromSymbols sModel sIdLst

    wModel.WX
    |> Map.filter (fun _ v ->
        match List.contains v.SrcPort pIdList, List.contains v.TargetPort pIdList with
        | true, true -> true
        | _ -> false
    )

/// Update the colour on the given wire
let setWireColor (wModel: Model) (wId: ConnectionId) (c: HighLightColor): Wire =
    {findWire wModel wId with WireColor = c}

/// Given a wire returns the correct colour depending on its attribute
let getWireColor (w: Wire): HighLightColor =
    match w with
    | w when w.Error <> None -> Red
    | w when  w.WireWidth < 4 -> Blue
    | _ -> Purple

 /// Gvien the wire model, a wire id and a position updates the ``Model.WX```  map such that the wire with the given id is set up to start dragging correctly
let startDrag (wModel: Model) (wId: ConnectionId) (pos: XYPos) : Map<ConnectionId, Wire> =
    let wire = findWire wModel wId

    let selectedSeg = findClosestSegment wire pos
    let updatedSegs = 
        match selectedSeg with 
        | idx when idx = 0 -> [{wire.Segments.[0] with StartPos = (posOf pos.X wire.Segments.[0].StartPos.Y)}] @ (List.tail wire.Segments)
        | idx when idx = List.length wire.Segments - 1 -> 
            let segRev = List.rev wire.Segments
            [{segRev.[0] with EndPos = (posOf pos.X segRev.[0].EndPos.Y )}] @ (List.tail segRev) |> List.rev
        | _ -> wire.Segments
    Map.add wire.Id {
        wire with
            SelectedSegment = selectedSeg
            LastDragPos = pos
            Segments = updatedSegs
        } wModel.WX
/// Given the wire model, a wire id and position returns ``Model.WX```  map with the wire with the given ids wire segemetns updated relative to the ```pos```
let dragging (wModel: Model) (wId: ConnectionId) (pos: XYPos) : Map<ConnectionId, Wire> =
    wModel.WX
    |> Map.add wId (manualRouting wModel wId pos)

/// Given the wire and symbol model goes through the wire model to ensure all wires are correcly routed and update the attributes accrodingly
/// Returns the updated ``Model.WX```  map
let endDrag (wModel: Model) (sModel: Symbol.Model) : Map<ConnectionId, Wire> =
    updateConnections wModel sModel

let singleSegBBView =
    FunctionComponent.Of
        (fun (props: SegBBRenderProps) ->
            let segBBox = createSegBB props.StartPos props.EndPos gridSize

            g [] [
                rect
                    [
                        X segBBox.Pos.X
                        Y segBBox.Pos.Y
                        Rx 5.
                        Ry 5.
                        SVGAttr.Width segBBox.Width
                        SVGAttr.Height segBBox.Height
                        SVGAttr.StrokeWidth "0.75px"
                        SVGAttr.Stroke "Black"
                        SVGAttr.FillOpacity 0
                    ] []
            ]
        )

let singleLabelView =
    FunctionComponent.Of
        (fun (props: LabelRenderProps) ->
            g [][
                text [ 
                        X props.Pos.X
                        Y props.Pos.Y
                        
                        Style [ 
                                TextAnchor "left"
                                DominantBaseline "middle"
                                FontSize "14px"
                                Fill props.ColorLabel
                                UserSelect UserSelectOptions.None] ] [
                        str <| sprintf $"{props.Label}"
                ]

                line [
                        X1 (props.Pos.X + 5.) ;
                        Y1 (props.Pos.Y + 20.);
                        X2 (props.Pos.X );
                        Y2 (props.Pos.Y + 5.);
                        SVGAttr.Stroke (props.ColorLabel)
                        SVGAttr.FillOpacity 0
                        SVGAttr.StrokeWidth props.BusIdcWidth
                ][]
            ])

/// Given a segements return an ```XYPos``` whcih gives the postion for a wire label for the given segment
let adjLabelPos (seg: WireSegment) : XYPos =
    match posDiff seg.StartPos seg.EndPos with
    | relDiff when relDiff.X > 10.  ->  posDiff seg.StartPos (posOf 7.5 12.5)
    | _ -> posDiff seg.StartPos (posOf -7.5 12.5)

let view (wModel: Model) (selectedWire: CommonTypes.ConnectionId option) (sModel: Symbol.Model) (dispatch: Dispatch<Msg>) =
    g [] (
        wModel.WX
        |> Map.fold (fun acc _ w ->
            let srcSeg = w.Segments.[0]

            let color =
                match selectedWire with
                | Some wId when wId = w.Id -> Green
                | _ -> w.WireColor

            let (labelProps: LabelRenderProps) = 
                match checkPortWidths sModel w.SrcPort w.TargetPort with 
                    | Ok width -> 
                        {
                            key = w.Id
                            Pos = adjLabelPos srcSeg 
                            ColorLabel = color.ToString()
                            Label = $"%d{width}"
                            BusIdcWidth = 2. 
                        }
                    | Error _ -> 
                        {
                            key = w.Id
                            Pos = adjLabelPos srcSeg  
                            ColorLabel = color.ToString()
                            Label = $"x"
                            BusIdcWidth = 2. 
                        }
            
            let (props: WireRenderProps) =
                {
                    Key = w.Id
                    SegPath = (pathDefString w)
                    WireColor = color
                    WireWidth = $"%d{w.WireWidth}"
                }

            

            let segBBox =
                w.Segments
                |> List.fold (fun acc s ->
                    let (segBBProps: SegBBRenderProps) =
                        {
                            key = ConnectionId (uuid())
                            StartPos = s.StartPos
                            EndPos = s.EndPos
                        }
                    acc @ [singleSegBBView segBBProps]
                ) []
                
            acc 
            @ if wModel.WireAnnotation && (w.WireWidth > 3) then [(singleLabelView labelProps)] else []
            @ if wModel.Debug then segBBox else []
           
            @ [singleWireView props]
            
        ) [] ) 
        

/// Given the wire and symbol models and a bouding box updates the routing of all wires which has ```ManualOVerride``` set to false
/// Returns the updated ``Model.WX```  map
let routingUpdate (wModel: Model) (sModel: Symbol.Model) (bbox: BBox) : Map<ConnectionId, Wire> =
    let visibleWires = wiresInScreen wModel sModel bbox

    wModel.WX
    |> Map.map (fun _ w ->
        match List.contains w.Id visibleWires with
        | true ->
            match w.ManualOverride with
            | false ->
                {w with Segments = smartRouting sModel w w.Segments}
            | true -> w
        | false -> w
    )


let update (msg: Msg) (model: Model) (sModel: Symbol.Model): Model * Cmd<Msg> =
    match msg with
    | DeleteSymbols sIdLst ->
        { model with WX = deleteWiresOfSymbols model sModel sIdLst }, Cmd.none
    | DraggingSymbols (sIdLst, bbox) ->
        { model with WX = updateSymWires model sModel sIdLst bbox }, Cmd.none
    | AddWire (wMsgId1, wMsgId2) ->
        let wxUpdated = addWire model sModel wMsgId1 wMsgId2
        { model with WX = wxUpdated }, Cmd.none
    | DeleteWire wMsg ->
        let wxUpdated = deleteWire model sModel wMsg   
        { model with WX = wxUpdated }, Cmd.none
    | Dragging (wMsgId, wMsgPos) ->
        let wxUpdated = dragging model wMsgId wMsgPos
        { model with WX = wxUpdated }, Cmd.none
    | StartDrag (wMsgId, wMsgPos) ->
        let wxUpdated = startDrag model wMsgId wMsgPos
        { model with WX = wxUpdated }, Cmd.none
    | EndDrag ->
        let wxUpdated = endDrag model sModel
        { model with WX = wxUpdated }, Cmd.none
    | RoutingUpdate bbox ->
        { model with WX = routingUpdate model sModel bbox }, Cmd.none
    | Debug ->
        { model with Debug = not model.Debug }, Cmd.none

///Dummy function to initialize for demo
let init (sModel: Symbol.Model) () =
    let rng = System.Random 0
    let inList, outList =
        Symbol.allPortsInModel sModel
        |> Map.toList
        |> List.partition (fun (_, p) -> p.PortType = PortType.Output)

    let inList, outList =
        inList |> List.map fst,
        outList |> List.map fst

    let n = min inList.Length outList.Length

    let model =
        {
            WX = Map.empty
            WireAnnotation = true
            Debug = false
        }
    
    [1..25]
    |> List.fold (fun acc _ ->
        let s1, s2 =
            outList.[rng.Next(0, n-1)],
            inList.[rng.Next(0, n-1)]

        {acc with WX = addWire acc sModel s1 s2}
    ) model
    , Cmd.none

//---------------Other interface functions--------------------//

///Calculates the distance between a point and a two other points which is used to define a start and end point of a wire segment
let distPtToSeg (pt: XYPos) ((startPt, endPt): XYPos * XYPos) = //(wSeg: WireSegment) =
    let ptToPtA = posOf (pt.X - endPt.X) (pt.Y - endPt.Y)
    let ptAToB = posOf (endPt.X - startPt.X) (endPt.Y - startPt.Y)

    let magPtAToPtB = ((ptAToB.X) ** 2.) + ((ptAToB.Y) ** 2.) ** 0.5

    let crossProd =
        ((ptAToB.X) * (ptToPtA.Y))
        - ((ptToPtA.X) * (ptAToB.Y))

    abs (crossProd) / magPtAToPtB

/// Gvien a position and a wire returns the shortest distance between the given point and the wire
let distPtToWire (pt: XYPos) (wire: Wire) =
    wire.Segments
    |> List.map (fun s -> distPtToSeg pt (s.StartPos, s.EndPos))
    |> List.maxBy (~-)

/// Given a position and a wire returns true if the pos lies within the bouding boxes of the wire segements which make up the wire and false otherwise
let isTargetWire (pt: XYPos) (wire: Wire) =
    //let ptCloseToSeg (pos:XYPos) (startPt: XYPos) (endPt: XYPos): bool =
    //    createSegBB startPt endPt 5. |> (containsPoint pos)

    let res =
        wire.Segments
        |> List.tryFindIndex (fun s -> (ptCloseToSeg 5. pt) s.StartPos s.EndPos)

    match res with
    | Some idx ->
        true
    | None -> false

/// Give position finds the wire which is within a few pixels. If there are multiple chooses the closest wire one
/// Returns a ```ConnectionId``` option
let getTargetedWire (wModel: Model) (pos: XYPos): ConnectionId Option =
    let closestWire (lst: (ConnectionId * Wire) list): ConnectionId =
        List.minBy (fun (_, w) -> distPtToWire pos w) lst
        |> fst

    let possibleWires =
        Map.filter (fun _ vWire -> (isTargetWire pos vWire)) wModel.WX
        |> Map.toList

    match possibleWires with
    | [ (wId, _) ] -> Some wId
    | [] -> None
    | lst -> Some(closestWire lst)

/// Given the wire and symbol model returns a list of the type Error
// which contains the id of the wire which has the error a position of the input port of the wire and a string explaining the error
let getErrors (wModel: Model) (sModel: Symbol.Model): Error list =
    Map.fold (fun lst _ w ->
        match w.Error with
        | Some errStr ->
            [{
                Id = w.Id
                Msg = errStr
                Pos = (Symbol.portPos sModel w.TargetPort)
            }] @ lst
        | None -> lst
    ) [] wModel.WX

/// Given the wire model returns a list of all portIds of the input ports which are connected to the given source port (output port)
let getAllPidEnds (wModel: Model) (pIdSrc: PortId) : PortId List =
    wModel.WX
    |> Map.toList
    |> List.filter (fun (_,w) ->
        w.SrcPort = pIdSrc
    )
    |> List.map (fun (_,w) -> w.TargetPort) 


//----------------------interface to Issie-----------------------/
/// Given a list of wire Segments returns a list where each tuple defines one of the vertices which make up the wire segement
let wireSegLstToVerticesLst (wSegs: WireSegment list) : (float * float) list = 
    let incompleteLstVert = List.map (fun (s:WireSegment) -> s.EndPos) wSegs
    [wSegs.[0].StartPos] @ incompleteLstVert
     |> List.map (fun pos -> (pos.X, pos.Y))   

///Convert the information contained in the Wire with the given id to a Issie Connection type instance 
///Given the wire and symbol model and a wire id returns a ```Connection``` instance which has attributes which matches the wire with the given id
let extractWire (wModel: Model) (sModel: Symbol.Model) (wId: ConnectionId) : Connection =
    let wire = findWire wModel wId
   

    {
        Id =  wire.Id.ToString()
        Source = (Symbol.findPort sModel wire.SrcPort)
        Target = (Symbol.findPort sModel wire.TargetPort)
        Vertices = (wireSegLstToVerticesLst wire.Segments)

    }
/// Takes a symbol id and returns a list of Connection which described the Wires connected to the given symbol
let extractSymbolWires (wModel: Model) (sModel: Symbol.Model)  (sId: ComponentId): Connection list = 
    let connectedWires = getConnectedWires wModel sModel  [sId]
    Map.toList connectedWires
    |> List.map (fun (wId, w) -> extractWire wModel sModel wId)

/// Returns a list of Connection which describe all the wire connections in the given model
let extractAllWires (wModel: Model) (sModel: Symbol.Model) : Connection List = 
    wModel.WX
    |> Map.fold (fun connLst wId w -> [extractWire wModel sModel wId]@connLst) []
    

// ---------------------------- End of Issie Interface functions -----------------------//



// ---------------- (Naive) Issie Interface Test Code -----------------------------------------//
(* let getPosPairs (s: WireSegment) = 
     ((s.StartPos.X, s.StartPos.Y), (s.EndPos.X, s.EndPos.Y))

let checkExtractWire (wModel: Model) (sModel: Symbol.Model) (wId: ConnectionId): bool =
    let extractVerts = 
        let conn = extractWire wModel sModel wId
        List.pairwise conn.Vertices
    let modelPairs = 
        wModel.WX.[wId].Segments
        |> List.map (fun s -> getPosPairs s)     
    extractVerts = modelPairs

let checkSymBol (wModel: Model) (sModel: Symbol.Model) (sIdLst: ComponentId list) : bool =
    let extractConns =
        List.fold (fun acc sId -> extractSymbolWires wModel sModel sId @acc) [] sIdLst 
        
    let modelPairs =
        let symPorts = Symbol.getPortsFromSymbols sModel sIdLst 
        Map.filter (fun wId w -> List.contains w.SrcPort symPorts || List.contains w.TargetPort symPorts) wModel.WX
        |> Map.map (fun wId w -> 
            List.map (fun (s:WireSegment) -> getPosPairs s) w.Segments)
    List.forall (fun extract -> (List.pairwise extract.Vertices) = modelPairs.[ConnectionId extract.Id]) extractConns

let checkModelExtract (wModel: Model) (sModel: Symbol.Model) : bool =
    let extractAllConns = extractAllWires wModel sModel

    let modelPairs =
        Map.map (fun wId w -> 
            List.map (fun (s:WireSegment)-> getPosPairs s) w.Segments) wModel.WX

    List.forall (fun extract -> (List.pairwise extract.Vertices) = modelPairs.[ConnectionId extract.Id]) extractAllConns

     *)
//----------------- End of Interface Test Code---------------------------------------------------------//


