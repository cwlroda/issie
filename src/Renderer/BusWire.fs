module BusWire

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
    }

type Model =
    {
        WX: Map<ConnectionId, Wire>
        WireAnnotation: bool
        Debug: bool
    }

type Msg =
    | AddSymbol
    | DeleteSymbols of CommonTypes.ComponentId list
    | DraggingSymbols of CommonTypes.ComponentId list
    | EndDragSymbols
    | AddWire of (PortId * PortId)
    | DeleteWire of ConnectionId
    | SetColor of color: HighLightColor
    | StartDrag of wId: ConnectionId * pos: XYPos
    | Dragging of wId: ConnectionId * pos: XYPos
    | EndDrag
    | RoutingUpdate
    | Debug


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

let findWire (wModel: Model) (wId: ConnectionId): Wire =
    match Map.tryFind wId wModel.WX with
    | Some vWire -> vWire
    | None -> failwithf "Invalid Id passed"

let isSegmentAtPort (pos1: XYPos) (pos2: XYPos) =
    if pos1 = pos2 then true else false

let ptCloseToSeg (width: float) (pos:XYPos) (startPt: XYPos) (endPt: XYPos): bool =
        createSegBB startPt endPt width |> (containsPoint pos)

// finds closest wire segment to mouse position
let findClosestSegment (wire: Wire) (pos: XYPos) : SegmentIndex =
    let index =
        wire.Segments
        |> List.tryFindIndex (fun s -> ptCloseToSeg 5. pos s.StartPos s.EndPos )

    match index with
    | Some x ->  
        x
    | None ->      
        failwithf $"This shouldn't happen! - WireSegements: {wire.Segments} and pos: {pos}"

// creates deafult wire segment
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

let verticalOverlap (box1: BBox) (box2: BBox) = 
    let isAbove (bb1: BBox) (bb2: BBox) = (bb1.Pos.Y + bb1.Height) <= bb2.Pos.Y
    not (isAbove box1 box2 || isAbove box2 box1)

// autoconnect wire segments
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

// smart routing algorithm
let smartRouting (sModel: Symbol.Model) (wire: Wire) (segList: WireSegment list) : WireSegment list =
    let srcPortPos = Symbol.portPos sModel wire.SrcPort
    let tgtPortPos = Symbol.portPos sModel wire.TargetPort
    let avgPortPos = snapToGrid (midPt srcPortPos tgtPortPos)
    let halfPortDist = posHalve (posDiff tgtPortPos srcPortPos)
    let interval = gridSize

    // checks if bounding boxes of wire segment and symbol overlap
    let collision (seg: WireSegment) : bool =
        let segBBox = createSegBB seg.StartPos seg.EndPos interval

        Symbol.getAllSymbols sModel
        |> List.map (Symbol.getSymbolFromSymbolId sModel)
        |> List.exists (fun sym -> overlaps segBBox (Symbol.symbolBBox sModel sym.Id))

    // reroute segments recursively
    let rec avoid (seg: WireSegment) (index: int) (dir: bool) (depth: int) : WireSegment =
        let offset = if dir then gridSize else -gridSize

        match depth with
        | x when x >= 40 -> seg
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

                avoid newSeg index dir (depth + 1)
            | false -> seg

    // bidirectional recursion to shorten path
    let rec routing (depth: int) (segList: WireSegment list) : WireSegment list =
        match depth with
        | x when x >= 3 -> segList
        | _ ->
            match List.exists (fun seg -> collision seg) segList with
            | true ->
                segList
                |> List.mapi (fun i s ->
                    match i with
                    | x when (x = 0) || (x = segList.Length - 1) -> s
                    | 1 ->
                        match segList.Length with
                        | 3 ->
                            let newSeg =
                                {s with
                                    StartPos = posOf avgPortPos.X s.StartPos.Y 
                                    EndPos = posOf avgPortPos.X s.EndPos.Y 
                                }

                            let segLeft = avoid newSeg i false 0
                            let segRight = avoid newSeg i true 0
                            let distLeft = abs (avgPortPos.X - segLeft.StartPos.X)
                            let distRight = abs (avgPortPos.X - segRight.StartPos.X)
                            let clearance = abs (halfPortDist.X) - interval

                            match distLeft, distRight with
                            | x, y when x >= clearance && y >= clearance -> newSeg
                            | x, _ when x < clearance -> segLeft
                            | _, y when y < clearance -> segRight
                            | _ -> if distLeft <= distRight then segLeft else segRight
                        | _ ->
                            let newSeg =
                                {s with
                                    StartPos = posOf (srcPortPos.X + interval) s.StartPos.Y
                                    EndPos = posOf (srcPortPos.X + interval) s.EndPos.Y
                                }

                            avoid newSeg i true 0
                    | 2 ->
                        let newSeg =
                            {s with
                                StartPos = posOf s.StartPos.X avgPortPos.Y
                                EndPos = posOf s.EndPos.X avgPortPos.Y
                            }

                        let segUp = avoid newSeg i false 0
                        let segDown = avoid newSeg i true 0
                        let distUp = abs (avgPortPos.Y - segUp.StartPos.Y)
                        let distDown = abs (avgPortPos.Y - segDown.StartPos.Y)

                        if distUp <= distDown then segUp else segDown
                    | _ ->
                        let newSeg =
                            {s with
                                StartPos = posOf (tgtPortPos.X - interval) s.StartPos.Y
                                EndPos = posOf (tgtPortPos.X - interval) s.EndPos.Y
                            }

                        avoid newSeg i false 0
                )
                |> autoConnect
                |> routing (depth + 1)
            | false -> segList

    routing 0 segList

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
        | x, _ when x > 1. ->
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

let typesValid (port1: PortId, port2: PortId) (sModel: Symbol.Model) : Result<PortId * PortId, string> =
    let getType pId = (Symbol.portType sModel pId)

    match getType port1, getType port2 with
    | pT1, pT2 when pT1 = pT2 -> Error $"Invalid connection! Ports cannot both be {pT1}s."
    | PortType.Output, PortType.Input -> Ok (port1, port2)
    | PortType.Input, PortType.Output -> Ok (port2, port1)
    | _ -> failwithf "Invalid connection!"

let notAvaliableInput (wModel: Model) (wId: ConnectionId) (inputId: PortId): bool =
    Map.exists (fun _ w -> (w.Id <> wId) && (w.TargetPort = inputId)) wModel.WX

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
        }
    | _ -> None

// specific wire update when symbol updates
let updateSymWires (wModel: Model) (sModel: Symbol.Model) (symIds: ComponentId list) : Map<ConnectionId, Wire> =
    let pIds =
        symIds
        |> List.fold (fun acc symId -> acc @ Symbol.getPortsFromSymbols sModel [symId]) []
    
    wModel.WX
    |> Map.map (fun _ w ->
        match List.contains w.SrcPort pIds || List.contains w.TargetPort pIds with
        | true ->
            let segList = autoRoute sModel w
            {w with Segments = smartRouting sModel w segList}
        | false -> {w with Segments = smartRouting sModel w w.Segments}
    )

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
    }

let fitConnection (wModel: Model) (sModel: Symbol.Model) (index: SegmentIndex) (segCurrentPos: XYPos) (newPortId: PortId)  (wire: Wire) : Wire = 
    let updatedWire = {wire with SelectedSegment = index; LastDragPos = segCurrentPos; Segments = wire.Segments}
    manualRouting {wModel with WX = Map.add updatedWire.Id updatedWire wModel.WX} updatedWire.Id (Symbol.portPos sModel newPortId)

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

let updateWireValidity (wModel: Model) (sModel: Symbol.Model) (wire: Wire): Wire =
    match createWire wModel sModel wire.SrcPort wire.TargetPort (Some wire.Id) with
    | Some newWire -> {newWire with Segments = wire.Segments}
    | None -> wire

let updateConnections (wModel: Model) (sModel: Symbol.Model) : Map<ConnectionId, Wire> =
    let updatedWX = Map.map (fun _ w -> checkPortConnections wModel sModel w) wModel.WX
    let updatedModel = {wModel with WX = updatedWX}

    updatedWX
    |> Map.map (fun _ w -> updateWireValidity updatedModel sModel w)

let addWire (wModel: Model) (sModel: Symbol.Model) (port1: PortId) (port2: PortId) : Map<ConnectionId, Wire> =
    match createWire wModel sModel port1 port2 None with
    | Some wire -> 
        let updatedSegs = autoRoute sModel wire
        let updatedWX = Map.add wire.Id {wire with Segments = smartRouting sModel wire updatedSegs} wModel.WX
        let updatedModel = {wModel with WX = updatedWX}

        updatedWX
        |> Map.map (fun _ w -> updateWireValidity updatedModel sModel w)
    | None -> wModel.WX

///Given a connectionId deletes the given wire
let deleteWire (wModel: Model) (sModel: Symbol.Model) (wId: ConnectionId) : Map<ConnectionId, Wire> =
    let updatedModel = {wModel with WX = Map.remove wId wModel.WX}
    updatedModel.WX
    |> Map.map (fun _ w -> updateWireValidity updatedModel sModel w)

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

let getWiresOfSymbols (wModel: Model) (sModel: Symbol.Model) (sIdLst: ComponentId list) : Map<ConnectionId, Wire> =
    let pIdList = Symbol.getPortsFromSymbols sModel sIdLst

    wModel.WX
    |> Map.filter (fun _ v ->
        match List.contains v.SrcPort pIdList, List.contains v.TargetPort pIdList with
        | false, false -> false
        | _ -> true
    )

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

let getWireColor (w: Wire): HighLightColor =
    match w with
    | w when w.Error <> None -> Red
    | w when  w.WireWidth < 4 -> Blue
    | _ -> Purple

let startDrag (wModel: Model) (wId: ConnectionId) (pos: XYPos) : Map<ConnectionId, Wire> =
    let wire = findWire wModel wId

    Map.add wire.Id {
        wire with
            SelectedSegment = findClosestSegment wire pos
            LastDragPos = pos
        } wModel.WX

let dragging (wModel: Model) (wId: ConnectionId) (pos: XYPos) : Map<ConnectionId, Wire> =
    wModel.WX
    |> Map.add wId (manualRouting wModel wId pos)

let endDrag (wModel: Model) (sModel: Symbol.Model) : Map<ConnectionId, Wire> =
    printf "Draging ended fine"
    updateConnections wModel sModel

let singleSegBBView =
    FunctionComponent.Of
        (fun (props: SegBBRenderProps) ->
            let segBBox = createSegBB props.StartPos props.EndPos 5.

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

let adjLabelPos (seg: WireSegment) : XYPos =
    match posDiff seg.StartPos seg.EndPos with
    | relDiff when relDiff.X > 10.  ->  posDiff seg.StartPos (posOf 7.5 12.5)
    | _ -> posDiff seg.StartPos (posOf -7.5 12.5)

let view (wModel: Model) (selectedWire: CommonTypes.ConnectionId option) (sModel: Symbol.Model) (dispatch: Dispatch<Msg>) =
    g [] (
        wModel.WX
        |> Map.fold (fun acc _ w ->
            let srcSeg= w.Segments.[0]

            let (labelProps: LabelRenderProps) = 
                match checkPortWidths sModel w.SrcPort w.TargetPort with 
                    | Ok width -> 
                        {
                            key = w.Id
                            Pos = adjLabelPos srcSeg 
                            ColorLabel = w.WireColor.ToString()
                            Label =  $"%d{width}"
                            BusIdcWidth =  2. 
                        }
                    | Error str -> 
                        {
                            key = w.Id
                            Pos = adjLabelPos srcSeg  
                            ColorLabel = w.WireColor.ToString()
                            Label =  $"x"
                            BusIdcWidth =  2. 
                        }
            
            let (props: WireRenderProps) =
                {
                    Key = w.Id
                    SegPath = (pathDefString w)
                    WireColor = match selectedWire with
                                | Some wId when wId = w.Id -> Green
                                | _ -> w.WireColor
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
                
            acc @ [singleWireView props]
            @ if wModel.WireAnnotation && (w.WireWidth > 3) then  [(singleLabelView labelProps)] else []
            @ if wModel.Debug then segBBox else []
        ) [] )


let routingUpdate (sModel: Symbol.Model) (wireMap: Map<ConnectionId, Wire>) : Map<ConnectionId, Wire> =
    wireMap
    |> Map.map (fun _ w ->
        {w with Segments = smartRouting sModel w w.Segments}
    )

let update (msg: Msg) (model: Model) (sModel: Symbol.Model): Model * Cmd<Msg> =
    match msg with
    | AddSymbol ->
        { model with WX = routingUpdate sModel model.WX }, Cmd.none
    | DeleteSymbols sIdLst ->
        { model with WX = deleteWiresOfSymbols model sModel sIdLst }, Cmd.none
    | DraggingSymbols sIdLst ->
        { model with WX = updateSymWires model sModel sIdLst }, Cmd.none
    | EndDragSymbols ->
        { model with WX = routingUpdate sModel model.WX }, Cmd.none
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
        printf$"pt which causes drag: {wMsgPos}"
        { model with WX = wxUpdated }, Cmd.none
    | EndDrag ->
        let wxUpdated = endDrag model sModel
        { model with WX = wxUpdated }, Cmd.none
    | SetColor c ->
        let wxUpdated =
            Map.map (fun wId _ -> setWireColor model wId c) model.WX
        { model with WX = wxUpdated }, Cmd.none
    | RoutingUpdate ->
        { model with WX = routingUpdate sModel model.WX }, Cmd.none
    | Debug ->
        { model with Debug = not model.Debug }, Cmd.none
///Dummy function to initialize for demo
let init () =
    {
        WX = Map.empty
        WireAnnotation = true
        Debug = false
    }, Cmd.none

//---------------Other interface functions--------------------//

//Calculates the distance between a point and a wire segment
let distPtToSeg (pt: XYPos) ((startPt, endPt): XYPos * XYPos) = //(wSeg: WireSegment) =
    let ptToPtA = posOf (pt.X - endPt.X) (pt.Y - endPt.Y)
    let ptAToB = posOf (endPt.X - startPt.X) (endPt.Y - startPt.Y)

    let magPtAToPtB = ((ptAToB.X) ** 2.) + ((ptAToB.Y) ** 2.) ** 0.5

    let crossProd =
        ((ptAToB.X) * (ptToPtA.Y))
        - ((ptToPtA.X) * (ptAToB.Y))

    abs (crossProd) / magPtAToPtB

let distPtToWire (pt: XYPos) (wire: Wire) =
    wire.Segments
    |> List.map (fun s -> distPtToSeg pt (s.StartPos, s.EndPos))
    |> List.maxBy (~-)

let isTargetWire (pt: XYPos) (wire: Wire) =
    //let ptCloseToSeg (pos:XYPos) (startPt: XYPos) (endPt: XYPos): bool =
    //    createSegBB startPt endPt 5. |> (containsPoint pos)

    let res =
        wire.Segments
        |> List.tryFindIndex (fun s -> (ptCloseToSeg 5. pt) s.StartPos s.EndPos)

    match res with
    | Some idx -> 
        printf $"pos which caused select: {pt}"
        true
    | None -> false

/// Give position finds the wire which is within a few pixels. If there are multiple chooses the closest one
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

let getAllPidEnds (wModel: Model) (pIdSrc: PortId) : PortId List =
    wModel.WX
    |> Map.toList
    |> List.filter (fun (_,w) ->
        w.SrcPort = pIdSrc
    )
    |> List.map (fun (_,w) -> w.TargetPort) 

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:ComponentId) : Component =
    failwithf "Not implemented"

let extractWires (wModel: Model) : Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:Component) =
    failwithf "Not Implemented"
