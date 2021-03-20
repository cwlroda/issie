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
        HostId: ConnectionId
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
    }

type Msg =
    | DeleteSymbols of CommonTypes.ComponentId list
    | DraggingSymbols of CommonTypes.ComponentId list
    | EndDragSymbols
    | AddWire of (PortId * PortId)
    | SetSelected of ConnectionId
    | DeleteWire of ConnectionId
    | UnselectAll
    | StartDrag of wId: ConnectionId * pos: XYPos
    | Dragging of wId: ConnectionId * pos: XYPos
    | EndDrag
    | SetColor of color: HighLightColor

type ConnectionRenderProps =
    {
        Key: ConnectionId
        StartSeg: WireSegment
        EndSeg: WireSegment
        WireColor: HighLightColor
        WireWidth: string
    }

type WireRenderProps =
    {
        Key: ConnectionId
        StartPos: XYPos
        EndPos: XYPos
        WireColor: HighLightColor
        WireWidth: string
    }

type LabelRenderProps =
    {
        Key: PortId
        Label: string
        ColorLabel: string
        Pos: XYPos
    }


/// Takes as input a relative position between two points and outputs true if the two original points are horzontal and false otherwise
let isVertical (relPos: XYPos): bool =
    abs (relPos.X) <= abs (relPos.Y)

let createSegBB (startPos: XYPos) (endPos: XYPos) (width: float) : BBox =
    match posDiff endPos startPos with
    // left to right
    | x when x.X > 0. ->
        toBBox startPos.X (startPos.Y - width) (abs x.X) (width * 2.)
    // right to left
    | x when x.X < 0. ->
        toBBox endPos.X (endPos.Y - width) (abs x.X) (width * 2.)
    // top to bottom
    | x when x.Y > 0. ->
        toBBox  (startPos.X - width) startPos.Y (width * 2.) (abs x.Y)
    // bottom to top
    | x when x.Y < 0. ->
        toBBox (endPos.X - width) endPos.Y (width * 2.) (abs x.Y)
    // failsafe case with no bounding box
    | _ ->
        toBBox 0. 0. 0. 0.

let checkPortWidths (sModel: Symbol.Model) (srcPort: PortId) (tgtPort: PortId) : Result<int, string> =
    let getWidth pId = Symbol.portWidth sModel pId

    match getWidth srcPort, getWidth tgtPort with
    | Some pW1, Some pW2 when pW1 <> pW2 ->
        Error $"Invalid Port Selection. Wire widths dont match, port widths of {pW1} bit(s) does not match widths of {pW2} bits"
    | None, Some w ->
        Error $"Invalid Port Selection. Wire widths dont match, port widths of None does not match {w}bits"
    | Some w, None ->
        Error $"Invalid Port Selection. Wire widths dont match, port widths of None does not match {w}bits"
    | Some w, _ -> Ok w
    | _, _ -> failwithf "Should not occur"

let findWire (wModel: Model) (wId: ConnectionId): Wire =
    match Map.tryFind wId wModel.WX with
    | Some vWire -> vWire
    | None -> failwithf "Invalid Id passed"

let isSegmentAtPort (pos1: XYPos) (pos2: XYPos) =
    if pos1 = pos2 then true else false

let isTargetSeg pos startPos endPos =
    (createSegBB startPos endPos 5.) |> (containsPoint pos)

// finds closest wire segment to mouse position
let findClosestSegment (wire: Wire) (pos: XYPos) : SegmentIndex =
    let index =
        wire.Segments
        |> List.tryFindIndex (fun s -> isTargetSeg pos s.StartPos s.EndPos)

    match index with
    | Some x -> x
    | None -> failwithf "This shouldn't happen!"

// creates deafult wire segment
let makeWireSegment (wire : Wire) (startPos: XYPos) (endPos: XYPos) : WireSegment =
    let direction =
        match isVertical (posDiff startPos endPos) with
        | true -> Vertical
        | false -> Horizontal
        
    {
        StartPos = startPos
        EndPos = endPos
        HostId = wire.Id
        Direction = direction
    }

let verticalOverlap (box1: BBox) (box2: BBox) = 
    let isAbove (bb1: BBox) (bb2: BBox) = (bb1.Pos.Y + bb1.Height) <= bb2.Pos.Y
    not (isAbove box1 box2 || isAbove box2 box1)

// --- START OF COLLISION DETECTION AUTOROUTING --- //

// autoconnect wire segments
let autoConnect (segList: WireSegment list) : WireSegment list =
    segList
    |> List.mapi (fun i s ->
        match i with
        | x when x = 0 -> {s with EndPos = segList.[i+1].StartPos}
        | x when x = (List.length segList - 1) -> {s with StartPos = segList.[i-1].EndPos}
        | x when (List.length segList = 3) && (x = 1) -> s
        | x when (List.length segList = 5) && (x = 1) ->
            {s with
                EndPos =
                    {
                        X = s.EndPos.X
                        Y = segList.[i+1].StartPos.Y
                    }
            }
        | x when (List.length segList = 5) && (x = 2) ->
            {s with
                StartPos =
                    {
                        X = segList.[i-1].EndPos.X
                        Y = s.StartPos.Y
                    }
                EndPos =
                    {
                        X = segList.[i+1].StartPos.X
                        Y = s.StartPos.Y
                    }
            }
        | x when (List.length segList = 5) && (x = 3) ->
            {s with
                StartPos =
                    {
                        X = s.StartPos.X
                        Y = segList.[i-1].EndPos.Y
                    }
            }
        | _ -> failwithf "This shouldn't happen!"
    )
    
// naive routing algorithm
let routing (sModel: Symbol.Model) (wire: Wire) (segList: WireSegment list) : WireSegment list =
    let srcSym = Symbol.findSymbolFromPort sModel (Symbol.findPort sModel wire.SrcPort)
    let tgtSym = Symbol.findSymbolFromPort sModel (Symbol.findPort sModel wire.TargetPort)

    // gets all other symbols within area bounded by the two symbols (inclusive of the two symbols)
    let symList =
        Symbol.getAllSymbols sModel
        |> List.map (Symbol.getSymbolFromSymbolId sModel)
        // toBBox  (min srcSym.Component.X tgtSym.Component.X)
        //         (min srcSym.Component.Y tgtSym.Component.Y)
        //         ((max srcSym.Component.W tgtSym.Component.W) + abs (srcSym.Component.X - tgtSym.Component.X))
        //         ((max srcSym.Component.H tgtSym.Component.H) + abs (srcSym.Component.Y - tgtSym.Component.Y))
        // |> Symbol.getSymbolsInTargetArea sModel
        // |> List.map (Symbol.getSymbolFromSymbolId sModel)

    // checks if bounding boxes of wire segment and symbol overlap
    let collisionDetection (startPos: XYPos) (endPos: XYPos) : Symbol.Symbol list =
        let bbox = createSegBB startPos endPos 20.

        symList
        |> List.filter (fun sym -> overlaps bbox (Symbol.symbolBBox sModel sym.Id))

    // reroute segments
    segList
    |> List.mapi (fun i s ->
        match i with
        | x when (x = 0) || (x = List.length segList - 1) ->
            match collisionDetection s.StartPos s.EndPos with
                | [] ->    
                    s
                | lst ->         
                    let offset = (lst.Head.Component.Y - s.StartPos.Y) - 2.*gridSize
                    match lst.Head.Component.Y - s.StartPos.Y with
                    | y when (y < 0.) && (x = 0) ->   
                        {s with
                            EndPos = posAdd s.EndPos {X = 0.; Y = offset}
                        }
                    | y when y < 0. -> 
                        {s with
                            StartPos = posAdd s.StartPos {X = 0.; Y = offset}
                        }
                    | y when (x = List.length segList - 1)->
                        {s with
                            StartPos = posAdd s.StartPos {X = 0.; Y = -offset}
                        }
                    | _ -> 
                        {s with
                            EndPos = posAdd s.EndPos {X = 0.; Y = -offset}
                        }
        | _ ->
            let rec collision (seg: WireSegment) (dir: bool) : WireSegment =
                // dir - left: false, right: true 
                match collisionDetection seg.StartPos seg.EndPos with
                | [] -> seg
                | lst ->
                    match seg.Direction with
                    | Horizontal ->
                        let offset =
                            match dir with
                            | false -> -((seg.StartPos.Y - lst.Head.Component.Y) + 2.*gridSize)
                            | true -> (lst.Head.Component.Y + lst.Head.Component.H - seg.StartPos.Y) + 2.*gridSize
                        
                        let newSeg =
                            {seg with
                                StartPos = posAdd seg.StartPos {X = 0.; Y = offset}
                                EndPos = posAdd seg.EndPos {X = 0.; Y = offset}
                            }
                        // match lst.Head.Component.Y - seg.StartPos.Y with
                        // | y when y < 0. ->   
                        //     {seg with
                        //         StartPos = posAdd seg.StartPos {X = 0.; Y = offset}
                        //         EndPos = posAdd seg.EndPos {X = 0.; Y = offset}
                        //     }
                        // | _ ->
                        //     {seg with
                        //         StartPos = posAdd seg.StartPos {X = 0.; Y = -offset}
                        //         EndPos = posAdd seg.EndPos {X = 0.; Y = -offset}
                        //     }

                        match dir, (newSeg.StartPos.Y <= (segList.Head.StartPos.Y + 20.)), (newSeg.StartPos.Y >= ((List.last segList).EndPos.Y - 20.))  with
                        | false, false, _ -> collision newSeg false
                        | false, true, _ -> collision s true
                        | true, _ , _ ->
                            if (srcSym.Component.Y >= tgtSym.Component.Y + tgtSym.Component.H || tgtSym.Component.Y >= srcSym.Component.Y + srcSym.Component.H) then s
                            else collision newSeg true
                        // | true, _, false -> collision newSeg true
                        
                        // | false, true, _ -> collision s true
                        // | true, _ , true -> s
                        // | true, _, _ -> collision newSeg true
                        // | _ -> collision newSeg false

                    | Vertical ->
                        let offset =
                            match dir with
                            | false -> -((seg.StartPos.X - lst.Head.Component.X) + 2.*gridSize)
                            | true -> (lst.Head.Component.X + lst.Head.Component.W - seg.StartPos.X) + 2.*gridSize
                        
                        let newSeg =
                            {seg with
                                StartPos = posAdd seg.StartPos {X = offset; Y = 0.}
                                EndPos = posAdd seg.EndPos {X = offset; Y = 0.}
                            }
                        // match lst.Head.Component.X - seg.StartPos.X with
                        // | x when x < 0. ->
                        //     {seg with
                        //         StartPos = posAdd seg.StartPos {X = offset; Y = 0.}
                        //         EndPos = posAdd seg.EndPos {X = offset; Y = 0.}
                        //     }
                        // | _ ->
                        //     {seg with
                        //         StartPos = posAdd seg.StartPos {X = -offset; Y = 0.}
                        //         EndPos = posAdd seg.EndPos {X = -offset; Y = 0.}
                        //     }
                        match ((List.last segList).EndPos.X <= segList.Head.StartPos.X), i, dir, (newSeg.StartPos.X <= (segList.Head.StartPos.X + 20.)), (newSeg.StartPos.X >= ((List.last segList).EndPos.X - 20.)) with
                        | false, _, false, false, false -> collision newSeg false
                        | false, _, false, false, true ->  collision newSeg false // X
                        | false, _, false, true, false -> collision s true
                        | false, _, false, true, true -> s
                        | false, _, true, false, false -> collision newSeg true
                        | false, _, true, false, true -> s
                        | false, _, true, true, false -> collision newSeg false // X
                        | false, _, true, true, true -> s

                        
                        | true, 1, false, false, false -> collision newSeg false //X
                        | true, 1, false, false, true -> collision newSeg false
                        | true, 1, false, true, false -> collision newSeg false //X
                        | true, 1, false, true, true -> collision newSeg true
                        | true, 1, true, false, false -> collision newSeg true // X
                        | true, 1, true, false, true -> collision newSeg true
                        | true, 1, true, true, false -> collision newSeg false // X
                        | true, 1, true, true, true -> collision newSeg true

                        | true, 3, false, false, false -> collision newSeg false //X
                        | true, 3, false, false, true -> collision newSeg false // X
                        | true, 3, false, true, false -> collision newSeg false
                        | true, 3, false, true, true -> collision newSeg false
                        | true, 3, true, false, false -> collision newSeg true //X
                        | true, 3, true, false, true -> collision newSeg false //X
                        | true, 3, true, true, false -> collision newSeg true
                        | true, 3, true, true, true -> collision newSeg false

                        // | _, _, false, false, _ -> collision newSeg false
                        // | _, _, false, true, _ -> collision s true
                        // | _, _, true, _ , true -> s
                        // | _, _, true, _, false -> collision newSeg true
                        // | _ -> collision newSeg false
                        
                        | _ -> collision newSeg false
                        
                        
                        
                        // true = 5seg, false = 3seg
                        // true = right, false = left
                        // 3seg, _ left, noerrout, noerrin-> left
                        // 3seg, _  left, noerrout, errin-> X
                        // 3seg, _  left, errout, noerrin-> right
                        // 3seg, _  left errout, errin-> s
                        // 3seg, _  right noerrout, noerrin -> right
                        // 3seg, _  right noerrout, errin -> s
                        // 3seg, _  right errout, noerrin-> X
                        // 3seg, _  right, errout errin-> s

                        // 5seg, 1, left, noerrout, noerrin -> X
                        // 5seg, 1, left, noerrout, errin -> left
                        // 5seg, 1, left, errout, noerrin -> X
                        // 5seg, 1, left, errout, errin -> right
                        // 5seg, 1, right, noerrout, noerrin -> X
                        // 5seg, 1, right, noerrout, errin -> right
                        // 5seg, 1, right, errout, noerrin -> X
                        // 5seg, 1, right, errout, errin -> right

                        // 5seg, 3, left, noerrout, noerrin -> X
                        // 5seg, 3, left, noerrout, errin -> X
                        // 5seg, 3, left, errout, noerrin -> left
                        // 5seg, 3, left, errout, errin -> left
                        // 5seg, 3, right, noerrout, noerrin -> X
                        // 5seg, 3, right, noerrout, errin -> X
                        // 5seg, 3, right, errout, noerrin -> right
                        // 5seg, 3, right, errout, errin -> left


                        // | false, false, false, false -> left
                        // | 
                        // | false, true, _, false -> collision s true
                        // | false, false, _, true -> collision newSeg false
                        // | true, _ , true -> s
                        // | true, _, _ -> collision newSeg true
                        // | _ -> collision newSeg false
                    // collision newSeg false
                    

                    // match newSeg.StartPos.X <= (segList.Head.StartPos.X + 20.) && (dir = false) with
                    //     match dir with
                    // | false -> collision newSeg false
                    // | true -> collision newSeg true
            
            match i with
            | 3 -> collision s true
            | _ -> collision s false
    )
    |> autoConnect

// --- END OF COLLISION DETECTION AUTOROUTING --- //

let autoRoute (sModel: Symbol.Model) (wire: Wire) : WireSegment list =
    let startPos = Symbol.portPos sModel wire.SrcPort
    let endPos = Symbol.portPos sModel wire.TargetPort
    let midPos = midPt startPos endPos

    let wireDir = posDiff endPos startPos

    let srcHost = Symbol.symbolBBox sModel (Symbol.getHostId sModel wire.SrcPort )
    let tgtHost = Symbol.symbolBBox sModel (Symbol.getHostId sModel wire.TargetPort)

    let vAdj = 
        match verticalOverlap srcHost tgtHost with
        | true when srcHost.Pos.Y <= tgtHost.Pos.Y -> 
            tgtHost.Pos.Y + tgtHost.Height + 20.
        | true ->  
            srcHost.Pos.Y + srcHost.Height + 20.
        | false -> 
            midPos.Y

    let defSeg pos vPos hPos =
        match wireDir.X, wireDir.Y with
        | x, _ when x > 1. ->
            [
                snapToGrid pos
                snapToGrid { X = midPos.X; Y = pos.Y }
            ]
        | _ ->
            [
                snapToGrid pos
                snapToGrid { X = pos.X + hPos; Y = pos.Y }
                snapToGrid { X = pos.X + hPos; Y = vPos }
            ]

    let initialSegs, finalSegs =
        defSeg startPos vAdj 20., List.rev (defSeg endPos vAdj -20.)

    initialSegs @ finalSegs
    |> List.pairwise
    |> List.map (fun (startPos, endPos) -> makeWireSegment wire startPos endPos)
    // |> routing sModel wire

let typesValid (port1, port2) (sModel: Symbol.Model) : Result<PortId * PortId, string> =
    let getType pId = (Symbol.portType sModel pId)

    match getType port1, getType port2 with
    | pT1, pT2 when pT1 = pT2 -> Error $"Invalid Port Selection. The Ports cannot be both be {pT1}s."
    | p, _ when p = PortType.Input -> Ok(port2, port1)
    | _ -> Ok(port1, port2)

let notAvaliableInput (wModel: Model) (inputId: PortId): bool =
    Map.exists (fun _ w -> w.TargetPort = inputId) wModel.WX

let createWire
    (wModel: Model)
    (sModel: Symbol.Model)
    (port1: PortId)
    (port2: PortId)
    (conId: ConnectionId Option): Wire =

    let widthValid = checkPortWidths sModel port1 port2
    let validSrcTgt = typesValid (port1, port2) sModel

    let src, tgt, width, colour, err =
        match widthValid, validSrcTgt with
        | _, Ok (s, t) when (notAvaliableInput wModel t) ->
            s, t, 5, Red,
            Some "Invalid Input port selection. An input port cannot have multiple input wires"
        | Ok w, Ok (s, t) when w < 2 ->
            s, t, 3, Blue, None
        | Ok _, Ok (s, t) ->
            s, t, 5, Blue, None
        | Error errStr, Ok (s, t) ->
            s, t, 5, Red, Some errStr
        | _, Error errType ->
            port1, port2, 5, Red, Some errType

    let wId =
        function
        | Some s -> s
        | None -> ConnectionId(uuid ())

    {
        Id = wId conId
        SrcPort = src
        TargetPort = tgt
        Segments = []
        LastDragPos = posOf 0.0 0.0
        WireColor = colour
        WireWidth = width
        Error = err
        SelectedSegment = -1
    }

// specific wire update when symbol updates
let updateSymWires (wModel: Model) (sModel: Symbol.Model) (symIds: ComponentId list) : Map<ConnectionId, Wire> =
    let pIds =
        symIds
        |> List.fold (fun acc symId -> acc @ Symbol.getPortsFromSymbols sModel [symId]) []
    
    wModel.WX
    |> Map.map (fun _ w ->
        match List.contains w.SrcPort pIds || List.contains w.TargetPort pIds with
        | true -> {w with Segments = autoRoute sModel w}
        | false -> w
        // {w with Segments = routing sModel w w.Segments}
    )

let singleWireView =
    FunctionComponent.Of
        (fun (props: WireRenderProps) ->
            let color = props.WireColor
            let width = props.WireWidth

            let segBBox = createSegBB props.StartPos props.EndPos 5.

            g [] [
                // rect
                //     [
                //         X segBBox.Pos.X
                //         Y segBBox.Pos.Y
                //         Rx 5.
                //         Ry 5.
                //         SVGAttr.Width segBBox.Width
                //         SVGAttr.Height segBBox.Height
                //         SVGAttr.StrokeWidth "1px"
                //         SVGAttr.Stroke "Black"
                //         SVGAttr.FillOpacity 0
                //     ] []

                line 
                    [
                        X1 props.StartPos.X;
                        Y1 props.StartPos.Y;
                        X2 props.EndPos.X;
                        Y2 props.EndPos.Y;
                        SVGAttr.Stroke (color.ToString())
                        SVGAttr.FillOpacity 0
                        SVGAttr.StrokeWidth width
                        SVGAttr.StrokeLinecap "round"
                    ] []
            ]
        )

let view (model: Model) (dispatch: Dispatch<Msg>) =
    g [] (model.WX
        |> Map.fold (fun acc _ w ->
            let segList =
                w.Segments
                |> List.fold (fun acc s ->
                    let props =
                        {
                            Key = ConnectionId (uuid())
                            StartPos = s.StartPos
                            EndPos = s.EndPos
                            WireColor = w.WireColor
                            WireWidth = $"%d{w.WireWidth}"
                        }

                    acc @ [singleWireView props]
                ) []
            acc @ segList
        ) [])

let addWire (wModel: Model) (sModel: Symbol.Model) (port1: PortId) (port2: PortId) : Map<ConnectionId, Wire> =
    let newWire = createWire wModel sModel port1 port2 None
    Map.add newWire.Id {newWire with Segments = autoRoute sModel newWire} wModel.WX

///Given a connectionId deletes the given wire
let deleteWire (wModel: Model) (wId: ConnectionId) : Map<ConnectionId, Wire> =
    Map.remove wId wModel.WX

let deleteWiresOfSymbols (wModel: Model) (sModel: Symbol.Model) (sIdLst: ComponentId list) : Map<ConnectionId, Wire> =
    let pIdList = Symbol.getPortsFromSymbols sModel sIdLst

    wModel.WX
    |> Map.filter (fun _ v ->
        match List.contains v.SrcPort pIdList, List.contains v.TargetPort pIdList with
        | true, _ -> false
        | _, true -> false
        | _ -> true
    )

/// Update the colour on the given wire
let setWireColor (wModel: Model) (wId: ConnectionId) (c: HighLightColor): Wire =
    {
        findWire wModel wId with
            WireColor = c
    }

/// Update the colour
let setSelectedColor (wModel: Model) (wId: ConnectionId): Map<ConnectionId, Wire> =
    let updatedWire =
        setWireColor wModel wId Green

    Map.add wId updatedWire wModel.WX

let getWireColor (w: Wire): HighLightColor =
    match w with
    | w when w.Error <> None -> Red
    | _ -> Blue

let manualRouting (wModel: Model) (wId: ConnectionId) (pos: XYPos): Wire =
    let wire = findWire wModel wId
    let diff = posDiff pos wire.LastDragPos

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
                    EndPos = posAdd seg.EndPos offset
                }
            | index when ((index = i) && (index = (List.length wire.Segments - 1))) ->
                {s with
                    StartPos = posAdd seg.StartPos offset
                    EndPos = posAdd seg.EndPos diff
                }
            | index when index = i ->
                {s with
                    StartPos = posAdd seg.StartPos offset
                    EndPos = posAdd seg.EndPos offset
                }
            | index when (index - 1) = i ->
                {s with
                    EndPos = posAdd seg.StartPos offset
                }
            | index when (index + 1) = i ->
                {s with
                    StartPos = posAdd seg.EndPos offset
                }
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

let checkPortConnections (wModel: Model) (sModel: Symbol.Model) (wire: Wire) =
    let srcSegId = 0
    let tgtSegId = List.length wire.Segments - 1   

    let rec findClosestPort pos n =
        match Symbol.portsInRange sModel pos n with
        | [] -> None
        | [pId] -> Some pId
        | _ -> findClosestPort pos (n-1.)

    match findClosestPort (wire.Segments.[srcSegId]).StartPos 15., findClosestPort (wire.Segments.[tgtSegId]).EndPos 15. with
    | Some srcPId, Some tgtPId when srcPId = wire.SrcPort && tgtPId = wire.TargetPort -> 
        fitConnection wModel sModel srcSegId (wire.Segments.[srcSegId]).StartPos srcPId wire
        |> fitConnection wModel sModel tgtSegId (wire.Segments.[tgtSegId]).EndPos tgtPId
    | Some pId, Some tgtPId when tgtPId = wire.TargetPort ->
        let updatedModel = {wModel with WX = Map.remove wire.Id wModel.WX} //to ensure it does not get a too many wire for input port validation error triggered by itself
        let updatedWire = createWire updatedModel sModel pId wire.TargetPort (Some wire.Id)
        {updatedWire with Segments = autoRoute sModel updatedWire}
        //{createWire updatedModel pId wire.TargetPort (Some wire.Id)  with Segments = wire.Segments}  
        //|> fitConnection wModel srcSegId (wire.Segments.[srcSegId]).StartPos pId
    | Some _, Some pId ->  
        //{createWire wModel pId wire.SrcPort (Some wire.Id) with Segments = wire.Segments}   
        //|> fitConnection wModel tgtSegId (wire.Segments.[tgtSegId]).EndPos pId
        let updatedWire = createWire wModel sModel pId wire.SrcPort (Some wire.Id)
        {updatedWire with Segments =  autoRoute sModel updatedWire}
    | None, Some _ -> fitConnection wModel sModel srcSegId (wire.Segments.[srcSegId]).StartPos wire.SrcPort wire
    | Some _, None -> fitConnection wModel sModel tgtSegId (wire.Segments.[tgtSegId]).EndPos wire.TargetPort wire
    | None, None ->
        {wire with Segments = autoRoute sModel wire}
    

let updateConnections (wModel: Model) (sModel: Symbol.Model): Map<ConnectionId, Wire> =
    Map.map (fun _ w -> checkPortConnections wModel sModel w) wModel.WX

/// Reset the color of all the wires except those set in red to highlight error
let setUnselectedColor (wModel: Model): Map<ConnectionId, Wire> =
    Map.map (fun wId w ->
        getWireColor w |> setWireColor wModel wId
    ) wModel.WX

let startDrag (wModel: Model) (wId: ConnectionId) (pos: XYPos): Map<ConnectionId, Wire> =
    let wire = findWire wModel wId

    Map.add wire.Id {
        wire with
            SelectedSegment = findClosestSegment wire pos
            LastDragPos = pos
        } wModel.WX

let dragging (wModel: Model) (wId: ConnectionId) (pos: XYPos): Map<ConnectionId, Wire> =
    wModel.WX
    |> Map.add wId (manualRouting wModel wId pos)

let endDrag (wModel: Model) (sModel: Symbol.Model) : Map<ConnectionId, Wire> =
    let updatedModel = {wModel with WX = updateConnections wModel sModel}
    setUnselectedColor updatedModel

let update (msg: Msg) (model: Model) (sModel: Symbol.Model): Model * Cmd<Msg> =
    match msg with
    | DeleteSymbols sIdLst ->
        { model with
            WX = deleteWiresOfSymbols model sModel sIdLst
        } , Cmd.none
    | DraggingSymbols sIdLst ->
        { model with
            WX = updateSymWires model sModel sIdLst
        } , Cmd.none
    | EndDragSymbols ->
        let updatedWX =
            model.WX
            |> Map.map (fun _ w ->
                {w with Segments = routing sModel w w.Segments}
            )

        { model with
            WX = updatedWX
        }, Cmd.none
    | AddWire (wMsgId1, wMsgId2) ->
        let wxUpdated = addWire model sModel wMsgId1 wMsgId2
        { model with WX = wxUpdated }, Cmd.none
    | DeleteWire wMsg ->
        let wxUpdated = deleteWire model wMsg
        { model with WX = wxUpdated }, Cmd.none
    | SetSelected wMsg ->
        let wxUpdated = setSelectedColor model wMsg
        { model with WX = wxUpdated }, Cmd.none
    | UnselectAll ->
        { model with WX = setUnselectedColor model }, Cmd.none
    | Dragging (wMsgId, wMsgPos) ->
        let wxUpdated = dragging model wMsgId wMsgPos
        { model with WX = wxUpdated }, Cmd.none
    | StartDrag (wMsgId, wMsgPos) ->
        let wxUpdated = startDrag model wMsgId wMsgPos
        { model with WX = wxUpdated }, Cmd.none
    | EndDrag ->
        let wxUpdated = endDrag model sModel
        { model with WX = wxUpdated }, Cmd.none
    | SetColor c ->
        let wxUpdated =
            Map.map (fun wId _ -> setWireColor model wId c) model.WX
        { model with WX = wxUpdated }, Cmd.none

///Dummy function to initialize for demo
let init (sModel: Symbol.Model) () =
    let pLst =
        Symbol.allPortsInModel sModel
        |> Map.toList
        |> List.map fst

    let rng = System.Random 0

    let model = {
        WX = Map.empty
        WireAnnotation = true
    }

    let makeRandomWire () =
        let n = pLst.Length

        let p1, p2 =
            pLst.[rng.Next(0, n - 1)], pLst.[rng.Next(0, n - 1)]

        { model with
            WX = addWire model sModel p1 p2
        }

    let model = makeRandomWire()

    model, Cmd.none    


//---------------Other interface functions--------------------//

//Calculates the distance between a point and a wire segment
let distPtToSeg (pt: XYPos) ((startPt, endPt): XYPos * XYPos) = //(wSeg: WireSegment) =
    let ptToPtA = posOf (pt.X - endPt.X) (pt.Y - endPt.Y)

    let ptAToB =
        posOf (endPt.X - startPt.X) (endPt.Y - startPt.Y)

    let magPtAToPtB =
        ((ptAToB.X) ** 2.) + ((ptAToB.Y) ** 2.) ** 0.5

    let crossProd =
        ((ptAToB.X) * (ptToPtA.Y))
        - ((ptToPtA.X) * (ptAToB.Y))

    abs (crossProd) / magPtAToPtB

let distPtToWire (pt: XYPos) (wire: Wire) =
    wire.Segments
    |> List.map (fun s -> distPtToSeg pt (s.StartPos, s.EndPos))
    |> List.maxBy (~-)

let isTargetWire (pt: XYPos) (wire: Wire) =
    let ptCloseToSeg (startPt: XYPos) (endPt: XYPos): bool =
        createSegBB startPt endPt 5. |> (containsPoint pt)

    let res =
        wire.Segments
        |> List.tryFindIndex (fun s -> ptCloseToSeg s.StartPos s.EndPos)

    match res with
    | Some _ -> true
    | None -> false

/// Give position finds the wire which is within a few pixels. If there are multiple chooses the closest one
let getTargetedWire (wModel: Model) (pos: XYPos): ConnectionId Option =
    let closestWire (lst: (ConnectionId * Wire) List): ConnectionId =
        List.map (fun (wId, w) -> (wId, distPtToWire pos w)) lst
        |> List.minBy (fun (_, dist) -> dist)
        |> fst

    let possibleWires =
        Map.filter (fun _ vWire -> (isTargetWire pos vWire)) wModel.WX
        |> Map.toList


    match possibleWires with
    | [ (wId, _) ] -> Some wId
    | [] -> None
    | lst -> Some(closestWire lst)


let getErrors (wModel: Model) (sModel: Symbol.Model): Error list =
    Map.fold
        (fun lst _ w ->
            match w.Error with
            | Some errStr ->
                [{ Msg = errStr; Pos = (Symbol.portPos sModel w.TargetPort)} ]
                @ lst
            | None -> lst)
        []
        wModel.WX

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:ComponentId) : Component =
    failwithf "Not implemented"

let extractWires (wModel: Model) : Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:Component) =
    failwithf "Not Implemented"
