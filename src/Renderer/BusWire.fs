module BusWire

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

/// <summary> Error type for invalid wires </summary>
/// <typeparam name="Id"> ```ConnectionId``` of the invalid wire </typeparam>
/// <typeparam name="Msg"> Error message ```string``` </typeparam>
/// <typeparam name="Pos"> Position of  </typeparam>
type Error =
    {
        Id: ConnectionId
        Msg: string
    }

/// <summary> Wire segment type for a single segment defining part of a wire </summary>
/// <typeparam name="StartPos"> ```XYPos``` of the start of the wire segment </typeparam>
/// <typeparam name="EndPos">  ```XYPos``` of the end of the wire segment  </typeparam>
/// <typeparam name="Orientation"> ```Orientation``` of the wire segment </typeparam>
type WireSegment =
    {
        /// <summary> ```XYPos``` of the start of the wire segment </summary>
        StartPos: XYPos

        /// <summary> ```XYPos``` of the end of the wire segment </summary>
        EndPos: XYPos

        /// <summary> ```Orientation``` of the wire segment </summary>
        Orientation: Orientation
    }

/// <summary> Wire type for a single wire connecting two ports </summary>
/// <typeparam name="Id"> ```ConnectionId of the wire``` </typeparam>
/// <typeparam name="SrcPort"> ```PortId``` of the source port </typeparam>
/// <typeparam name="TargetPort"> ```PortId``` of the target port </typeparam>
/// <typeparam name="WireColor"> ```HighlightColor``` of the wire </typeparam>
/// <typeparam name="WireWidth"> ```int``` width of the wire </typeparam>
/// <typeparam name="Error"> ```string option``` of the error message </typeparam>
/// <typeparam name="SelectedSegment"> ```int``` selected segment index </typeparam>
/// <typeparam name="LastDragPos"> ```XYPos``` of the last dragged position </typeparam>
/// <typeparam name="Segments"> ```WireSegment``` list of segments defining a wire </typeparam>
/// <typeparam name="ManualOverride"> ```bool``` indicating whether wire has been manually routed by user </typeparam>
type Wire =
    {
        /// <summary> ```ConnectionId of the wire``` </summary>
        Id: ConnectionId

        /// <summary> ```PortId``` of the source port </summary>
        SrcPort: PortId

        /// <summary> ```PortId``` of the target port </summary>
        TargetPort: PortId

        /// <summary> ```HighLightColor``` of the wire </summary>
        WireColor: HighLightColor

        /// <summary> ```int``` width of the wire </summary>
        WireWidth: int

        /// <summary> ```string option``` of the error message </summary>
        Error: string Option

        /// <summary> ```int``` selected segment index </summary>
        SelectedSegment: SegmentIndex

        /// <summary> ```XYPos``` of the last dragged position </summary>
        LastDragPos: XYPos

        /// <summary> ```WireSegment``` list of segments defining a wire </summary>
        Segments: WireSegment list

        /// <summary> ```bool``` indicating whether wire has been manually routed by user </summary>
        ManualOverride: bool
    }

/// <summary> Map of wires and state booleans </summary>
/// <typeparam name="WX"> ```Map``` containing all ```ConnectionId``` and ```Wire``` as key-value pairs </typeparam>
/// <typeparam name="WireAnnotation"> ```int``` width of the wire </typeparam>
/// <typeparam name="Debug"> ```bool``` indicating whether debug mode for wires is activated (shows wire segment bounding boxes) </typeparam>
type Model =
    {
        WX: Map<ConnectionId, Wire>
        WireAnnotation: bool
        Debug: bool
    }

/// <summary> Update messages to change ```Model``` state </summary>
/// <typeparam name="DeleteSymbols"> Performs ```Wire``` re-routing/deletion when ```Symbol``` is being deleted </typeparam>
/// <typeparam name="DraggingSymbols"> Performs ```Wire``` re-routing when ```Symbol``` is being dragged </typeparam>
/// <typeparam name="AddWire"> Creates a ```Wire``` between two ports (must be ports of opposite polarity) </typeparam>
/// <typeparam name="DeleteWire"> Deletes a ```Wire``` </typeparam>
/// <typeparam name="StartDrag"> Starts dragging a ```WireSegment``` </typeparam>
/// <typeparam name="Dragging"> While dragging a ```WireSegment``` </typeparam>
/// <typeparam name="EndDrag"> Completes dragging a ```WireSegment``` </typeparam>
/// <typeparam name="RoutingUpdate"> Updates ```Wire``` routing </typeparam>
/// <typeparam name="Debug"> Enables wire ```BBox``` to be displayed when ```Debug``` is ```true``` </typeparam>
type Msg =
    /// <summary> Performs ```Wire``` re-routing/deletion when ```Symbol``` is being deleted </summary>
    | DeleteSymbols of CommonTypes.ComponentId list

    /// <summary> Performs ```Wire``` re-routing when ```Symbol``` is being dragged </summary>
    | DraggingSymbols of CommonTypes.ComponentId list * BBox

    /// <summary> Creates a ```Wire``` between two ports (must be ports of opposite polarity) </summary>
    | AddWire of (PortId * PortId)

    /// <summary> Deletes a ```Wire``` </summary>
    | DeleteWire of ConnectionId

    /// <summary> Starts dragging a ```WireSegment``` </summary>
    | StartDrag of wId: ConnectionId * pos: XYPos

    /// <summary> While dragging a ```WireSegment``` </summary>
    | Dragging of wId: ConnectionId * pos: XYPos

    /// <summary> Completes dragging a ```WireSegment``` </summary>
    | EndDrag

    /// <summary> Updates ```Wire``` routing </summary>
    | RoutingUpdate of BBox

    /// <summary> Enables wire ```BBox``` to be displayed when ```Debug``` is ```true``` </summary>
    | Debug

/// <summary> Props type to render a ```Wire``` </summary>
/// <typeparam name="Key"> Unique prop key given by the ```ConnectionId``` of the ```Wire``` </typeparam>
/// <typeparam name="SegPath"> Path ```string``` defining the curves of the ```Wire``` </typeparam>
/// <typeparam name="WireColor"> ```HighlightColor``` of the ```Wire``` </typeparam>
/// <typeparam name="WireWidth"> ```int``` stroke width of the ```Wire``` </typeparam>
type WireRenderProps =
    {
        /// <summary> Unique prop key given by the ```ConnectionId``` of the ```Wire``` </summary>
        Key: ConnectionId
        
        /// <summary> Path ```string``` defining the curves of the ```Wire``` </summary>
        SegPath: string
        
        /// <summary> ```HighlightColor``` of the ```Wire``` </summary>
        WireColor: HighLightColor
        
        /// <summary> ```int``` stroke width of the ```Wire``` </summary>
        WireWidth: string
    }

/// <summary> Props type to render bus width labels </summary>
/// <typeparam name="key"> Unique prop key given by the ```ConnectionId``` of the ```Wire``` </typeparam>
/// <typeparam name="Label"> Label name </typeparam>
/// <typeparam name="ColorLabel"> Label colour </typeparam>
/// <typeparam name="Pos"> ```XYPos``` of the label </typeparam>
/// <typeparam name="BusWidth"> ```float``` width of the bus </typeparam>
type LabelRenderProps =
    {
        /// <summary> Unique prop key given by the ```ConnectionId``` of the ```Wire``` </summary>
        key: ConnectionId
        
        /// <summary> Label name </summary>
        Label: string
        
        /// <summary> Label colour </summary>
        ColorLabel: string
        
        /// <summary> ```XYPos``` of the label </summary>
        Pos: XYPos
        
        /// <summary> ```float``` width of the bus </summary>
        BusWidth: float
    }

/// <summary> Props type to render ```WireSegment``` bounding boxes </summary>
/// <typeparam name="key"> Unique prop key given by the ```ConnectionId``` of the ```Wire``` </typeparam>
/// <typeparam name="StartPos"> ```XYPos``` of the start of the ```WireSegment``` </typeparam>
/// <typeparam name="EndPos"> ```XYPos``` of the end of the ```WireSegment``` </typeparam>
type SegBBRenderProps =
    {
        /// <summary> Props type to render ```WireSegment``` bounding boxes </summary>
        key: ConnectionId
        
        /// <summary> ```XYPos``` of the start of the ```WireSegment``` </summary>
        StartPos: XYPos
        
        /// <summary> ```XYPos``` of the end of the ```WireSegment``` </summary>
        EndPos: XYPos
    }


//---------------------------------helper functions---------------------------------//

/// <summary> Checks if two points form a vertical line </summary>
/// <param name="relPos"> Relative distance between two points </param>
/// <returns> ```true``` if the two original points form a vertical line, and ```false``` otherwise </returns>
let isVertical (relPos: XYPos): bool =
    abs (relPos.X) <= abs (relPos.Y)

/// <summary> Creates a standard bounding box for the wire segments </summary>
/// <param name="startPos"> ```XYPos``` of the start of the ```WireSegment``` </param>
/// <param name="endPos"> ```XYPos``` of the end of the ```WireSegment``` </param>
/// <param name="width"> ```float``` width of the bounding box </param>
/// <returns> Bounding box represented by ```BBox``` </returns>
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

/// <summary> Finds the wire with a specific id </summary>
/// <param name="wModel"> Wire ```Model``` </param>
/// <param name="wId"> ```ConnectionId``` of a wire </param>
/// <returns> ```Wire``` </returns>
/// <exception type="failwithf"> If an invalid ```ConnectionId``` is passed </exception>
let findWire (wModel: Model) (wId: ConnectionId): Wire =
    match Map.tryFind wId wModel.WX with
    | Some vWire -> vWire
    | None -> failwithf "Invalid Id passed"

/// <summary> Checks if end of a wire segment is aligned with a port </summary>
/// <param name="pos1"> Start/end position of wire segment </param>
/// <param name="pos2"> Port position </param>
/// <returns> ```true``` if both positions are the same, and ```false``` otherwise </returns>
let isSegmentAtPort (pos1: XYPos) (pos2: XYPos) : bool =
    if pos1 = pos2 then true else false

/// <summary> Checks if a point is inside a wire segment bounding box </summary>
/// <param name="pos"> Point of interest </param>
/// <param name="startPt"> Start position of wire segment </param>
/// <param name="endPt"> End position of wire segment </param>
/// <param name="width"> ```float``` width of bounding box </param>
/// <returns> ```true``` if the point is inside the bounding box, and ```false``` otherwise </returns>
let ptCloseToSeg (pos: XYPos) (startPt: XYPos) (endPt: XYPos) (width: float) : bool =
    createSegBB startPt endPt width |> (containsPoint pos)

/// <summary> Finds the closest wire segment of a given wire to mouse position </summary>
/// <param name="wire"> ```Wire``` </param>
/// <param name="pos"> Point of interest </param>
/// <returns> Index of the wire segment represented by ```SegmentIndex``` </returns>
/// <exception type="failwithf"> If the point is not inside the bounding boxes of any of the segments of the given wire </exception>
let findClosestSegment (wire: Wire) (pos: XYPos) : SegmentIndex =
    let index =
        wire.Segments
        |> List.tryFindIndex (fun s -> ptCloseToSeg pos s.StartPos s.EndPos 5.)

    match index with
    | Some x -> x
    | None ->
        failwithf $"This shouldn't happen! - WireSegments: {wire.Segments} and pos: {pos}"

/// <summary> Checks if two ports are of opposite type - i.e. one output and one input </summary>
/// <param name="port1"> ```PortId``` </param>
/// <param name="port2"> ```PortId``` </param>
/// <param name="sModel"> Symbol ```Model``` </param>
/// <returns> Result containing a tuple of ```(PortId * PortId)``` if both ports are of opposite type, and an error ```string``` otherwise </returns>
/// <exception type="failwithf"> If either of the port types are invalid </exception>
let typesValid (port1: PortId) (port2: PortId) (sModel: Symbol.Model) : Result<PortId * PortId, string> =
    let getType pId = (Symbol.portType sModel pId)

    match getType port1, getType port2 with
    | pT1, pT2 when pT1 = pT2 -> Error $"Invalid connection! Ports cannot both be {pT1}s."
    | PortType.Output, PortType.Input -> Ok (port1, port2)
    | PortType.Input, PortType.Output -> Ok (port2, port1)
    | _ -> failwithf "Invalid connection!"

/// <summary> Checks if an input has multiple wires connected to it </summary>
/// <param name="wModel"> Wire ```Model``` </param>
/// <param name="wId"> ```ConnectionId``` </param>
/// <param name="inputId"> ```PortId``` </param>
/// <returns> ```true``` if there is already another wire connected and false otherwise </returns>
let notAvaliableInput (wModel: Model) (wId: ConnectionId) (inputId: PortId): bool =
    Map.exists (fun _ w -> (w.Id <> wId) && (w.TargetPort = inputId)) wModel.WX

/// <summary> Checks the width of the given ports and if their connection is valid </summary>
/// <param name="sModel"> Symbol ```Model``` </param>
/// <param name="srcPortId"> ```PortId``` </param>
/// <param name="tgtPortId"> ```PortId``` </param>
/// <returns> Result containing the correct width if valid, and error string otherwise </returns>
/// <exception type="failwithf"> Error obtaining widths </exception>
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
        | Some x, _ when x <= 0 ->
            Error $"Invalid connection! Invalid Driver"
        | Some pW1, Some pW2 when pW1 <> pW2 ->
            Error $"Invalid connection! Mismatched wire widths [{bits pW1}, {bits pW2}]"
        | None, Some w ->
            Error $"Invalid connection! Mismatched wire widths [None, {bits w}]"
        | Some w, None ->
            Error $"Invalid connection! Mismatched wire widths [None, {bits w}]"
        | Some w, _ -> Ok w
        | _, _ -> failwithf "Error obtaining widths"

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

/// <summary> Finds all wires visible in the current screen </summary>
/// <param name="wModel"> Wire ```Model``` </param>
/// <param name="sModel"> Symbol ```Model``` </param>
/// <param name="bbox"> ```BBox``` </param>
/// <returns> ```ConnectionId``` list of the wires which are visible in the current screen </returns>
let wiresInScreen (wModel: Model) (sModel: Symbol.Model) (bbox: BBox) : ConnectionId list =
    wModel.WX
    |> Map.filter (fun _ w ->
        let srcPos = Symbol.portPos sModel w.SrcPort
        let tgtPos = Symbol.portPos sModel w.TargetPort

        (containsPoint srcPos bbox) || (containsPoint tgtPos bbox)
    )
    |> Map.toList
    |> List.map fst

/// <summary> Calculates the distance between a point and a wire segment </summary>
/// <param name="pt"> ```XYPos``` </param>
/// <param name="startPt"> Start position of wire segment </param>
/// <param name="endPt"> End position of wire segment </param>
/// <returns> ```float``` </returns>
let distPtToSeg (pt: XYPos) (startPt: XYPos) (endPt: XYPos) : float =
    let ptToPtA = posOf (pt.X - endPt.X) (pt.Y - endPt.Y)
    let ptAToB = posOf (endPt.X - startPt.X) (endPt.Y - startPt.Y)

    let magPtAToPtB = ((ptAToB.X) ** 2.) + ((ptAToB.Y) ** 2.) ** 0.5

    let crossProd =
        ((ptAToB.X) * (ptToPtA.Y))
        - ((ptToPtA.X) * (ptAToB.Y))

    abs (crossProd) / magPtAToPtB

/// <summary> Finds the shortest distance between a point and a wire </summary>
/// <param name="pt"> ```XYPos``` </param>
/// <param name="wire"> ```Wire``` </param>
/// <returns> ```float``` </returns>
let distPtToWire (pt: XYPos) (wire: Wire) : float =
    wire.Segments
    |> List.map (fun s -> distPtToSeg pt s.StartPos s.EndPos)
    |> List.maxBy (~-)

/// <summary> Checks if point is inside any of the bounding boxes of a wire's segments </summary>
/// <param name="pt"> ```XYPos``` </param>
/// <param name="wire"> ```Wire``` </param>
/// <returns> ```true``` if point is inside any bounding box, and false otherwise </returns>
let isTargetWire (pt: XYPos) (wire: Wire) : bool =
    let res =
        wire.Segments
        |> List.tryFindIndex (fun s -> ptCloseToSeg pt s.StartPos s.EndPos 5.)

    match res with
    | Some idx -> true
    | None -> false

/// <summary> Finds the closest wire within a few pixels to a point </summary>
/// <param name="wModel"> Wire ```Model``` </param>
/// <param name="pos"> ```XYPos``` </param>
/// <returns> ```Option``` with ```Some ConnectionId``` if wire is found, and ```None``` otherwise </returns>
let getTargetedWire (wModel: Model) (pos: XYPos) : ConnectionId Option =
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

/// <summary> Gets all wire errors </summary>
/// <param name="wModel"> Wire ```Model``` </param>
/// <returns> ```Error``` list </returns>
let getErrors (wModel: Model) : Error list =
    Map.fold (fun lst _ w ->
        match w.Error with
        | Some errStr ->
            [{
                Id = w.Id
                Msg = errStr
            }] @ lst
        | None -> lst
    ) [] wModel.WX

/// <summary> Gets all IDs of the input ports which are connected to the given output port </summary>
/// <param name="wModel"> Wire ```Model``` </param>
/// <param name="pIdSrc"> ```PortId``` </param>
/// <returns> ```PortId``` list </returns>
let getAllPidEnds (wModel: Model) (pIdSrc: PortId) : PortId List =
    wModel.WX
    |> Map.toList
    |> List.filter (fun (_, w) ->
        w.SrcPort = pIdSrc
    )
    |> List.map (fun (_, w) -> w.TargetPort)

/// <summary> Gets all wires which have at least one end connected to any of the given symbols </summary>
/// <param name="wModel"> Wire ```Model``` </param>
/// <param name="sModel"> Symbol ```Model``` </param>
/// <param name="sIdLst"> ```ComponentId``` list </param>
/// <returns> ```Map&lt;ConnectionId, Wire&gt;``` </returns>
let getWiresOfSymbols (wModel: Model) (sModel: Symbol.Model) (sIdLst: ComponentId list) : Map<ConnectionId, Wire> =
    let pIdList = Symbol.getPortsFromSymbols sModel sIdLst

    wModel.WX
    |> Map.filter (fun _ v ->
        match List.contains v.SrcPort pIdList, List.contains v.TargetPort pIdList with
        | false, false -> false
        | _ -> true
    )

/// <summary> Gets all wires which have both ends connected to any of the given symbols </summary>
/// <param name="wModel"> Wire ```Model``` </param>
/// <param name="sModel"> Symbol ```Model``` </param>
/// <param name="sIdLst"> ```ComponentId``` list </param>
/// <returns> ```Map&lt;ConnectionId, Wire&gt;``` </returns>
let getConnectedWires (wModel: Model) (sModel: Symbol.Model) (sIdLst: ComponentId list) : Map<ConnectionId, Wire> =
    let pIdList = Symbol.getPortsFromSymbols sModel sIdLst

    wModel.WX
    |> Map.filter (fun _ v ->
        match List.contains v.SrcPort pIdList, List.contains v.TargetPort pIdList with
        | true, true -> true
        | _ -> false
    )


//---------------------------------main functions---------------------------------//

/// Creates default wire segment.
/// Given a start and end position returns a WireSegement record
let makeWireSegment (startPos: XYPos) (endPos: XYPos) : WireSegment =
    let orientation =
        match isVertical (posDiff startPos endPos) with
        | true -> Vertical
        | false -> Horizontal
        
    {
        StartPos = startPos
        EndPos = endPos
        Orientation = orientation
    }

/// Given the current wire and symbol model, two port ids and connection id option creates 
/// a wire if the connection is valid i.e. the two ports of are not of the same time otherwise returns None.
/// If supplied with a connection id it creates the wire with the given id (logically a wire which exists has been connected to a new port) otherwise a new id is generated.
let createWire (wModel: Model) (sModel: Symbol.Model) (port1: PortId) (port2: PortId) (conId: ConnectionId Option) : Wire option =
    let createId =
        function
        | Some s -> s
        | None -> ConnectionId(uuid ())

    let wId = createId conId

    let widthValid = checkPortWidths sModel port1 port2 

    match typesValid port1 port2 sModel with
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

/// Adjusted the start and end position of each wire segments so that the segment connects.
/// Given a segment list, it returns another segment list where start position of the next segment matches the end position of the current segment
/// (the first and last segment only connects at their end and start position respectively)
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
/// Takes the current symbol model, wire of interest and segment list and uses a recursive algorithm
/// to find the shortest path while avoiding collsions with symbols. Returns the updated ```wireSegment list``` 
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
                    match seg.Orientation with
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

/// Makes the inital auto routing of a wire.
/// Takes the current symbol model and the wire and 
/// returns a list of wire segments which are defined based on the relative position between the wires' input and output port
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

/// Given the current wire model, the id of the selected wire and the position, updates the selected segments and the connected segments
/// according to the relative movement given by the ```pos``` and the ```lastPos```.
/// Returns a wire with the updated ```Segments```
let manualRouting (wModel: Model) (wId: ConnectionId) (pos: XYPos): Wire =
    let wire = findWire wModel wId
    let diff = snapToGrid (posDiff pos wire.LastDragPos)

    let seg =
        match List.tryItem wire.SelectedSegment wire.Segments with
        | Some x -> x
        | None -> failwithf "Invalid index!"

    let offset =
        match seg.Orientation with
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

/// Given the current wire and symbol models, the ```index``` of the segment of interest, the current postion of the segments ending of interest,
/// the id of the port to which the connection should be and the wire. Returns an updated with with a segments adjusted to ensure that the endpoint of the wires sits on the port.
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
            let updatedModel = {wModel with WX = Map.remove wire.Id wModel.WX} //to ensure it does not trigger input port validation error with itself
            
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

/// Takes a symbol, wire model and a wire and calls the createWire to run the validity checks and 
/// return a wire with an updated error message
let updateWireValidity (wModel: Model) (sModel: Symbol.Model) (wire: Wire): Wire =
    match createWire wModel sModel wire.SrcPort wire.TargetPort (Some wire.Id) with
    | Some newWire -> 
        {newWire with
            Segments = wire.Segments
            ManualOverride = wire.ManualOverride
        }
    | None -> wire

/// Takes the current wire and symbol model and goes through all the wire connections to update the wire instances if manual reconnections have been made
/// and ensure that all wires are have the correctly routed segments and valid connections.
let updateConnections (wModel: Model) (sModel: Symbol.Model) : Map<ConnectionId, Wire> =
    let updatedWX = Map.map (fun _ w -> checkPortConnections wModel sModel w) wModel.WX
    let updatedModel = {wModel with WX = updatedWX}

    updatedWX
    |> Map.map (fun _ w -> updateWireValidity updatedModel sModel w)

/// Given the current wire and symbol models, a list of the symbols of interest and a bounding box
/// updates all wires which have not been manually modified previously (i.e. manual override set to false) that intersect with the symbols of interest.
/// Also smartRoutes the wires which are connected to the symbols of interest, regardless of manual modification (resets manual override to false).
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
        | false ->
            match List.contains w.SrcPort pIds || List.contains w.TargetPort pIds with
            | true ->
                let segList = autoRoute sModel w
                {w with
                    Segments = smartRouting sModel w segList
                    ManualOverride = false
                }
            | false -> w
    )

/// Given the current symbol and wire models and two port ids creates a new wire instance, routes it and checks the validity of the wire
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

///Given a connectionId deletes the given wire and returns the updated ```Model.WX```  map
let deleteWire (wModel: Model) (sModel: Symbol.Model) (wId: ConnectionId) : Map<ConnectionId, Wire> =
    let updatedModel = {wModel with WX = Map.remove wId wModel.WX}
    updatedModel.WX
    |> Map.map (fun _ w -> updateWireValidity updatedModel sModel w)

///  Given a list of symbol ids deletes all the wires connected to the symbol, i.e. removes them from ```Model.WX```  map
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

/// Update the colour on the given wire
let setWireColor (wModel: Model) (wId: ConnectionId) (c: HighLightColor): Wire =
    {findWire wModel wId with WireColor = c}

/// Given a wire returns the correct colour depending on its attribute
let getWireColor (w: Wire): HighLightColor =
    match w with
    | w when w.Error <> None -> Red
    | w when  w.WireWidth < 4 -> Blue
    | _ -> Purple

 /// Given the wire model, a wire id and a position updates the ```Model.WX``` map such that the wire with the given id is set up to start dragging correctly
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
/// Given the wire model, a wire id and position returns ```Model.WX``` map with the wire with the given ids wire segments updated relative to the ```pos```
let dragging (wModel: Model) (wId: ConnectionId) (pos: XYPos) : Map<ConnectionId, Wire> =
    wModel.WX
    |> Map.add wId (manualRouting wModel wId pos)

/// Given the wire and symbol model goes through the wire model to ensure all wires are correcly routed and update the attributes accordingly
/// Returns the updated ```Model.WX``` map
let endDrag (wModel: Model) (sModel: Symbol.Model) : Map<ConnectionId, Wire> =
    updateConnections wModel sModel

/// Given the wire and symbol models and a bounding box updates the routing of all wires which has ```ManualOverride``` set to false
/// Returns the updated ```Model.WX``` map
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


//---------------------------------view functions---------------------------------//

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

let singleSegBBView =
    FunctionComponent.Of
        (fun (props: SegBBRenderProps) ->
            /// Creates a bbox with a defined width - now it is set to gridsize to debug auto-routing
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
                        SVGAttr.StrokeWidth props.BusWidth
                ][]
            ])

/// Given a wire returns the string which contains the points formatted to give the appropriate SVG path element
let pathDefString (w: Wire) : string =  
    let relMove (startSeg: WireSegment) (endSeg: WireSegment) =
        let adjPos = 
            match posDiff startSeg.StartPos endSeg.EndPos with
            | relPos when ((abs relPos.Y) <1.) || ((abs relPos.X) <1.)->  [posOf 0. 0.; posOf 0. 0.]      
            | relPos when (startSeg.Orientation = Horizontal) && (relPos.Y >0.) && (relPos.X > 0.) ->  [{X = -5.; Y = 0.};  {X = 0.; Y = 5.}]
            | relPos when (startSeg.Orientation = Horizontal) && (relPos.Y > 0.) -> [{X = 5.; Y = 0.};  {X = 0.; Y = 5.}]
            | relPos when (startSeg.Orientation = Horizontal) && (relPos.X > 0.) -> [{X = -5.; Y = 0.};  {X = 0.; Y = -5.}]
            | relPos when startSeg.Orientation = Horizontal -> [{X = 5.; Y = 0.};  {X = 0.; Y = -5.}]
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

/// Given a wire segment return an ```XYPos``` which gives the position for a wire label for the given segment
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
                        BusWidth = 2. 
                    }
                | Error _ -> 
                    {
                        key = w.Id
                        Pos = adjLabelPos srcSeg  
                        ColorLabel = color.ToString()
                        Label = $"x"
                        BusWidth = 2. 
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

/// Dummy function to initialize for demo
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


//----------------------interface to Issie-----------------------/

/// Given a list of wire segments returns a list where each tuple defines one of the vertices which make up the wire segment
let wireSegLstToVerticesLst (wSegs: WireSegment list) : (float * float) list = 
    let incompleteLstVert = List.map (fun (s:WireSegment) -> s.EndPos) wSegs
    [wSegs.[0].StartPos] @ incompleteLstVert
    |> List.map (fun pos -> (pos.X, pos.Y))   

/// Convert the information contained in the Wire with the given id to a Issie Connection type instance 
/// Given the wire and symbol model and a wire id returns a ```Connection``` instance which has attributes which matches the wire with the given id
let extractWire (wModel: Model) (sModel: Symbol.Model) (wId: ConnectionId) : Connection =
    let wire = findWire wModel wId

    {
        Id =  wire.Id.ToString()
        Source = (Symbol.findPort sModel wire.SrcPort)
        Target = (Symbol.findPort sModel wire.TargetPort)
        Vertices = (wireSegLstToVerticesLst wire.Segments)
    }
    
/// Takes a symbol id and returns a list of Connection which describes the Wires connected to the given symbol
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


