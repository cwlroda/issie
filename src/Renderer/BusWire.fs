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
        Id: WireSegId
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
        SelectedSegment: WireSegId option
        LastDragPos: XYPos
        Segments: Map<WireSegId, WireSegment>
    }

type Model =
    {
        WX: Map<ConnectionId, Wire>
        WireAnnotation: bool
    }

type Msg =
    | DeleteSymbols of CommonTypes.ComponentId list
    | DraggingSymbols of CommonTypes.ComponentId list
    | AddWire of (PortId * PortId)
    | SetSelected of ConnectionId
    | DeleteWire of ConnectionId
    | UnselectAll
    | StartDrag of wId: ConnectionId * pos: XYPos
    | Dragging of wId: ConnectionId * pos: XYPos
    | EndDrag
        // snap wire to grid
        // moving wires to different ports -> snap to port
    | SetColor of color: HighLightColor
    | DecreaseWidth of wId: ConnectionId
    | IncreaseWidth of wId: ConnectionId

type SegRenderProps =
    {
        Key : WireSegId
        StartPos: XYPos
        EndPos: XYPos
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


/// Takes as input a relative position between two points and outputs true if the two original points are horzontal and false otherwise
let isVertical (relPos: XYPos): bool =
    abs (relPos.X) <= abs (relPos.Y)

let createSegBB (startPos: XYPos) (endPos: XYPos) : BBox =
    match posDiff endPos startPos with
    // left to right
    | x when x.X > 0. ->
        toBBox startPos.X (startPos.Y - 5.) (abs x.X) 10.
    // right to left
    | x when x.X < 0. ->
       toBBox endPos.X (endPos.Y - 5.) (abs x.X) 10.
    // top to bottom
    | x when x.Y > 0. ->
        toBBox  (startPos.X - 5.) startPos.Y 10. (abs x.Y)
    // bottom to top
    | x when x.Y < 0. ->
        toBBox (endPos.X - 5.) endPos.Y 10. (abs x.Y)
    // failsafe case with no bounding box
    | _ ->
        toBBox 0. 0. 0. 0.


let checkPortWidths (wModel: Model) (sModel: Symbol.Model) (srcPort: PortId) (tgtPort: PortId) : Result<int, string> =
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


// find previous segment connected to current segment
let findPrevSegment (wModel: Model) (wId: ConnectionId) (pos: XYPos) (segDirection: Direction) : WireSegId option =
    let wire = findWire wModel wId
    
    wire.Segments
    |> Map.filter (fun _ v -> v.EndPos = pos)
    |> Map.tryFindKey (fun _ s -> s.Direction <> segDirection)

// find next segment connected to current segment
let findNextSegment (wModel: Model) (wId: ConnectionId) (pos: XYPos) (segDirection: Direction) : WireSegId option =
    let wire = findWire wModel wId

    wire.Segments
    |> Map.filter (fun _ v -> v.StartPos = pos)
    |> Map.tryFindKey (fun _ s -> s.Direction <> segDirection)

// checks if segment is connected directly to either input or output port
let isFirstOrLastSegment (wModel: Model) (sModel: Symbol.Model) (wire: Wire) (seg: WireSegment) : bool =
    let srcPos = Symbol.portPos sModel wire.SrcPort
    let tgtPos = Symbol.portPos sModel wire.TargetPort

    match seg.StartPos, seg.EndPos with
    | x, _ when x = srcPos -> true
    | _, x  when x = tgtPos -> true
    | _ -> false

let isTargetSeg pos startPos endPos =
    (createSegBB startPos endPos) |> (containsPoint pos)

// finds closest wire segment to mouse position
let findClosestSegment (wire: Wire) (pos: XYPos) : WireSegId option =
    wire.Segments
    |> Map.tryFindKey (fun _ s -> isTargetSeg pos s.StartPos s.EndPos)

// creates deafult wire segment
let makeWireSegment (wire : Wire) (startPos: XYPos) (endPos: XYPos) : WireSegment =
    let direction =
        match isVertical (posDiff startPos endPos) with
        | true -> Vertical
        | false -> Horizontal
        
    {
        Id = WireSegId (uuid())
        StartPos = startPos
        EndPos = endPos
        HostId = wire.Id
        Direction = direction
    }

let verticalOverlap (box1: BBox) (box2:BBox) = 
    let isAbove (bb1: BBox) (bb2: BBox) = (bb1.Pos.Y + bb1.Height) <= bb2.Pos.Y
   
    not ( isAbove box1 box2 || isAbove box2 box1)

let autoRoute (wModel: Model) (sModel: Symbol.Model) (wire: Wire) : Map<WireSegId, WireSegment> =
    let startPos = Symbol.portPos sModel wire.SrcPort
    let endPos = Symbol.portPos sModel wire.TargetPort
    let midPos = midPt startPos endPos

    let wireDir = posDiff endPos startPos

    let srcHost = Symbol.symbolBBox sModel (Symbol.getHostId sModel wire.SrcPort )
    let tgtHost = Symbol.symbolBBox sModel (Symbol.getHostId sModel wire.TargetPort)

    let vAdj = 
        match verticalOverlap srcHost tgtHost with
        | true when srcHost.Pos.Y <= tgtHost.Pos.Y -> 
            tgtHost.Pos.Y + tgtHost.Height+ 20.
           
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
        defSeg startPos (vAdj) 20., List.rev (defSeg endPos vAdj -20.)

    initialSegs @ finalSegs
    |> List.pairwise
    |> List.map (fun (startPos, endPos) -> makeWireSegment wire startPos endPos)
    |> List.map (fun s -> (s.Id, s))
    |> Map.ofList

// reconnects the two ends of an updated wire segment to its original neighbours
let autoConnect
    (wModel: Model)
    (wId: ConnectionId)
    (startPos: XYPos)
    (endPos: XYPos)
    (sId: WireSegId)
    (segMap: Map<WireSegId, WireSegment>) : Map<WireSegId, WireSegment> =
    
    let seg = segMap.[sId]

    let updatePrev =
        match findPrevSegment wModel wId startPos seg.Direction with
        | Some x ->
            Map.add x {
                segMap.[x] with EndPos = seg.StartPos
            } segMap
        | None ->
            segMap

    match findNextSegment wModel wId endPos seg.Direction with
    | Some x ->
        Map.add x {
            segMap.[x] with StartPos = seg.EndPos
        } updatePrev
    | None ->
        updatePrev

let typesValid (port1, port2) (wModel: Model) (sModel: Symbol.Model) : Result<PortId * PortId, string> =
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

    let widthValid = checkPortWidths wModel sModel port1 port2
    let validSrcTgt = typesValid (port1, port2) wModel sModel

    let src, tgt, width, colour, err =
        match widthValid, validSrcTgt with
        | _, Ok (s, t) when (notAvaliableInput wModel t) ->
            s, t, 6, Red,
            Some "Invalid Input port selection. An input port cannot have multiple input wires"
        | Ok w, Ok (s, t) when w < 2 ->
            s, t, 3, Blue, None
        | Ok _, Ok (s, t) ->
            s, t, 6, Blue, None
        | Error errStr, Ok (s, t) ->
            s, t, 6, Red, Some errStr
        | _, Error errType ->
            port1, port2, 6, Red, Some errType

    let wId =
        function
        | Some s -> s
        | None -> ConnectionId(uuid ())

    {
        Id = wId conId
        SrcPort = src
        TargetPort = tgt
        Segments = Map.empty
        LastDragPos = posOf 0.0 0.0
        WireColor = colour
        WireWidth = width
        Error = err
        SelectedSegment = None
    }

// specific wire update when symbol updates
let updateSymWires (wModel: Model) (sModel: Symbol.Model) (symIds: ComponentId list) : Map<ConnectionId, Wire> =
    let pIds =
        symIds
        |> List.fold (fun acc symId -> acc @ Symbol.getPortsFromSymbols sModel [symId]) []
    
    wModel.WX
    |> Map.map (fun _ w ->
        match List.contains w.SrcPort pIds || List.contains w.TargetPort pIds with
        | true -> {w with Segments = autoRoute wModel sModel w}
        | false -> w
    )



let addWire (wModel: Model) (sModel: Symbol.Model) (port1: PortId) (port2: PortId) : Map<ConnectionId, Wire> =
    let newWire = createWire wModel sModel port1 port2 None
    Map.add newWire.Id {newWire with Segments = autoRoute wModel sModel newWire} wModel.WX

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

let findSrcSeg (wModel: Model) (wire: Wire) : WireSegId =
    let isSrc (seg: WireSegment): bool=
        match findPrevSegment wModel wire.Id seg.StartPos seg.Direction with
        | Some segId -> false
        | _ -> true
    match Map.tryFindKey (fun _ s -> isSrc s) wire.Segments with
    | Some segId -> segId
    | None -> failwithf "Something in the check is wrong"

let findtgtSeg (wModel: Model) (wire: Wire) : WireSegId =
    let isSrc (seg: WireSegment): bool=
        match findNextSegment wModel wire.Id seg.EndPos seg.Direction with
        | Some segId -> false
        | _ -> true
    match Map.tryFindKey (fun _ s -> isSrc s) wire.Segments with
    | Some segId -> segId
    | None -> failwithf "Something in the check is wrong"


let manualRouting (wModel: Model) (wId: ConnectionId) (pos: XYPos): Wire =
    let wire = findWire wModel wId
    let diff = posDiff pos wire.LastDragPos

    let selectedId =
        match wire.SelectedSegment with
        | Some x -> x
        | None -> failwithf "This shouldn't happen!"

    let seg = wire.Segments.[selectedId]
    let origStartPos = seg.StartPos
    let origEndPos = seg.EndPos

    let offset =
            match seg.Direction with
            | Horizontal -> posOf 0. diff.Y
            | Vertical -> posOf diff.X 0.

    let isSrcConnection = findPrevSegment wModel wId seg.StartPos seg.Direction
    let istgtConnection = findNextSegment wModel wId seg.EndPos seg.Direction

    let updatedSegs =
        match isSrcConnection, istgtConnection with
        
        | Some segId, None -> 
            Map.add seg.Id {
                seg with
                    StartPos = posAdd seg.StartPos offset
                    EndPos = posAdd seg.EndPos diff 
            } wire.Segments
        | None , Some segId  -> 
            Map.add seg.Id {
                    seg with
                        StartPos = posAdd seg.StartPos diff
                        EndPos = posAdd seg.EndPos offset
                } wire.Segments
        | _ -> 
            Map.add seg.Id {
                seg with
                    StartPos = posAdd seg.StartPos offset
                    EndPos = posAdd seg.EndPos offset
            } wire.Segments

    {
            wire with
                Segments = autoConnect wModel wire.Id origStartPos origEndPos seg.Id updatedSegs
                LastDragPos = pos
        }
let fitConnection (wModel: Model) (sModel: Symbol.Model) (connectionSegId: WireSegId) (segCurrentPos: XYPos) (newPortId: PortId)  (wire: Wire) : Wire = 
    let updatedWire = {wire with SelectedSegment = Some connectionSegId; LastDragPos = segCurrentPos; Segments = wire.Segments}
    manualRouting {wModel with WX = Map.add updatedWire.Id updatedWire wModel.WX} updatedWire.Id (Symbol.portPos sModel newPortId)

let checkPortConnections (wModel: Model) (sModel: Symbol.Model) (wire: Wire) =
    let srcSegId = findSrcSeg wModel wire
    let tgtSegId = findtgtSeg wModel wire     

    let rec findClosestPort pos n =
        match Symbol.portsInRange sModel pos n with
        | [] -> None
        | [pId] -> Some pId
        | lst -> findClosestPort pos (n-1.)

    match findClosestPort (wire.Segments.[srcSegId]).StartPos 15., findClosestPort (wire.Segments.[tgtSegId]).EndPos 15. with
    | Some srcPId, Some tgtPId when srcPId = wire.SrcPort && tgtPId = wire.TargetPort -> 
        fitConnection wModel sModel srcSegId (wire.Segments.[srcSegId]).StartPos srcPId wire
        |> fitConnection wModel sModel tgtSegId (wire.Segments.[tgtSegId]).EndPos tgtPId
    | Some pId, Some tgtPId when tgtPId = wire.TargetPort ->
        let updatedModel = {wModel with WX = Map.remove wire.Id wModel.WX} //to ensure it does not get a too many wire for input port validation error triggered by itself
        let updatedWire = createWire updatedModel sModel pId wire.TargetPort (Some wire.Id)
        {updatedWire with Segments = autoRoute updatedModel sModel updatedWire}
        //{createWire updatedModel pId wire.TargetPort (Some wire.Id)  with Segments = wire.Segments}  
        //|> fitConnection wModel srcSegId (wire.Segments.[srcSegId]).StartPos pId
    | Some srcPId, Some pId ->  
        //{createWire wModel pId wire.SrcPort (Some wire.Id) with Segments = wire.Segments}   
        //|> fitConnection wModel tgtSegId (wire.Segments.[tgtSegId]).EndPos pId
        let updatedWire = createWire wModel sModel pId wire.SrcPort (Some wire.Id)
        {updatedWire with Segments =  autoRoute wModel sModel updatedWire}
    | None, Some pId -> fitConnection wModel sModel srcSegId (wire.Segments.[srcSegId]).StartPos wire.SrcPort wire
    | Some pId, None -> fitConnection wModel sModel tgtSegId (wire.Segments.[tgtSegId]).EndPos wire.TargetPort wire
    | None, None ->
        {wire with Segments = autoRoute wModel sModel wire}
    

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

let singleLabelView =
    FunctionComponent.Of
        (fun (props: LabelRenderProps) ->
            g [][
                text [ 
                        X (props.Pos.X + 7.5)
                        Y (props.Pos.Y + 12.5)
                        Style [ 
                                TextAnchor "left"
                                DominantBaseline "middle"
                                FontSize "14px"
                                Fill props.ColorLabel ] ] [
                        str <| sprintf $"{props.Label}"
                ]
                line [
                        X1 (props.Pos.X + 7.5 );
                        Y1 (props.Pos.Y - 5.);
                        X2 (props.Pos.X + 15.);
                        Y2 (props.Pos.Y + 5. );
                        SVGAttr.Stroke (props.ColorLabel)
                        SVGAttr.FillOpacity 0
                        SVGAttr.StrokeWidth props.BusIdcWidth
                ][]
            ])


let singleSegView =
    FunctionComponent.Of
        (fun (props: SegRenderProps) ->
            let color = props.WireColor
            let width = props.WireWidth

            //let segBBox = createSegBB props.StartPos props.EndPos

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

let view (wModel: Model) (sModel: Symbol.Model) (dispatch: Dispatch<Msg>) =
    g [] (wModel.WX
        |> Map.fold (fun acc _ w ->
            let srcSeg= w.Segments.[findSrcSeg wModel w]
            let lbl = 
                match checkPortWidths wModel sModel w.SrcPort w.TargetPort with 
                    | Ok w -> $"%d{w}"
                    | Error str -> "Undef."
            
              
            let (labelProps: LabelRenderProps) = {
                key = w.Id
                Pos = srcSeg.StartPos 
                ColorLabel = w.WireColor.ToString()
                Label = lbl
                BusIdcWidth = (if w.WireWidth > 3 then 3. else 0.)
            }

            let segList =
                w.Segments
                |> Map.fold (fun acc _ s ->
                    let props =
                        {
                            Key = s.Id
                            StartPos = s.StartPos
                            EndPos = s.EndPos
                            WireColor = w.WireColor
                            WireWidth = $"%d{w.WireWidth}"
                        }

                    acc @ [singleSegView props]
                ) []

            acc @ segList
            @ [(singleLabelView labelProps)]
        ) [])






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
    |> Map.map (fun _ s -> distPtToSeg pt (s.StartPos, s.EndPos))
    |> Map.toList
    |> List.map snd
    |> List.maxBy (~-)

let isTargetWire (pt: XYPos) (wire: Wire) =
    let ptCloseToSeg (startPt: XYPos) (endPt: XYPos): bool =
        createSegBB startPt endPt |> (containsPoint pt)

    let res =
        wire.Segments
        |> Map.tryFindKey (fun _ s -> ptCloseToSeg s.StartPos s.EndPos)

    match res with
    | Some _ -> true
    | None -> false

/// Give position finds the wire which is within a few pixels. If there are multiple chooses the closest one
let getTargetedWire (wModel: Model) (pos: XYPos): ConnectionId Option =
    let closestWire (lst: (ConnectionId * Wire) List): ConnectionId =
        List.map (fun (wId, w) -> (wId, distPtToWire pos w)) lst
        |> List.maxBy (fun (w, dist) -> -dist)
        |> fst

    let possibleWires =
        Map.filter (fun wId vWire -> (isTargetWire pos vWire)) wModel.WX
        |> Map.toList


    match possibleWires with
    | [ (wId, w) ] -> Some wId
    | [] -> None
    | lst -> Some(closestWire lst)


let getErrors (wModel: Model) (sModel: Symbol.Model): Error list =
    Map.fold
        (fun lst wId w ->
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
