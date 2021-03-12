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
        Symbol: Symbol.Model
        WX: Map<ConnectionId, Wire>
        WireAnnotation: bool
    }

type Msg =
    | Symbol of Symbol.Msg
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
        Key: PortId
        Label: string
        ColorLabel: string
        Pos: XYPos
    }

let makeBBox (pos: XYPos) (width: float) (height: float) : BBox =
    {
        Pos = pos
        W =  width
        H = height
    }

/// Takes as input a relative position between two points and outputs true if the two original points are horzontal and false otherwise
let isVertical (relPos: XYPos): bool =
    abs (relPos.X) < abs (relPos.Y)

let createSegBB (startPos: XYPos) (endPos: XYPos) : BBox =
    match posDiff endPos startPos with
    // left to right
    | x when x.X > 0. ->
        makeBBox (posOf startPos.X (startPos.Y - 5.)) (abs x.X) 10.
    // right to left
    | x when x.X < 0. ->
        makeBBox (posOf endPos.X (endPos.Y - 5.)) (abs x.X) 10.
    // top to bottom
    | x when x.Y > 0. ->
        makeBBox (posOf (startPos.X - 5.) startPos.Y) 10. (abs x.Y)
    // bottom to top
    | x when x.Y < 0. ->
        makeBBox (posOf (endPos.X - 5.) endPos.Y) 10. (abs x.Y)
    // failsafe case with no bounding box
    | _ ->
        makeBBox (posOf 0. 0.) 0. 0.

let inBBox (point: XYPos) (bbox: BBox) : bool =
    let x = point.X - bbox.Pos.X
    let y = point.Y - bbox.Pos.Y
    
    match x, y with
    | x, y when x <= bbox.W
            && x >= 0.
            && y <= bbox.H
            && y >= 0. 
            -> true
    | _, _ -> false

let checkPortWidths (wModel: Model) (srcPort: PortId) (tgtPort: PortId) : Result<int, string> =
    let getWidth pId = Symbol.portWidth wModel.Symbol pId

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
let findPrevSegment (wModel: Model) (wId: ConnectionId) (pos: XYPos) : WireSegId option =
    let wire = findWire wModel wId
    
    wire.Segments
    |> Map.tryFindKey (fun _ v -> v.EndPos = pos)

// find next segment connected to current segment
let findNextSegment (wModel: Model) (wId: ConnectionId) (pos: XYPos) : WireSegId option =
    let wire = findWire wModel wId

    wire.Segments
    |> Map.tryFindKey (fun _ v -> v.StartPos = pos)

// checks if segment is connected directly to either input or output port
let isFirstOrLastSegment (wModel: Model) (wire: Wire) (seg: WireSegment) : bool =
    let srcPos = Symbol.portPos wModel.Symbol wire.SrcPort
    let tgtPos = Symbol.portPos wModel.Symbol wire.TargetPort

    match seg.StartPos, seg.EndPos with
    | x, _ when x = srcPos -> true
    | _, x  when x = tgtPos -> true
    | _ -> false

let isTargetSeg pos startPos endPos =
    (createSegBB startPos endPos) |> (inBBox pos)

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

let autoRoute (wModel: Model) (wire: Wire) : Map<WireSegId, WireSegment> =
    let startPos = Symbol.portPos wModel.Symbol wire.SrcPort
    let endPos = Symbol.portPos wModel.Symbol wire.TargetPort
    let midPos = midPt startPos endPos

    let wireDir = posDiff endPos startPos

    let defSeg pos wAdj hAdj =
        match wireDir.X, wireDir.Y with
        // case 1
        | x, y when x > 0. && y = 0. -> [pos]
        // cases 2 & 3
        | x, _ when x > 0. ->
            [
                pos
                { X = midPos.X; Y = pos.Y }
            ]
        | _ ->
            [
                pos
                { X = pos.X + hAdj; Y = pos.Y }
                { X = pos.X + hAdj; Y = wAdj }
            ]

    let initialSegs, finalSegs =
        defSeg startPos (midPos.Y) 5., List.rev (defSeg endPos midPos.Y -5.)

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
        match findPrevSegment wModel wId startPos with
        | Some x ->
            Map.add x {
                segMap.[x] with EndPos = seg.StartPos
            } segMap
        | None ->
            segMap

    match findNextSegment wModel wId endPos with
    | Some x ->
        Map.add x {
            segMap.[x] with StartPos = seg.EndPos
        } updatePrev
    | None ->
        updatePrev

let typesValid (port1, port2) (wModel: Model): Result<PortId * PortId, string> =
    let getType pId = (Symbol.portType wModel.Symbol pId)

    match getType port1, getType port2 with
    | pT1, pT2 when pT1 = pT2 -> Error $"Invalid Port Selection. The Ports cannot be both be {pT1}s."
    | p, _ when p = PortType.Input -> Ok(port2, port1)
    | _ -> Ok(port1, port2)

let notAvaliableInput (wModel: Model) (inputId: PortId): bool =
    Map.exists (fun _ w -> w.TargetPort = inputId) wModel.WX

let createWire
    (wModel: Model)
    (port1: PortId)
    (port2: PortId)
    (conId: ConnectionId Option): Wire =

    let widthValid = checkPortWidths wModel port1 port2
    let validSrcTgt = typesValid (port1, port2) wModel

    let src, tgt, width, colour, err =
        match widthValid, validSrcTgt with
        | _, Ok (s, t) when (notAvaliableInput wModel t) ->
            s, t, 5, Red,
            Some "Invalid Input port selection. An input port cannot have multiple input wires"
        | Ok w, Ok (s, t) when w < 2 ->
            s, t, w, Grey, None
        | Ok _, Ok (s, t) ->
            s, t, 3, Blue, None
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
        Segments = Map.empty
        LastDragPos = posOf 0.0 0.0
        WireColor = colour
        WireWidth = width
        Error = err
        SelectedSegment = None
    }

// specific wire update when symbol updates
let updateSymWires (wModel: Model) (symIds: ComponentId list) : Map<ConnectionId, Wire> =
    let pIds =
        symIds
        |> List.fold (fun acc symId -> acc @ Symbol.getPortsOfSymbol wModel.Symbol symId) []
    
    wModel.WX
    |> Map.map (fun _ w ->
        match List.contains w.SrcPort pIds || List.contains w.TargetPort pIds with
        | true -> {w with Segments = autoRoute wModel w}
        | false -> w
    )

let singleSegView =
    FunctionComponent.Of
        (fun (props: SegRenderProps) ->
            let color = props.WireColor
            let width = props.WireWidth

            let segBBox = createSegBB props.StartPos props.EndPos

            g [] [
                rect
                    [
                        X segBBox.Pos.X
                        Y segBBox.Pos.Y
                        Rx 5.
                        Ry 5.
                        SVGAttr.Width segBBox.W
                        SVGAttr.Height segBBox.H
                        SVGAttr.StrokeWidth "1px"
                        SVGAttr.Stroke "Black"
                        SVGAttr.FillOpacity 0
                    ] []

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
    let wires =
        model.WX
        |> Map.fold (fun acc _ w ->
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
        ) []

    let symbols =
        Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))

    g [] [
        (g [] wires);
        symbols
    ]

let addWire (wModel: Model) (port1: PortId) (port2: PortId) : Map<ConnectionId, Wire> =
    let newWire = createWire wModel port1 port2 None
    Map.add newWire.Id {newWire with Segments = autoRoute wModel newWire} wModel.WX

///Given a connectionId deletes the given wire
let deletWire (wModel: Model) (wId: ConnectionId) : Map<ConnectionId, Wire> =
    Map.remove wId wModel.WX

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
    | w when w.WireWidth > 1 -> Blue
    | _ -> Grey

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

let endDrag (wModel: Model): Map<ConnectionId, Wire> =
    // let wxUpdate =
    //     {wModel with WX = fitToGrid wModel}

    // let modelUpdate = { wModel with WX = wxUpdate }
    // setUnselectedColor modelUpdate
    setUnselectedColor wModel

let dragging (wModel: Model) (wId: ConnectionId) (pos: XYPos): Map<ConnectionId, Wire> =
    let wire = findWire wModel wId
    let diff = posDiff pos wire.LastDragPos

    let selectedId =
        match wire.SelectedSegment with
        | Some x -> x
        | None -> failwithf "This shouldn't happen!"

    let seg = wire.Segments.[selectedId]

    match isFirstOrLastSegment wModel wire seg with
    | false ->
        let origStartPos = seg.StartPos
        let origEndPos = seg.EndPos

        let offset =
            match seg.Direction with
            | Horizontal -> posOf 0. diff.Y
            | Vertical -> posOf diff.X 0.

        let updatedSegs =
            Map.add seg.Id {
                seg with
                    StartPos = posAdd seg.StartPos offset
                    EndPos = posAdd seg.EndPos offset
            } wire.Segments

        wModel.WX
        |> Map.add wId {
            wire with
                Segments = autoConnect wModel wire.Id origStartPos origEndPos seg.Id updatedSegs
                LastDragPos = pos
        }
    | true ->
        wModel.WX

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        let sm, sCmd = Symbol.update sMsg model.Symbol

        let wx =
            match sMsg with
            | Symbol.Dragging (symId, _) ->
                updateSymWires model [symId]
            | _ -> model.WX

        { model with Symbol = sm; WX = wx }, Cmd.map Symbol sCmd

    | AddWire (wMsgId1, wMsgId2) ->
        let wxUpdated = addWire model wMsgId1 wMsgId2
        { model with WX = wxUpdated }, Cmd.none
    | DeleteWire wMsg ->
        let wxUpdated = deletWire model wMsg
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
        let wxUpdated = endDrag model
        printf $"{wxUpdated}"
        { model with WX = wxUpdated }, Cmd.none
    | SetColor c ->
        let wxUpdated =
            Map.map (fun wId _ -> setWireColor model wId c) model.WX

        { model with WX = wxUpdated }, Cmd.none

///Dummy function to initialize for demo
let init n () =
    let symbols, cmd = Symbol.init ()

    {
        Symbol = symbols
        WX = Map.empty
        WireAnnotation = true
    },
    Cmd.none    


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
        createSegBB startPt endPt |> (inBBox pt)

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


let getErrors (wModel: Model): Error list =
    Map.fold
        (fun lst wId w ->
            match w.Error with
            | Some errStr ->
                [{ Msg = errStr; Pos = (Symbol.portPos wModel.Symbol w.SrcPort)} ]
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