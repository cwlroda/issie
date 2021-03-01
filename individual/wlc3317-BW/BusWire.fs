module BusWire

open System
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes


//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//


/// type for buswires
/// for demo only. The real wires will
/// connect to Ports - not symbols, where each symbol has
/// a number of ports (see Issie Component and Port types) and have
/// extra information for highlighting, width, etc.
/// NB - how you define Ports for drawing - whether they correspond to
/// a separate datatype and Id, or whether port offsets from
/// component coordinates are held in some other way, is up to groups.

type Direction = 
    | Vertical
    | Horizontal

type WireSegment = {
    Id: WireSegId
    StartPos: XYPos
    EndPos: XYPos
    Color: HighLightColor
    HostId: ConnectionId
    Selected: bool
    LastDragPos: XYPos
    IsDragging: bool
    Direction: Direction
}

type Wire = {
    Id: ConnectionId
    SrcPort: PortId
    TargetPort: PortId
    Segments: Map<WireSegId, WireSegment>
    Color: HighLightColor
}

type WireError = {
    Wire: Wire
    Msg: string
}

type Model = {
    Symbol: Symbol.Model
    WX: Wire list
}

//----------------------------Message Type-----------------------------------//

/// Messages to update buswire model
/// These are OK for the demo - but not the correct messages for
/// a production system. In the real system wires must connection
/// to ports, not symbols. In addition there will be other changes needed
/// for highlighting, width inference, etc
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of PortId * PortId
    | DeleteWires
    | SetWireColor of HighLightColor
    | SelectSegment of pagePos : XYPos
    | DeselectSegments
    | MouseMsg of MouseT
    | StartDragging of pagePos : XYPos
    | Dragging of pagePos: XYPos
    | EndDragging of pagePos : XYPos
    | Discard

type WireRenderProps = {
    Key : ConnectionId
    WireP: Wire
    SrcP: XYPos 
    TgtP: XYPos
    StrokeWidthP: string
    Dispatch : Dispatch<Msg>
}

//---------------------------------helper types and functions----------------//

let posDiff (a: XYPos) (b: XYPos) =
    {
        X = a.X - b.X
        Y = a.Y - b.Y
    }

let posAdd (a: XYPos) (b: XYPos) =
    {
        X = a.X + b.X
        Y = a.Y + b.Y
    }

let posOf x y =
    {
        X = x
        Y = y
    }

let midPt (a: XYPos) (b: XYPos) =
    {
        X = (a.X + b.X) / 2.
        Y = (a.Y + b.Y) / 2.
    }


let min a b = if a < b then a else b

// creates bounding box for a wire segment
let generateSegmentBBox (startPos: XYPos) (endPos: XYPos) : BBox =
    match posDiff endPos startPos with
    // left to right
    | x when x.X > 0. ->
        {
            Corner =
                {
                    X = startPos.X
                    Y = startPos.Y - 5.
                }
            W = x.X
            H = 10.
        }
    // right to left
    | x when x.X < 0. ->
        {
            Corner = 
                {
                    X = endPos.X
                    Y = endPos.Y - 5.
                }
            W = abs x.X
            H = 10.
        }
    // top to bottom
    | x when x.Y > 0. ->
        {
            Corner =
                {
                    X = startPos.X - 5.
                    Y = startPos.Y
                }
            W = 10.
            H = x.Y
        }
    // bottom to top
    | x when x.Y < 0. ->
        {
            Corner = 
                {
                    X = endPos.X - 5.
                    Y = endPos.Y
                }
            W = 10.
            H = abs x.Y
        }
    | _ ->
        {
            Corner =
                {
                    X = 0.
                    Y = 0.
                }
            W = 0.
            H = 0.
        }

// checks if point is inside wire segment bounding box
let inBBox (point: XYPos) (bbox: BBox) : bool =
    let x = point.X - bbox.Corner.X
    let y = point.Y - bbox.Corner.Y
    
    match x, y with
    | x, y when x <= bbox.W
            && x >= 0.
            && y <= bbox.H
            && y >= 0. 
            -> true
    | _, _ -> false

/// look up wire in WireModel
let findWire (wModel: Model) (wId: ConnectionId): Wire =
    wModel.WX
    |> List.find (fun w -> w.Id = wId)

let findPrevSegment (wModel: Model) (wId: ConnectionId) (pos: XYPos) : WireSegId option =
    let wire = findWire wModel wId

    wire.Segments
    |> Map.tryFindKey (fun _ v -> v.EndPos = pos)

let findNextSegment (wModel: Model) (wId: ConnectionId) (pos: XYPos) : WireSegId option =
    let wire = findWire wModel wId

    wire.Segments
    |> Map.tryFindKey (fun _ v -> v.StartPos = pos)

let isFirstOrLastSegment (wModel: Model) (wire: Wire) (seg: WireSegment) : bool =
    let srcPos = Symbol.portPos wModel.Symbol wire.SrcPort
    let tgtPos = Symbol.portPos wModel.Symbol wire.TargetPort

    match seg.StartPos, seg.EndPos with
    | x, _ when x = {srcPos with X = srcPos.X + 20.} -> true
    | _, x  when x = {tgtPos with X = tgtPos.X - 20.} -> true
    | _ -> false

let findClosestSegment (wModel: Model) (point: XYPos) : (ConnectionId * WireSegId) option =
    let seg =
        wModel.WX
        |> List.map (fun w ->
            let res =
                w.Segments
                |> Map.map (fun _ v -> generateSegmentBBox v.StartPos v.EndPos)
                |> Map.tryFindKey (fun _ b -> inBBox point b)

            match res with
            | Some x -> Some (w.Id, x)
            | None -> None
        )
        |> List.tryFind (fun x -> x <> None)
    
    match seg with
    | Some x -> x
    | None -> None

// returns selected wire segment
let getSelectedSegments (wModel: Model) : (ConnectionId * WireSegId) list =
    wModel.WX
    |> List.fold (fun acc w -> 
        acc @ (
            w.Segments
            |> Map.filter (fun _ v -> v.Selected)
            |> Map.toList
            |> List.map (fun (k, v) -> (v.HostId, k))
            )
        ) []

// creates wire segment
let makeWireSegment (wire : Wire) x1 y1 x2 y2 =
    let startPos = 
        {
            X = x1
            Y = y1
        }
    let endPos = 
        {
            X = x2
            Y = y2
        }

    let direction = match (x2 - x1), (y2 - y1) with
                    | x, _ when x <> 0. -> Horizontal
                    | _, x when x <> 0. -> Vertical
                    | _ -> failwithf "Not implemented"
    
    {
        Id = WireSegId (uuid())
        StartPos = startPos
        EndPos = endPos
        Selected = false
        LastDragPos =
            {
                X = 0.
                Y = 0.
            }
        IsDragging = false
        Color = wire.Color
        HostId = wire.Id
        Direction = direction
    }

// hardcoded routing to initialise wires based on relative symbols positions
let initRouting (wire: Wire) (srcPos: XYPos) (tgtPos: XYPos) : Map<WireSegId, WireSegment> =
    let srcX = srcPos.X + 20.
    let srcY = srcPos.Y
    let tgtX = tgtPos.X - 20.
    let tgtY = tgtPos.Y
    let midX = (midPt srcPos tgtPos).X
    let midY = (midPt srcPos tgtPos).Y

    let offset = 100.
    let (segList: WireSegment List) = []

    match (tgtX - srcX) with
    | x when x >= 0. -> 
        match (tgtY - srcY) with
        // case 2 & 3
        | y when y <> 0. ->
            let (s1: WireSegment) = makeWireSegment wire srcX srcY midX srcY
            let (s2: WireSegment) = makeWireSegment wire midX srcY midX tgtY
            let (s3: WireSegment) = makeWireSegment wire midX tgtY tgtX tgtY
            
            s1::s2::s3::segList
            |> List.map (fun s -> (s.Id, s))
            |> Map.ofList
        // case 1
        | y when y = 0. ->
            let (s1: WireSegment) = makeWireSegment wire srcX srcY tgtX tgtY
            
            s1::segList
            |> List.map (fun s -> (s.Id, s))
            |> Map.ofList
        | _ -> segList
                |> List.map (fun s -> (s.Id, s))
                |> Map.ofList
    | x when x < 0. ->
        match (tgtY - srcY) with
        // case 9
        | y when y < 0. ->
            let (s1: WireSegment) = makeWireSegment wire srcX srcY (srcX + offset) srcY
            let (s2: WireSegment) = makeWireSegment wire (srcX + offset) srcY (srcX + offset) (srcY - offset + y)
            let (s3: WireSegment) = makeWireSegment wire (srcX + offset) (srcY - offset + y) (tgtX - offset) (tgtY - offset)
            let (s4: WireSegment) = makeWireSegment wire (tgtX - offset) (tgtY - offset) (tgtX - offset) tgtY
            let (s5: WireSegment) = makeWireSegment wire (tgtX - offset) tgtY tgtX tgtY
            
            s1::s2::s3::s4::s5::segList
            |> List.map (fun s -> (s.Id, s))
            |> Map.ofList
        // case 8
        | y when y >= 0. ->
            let (s1: WireSegment) = makeWireSegment wire srcX srcY (srcX + offset) srcY
            let (s2: WireSegment) = makeWireSegment wire (srcX + offset) srcY (srcX + offset) (srcY + offset + y)
            let (s3: WireSegment) = makeWireSegment wire (srcX + offset) (srcY + offset + y) (tgtX - offset) (tgtY + offset)
            let (s4: WireSegment) = makeWireSegment wire (tgtX - offset) (tgtY + offset) (tgtX - offset) tgtY
            let (s5: WireSegment) = makeWireSegment wire (tgtX - offset) tgtY tgtX tgtY
            
            s1::s2::s3::s4::s5::segList
            |> List.map (fun s -> (s.Id, s))
            |> Map.ofList
        | _ -> segList
                |> List.map (fun s -> (s.Id, s))
                |> Map.ofList
    | _ -> segList
            |> List.map (fun s -> (s.Id, s))
            |> Map.ofList

// autoroutes a given wire
let autoRoute (wModel: Model) (wId: ConnectionId) : Wire =
    // match wire type cases based on port positions
    // pathfind by incrementing in intervals of 5px
    // check if next increment will hit bounding box of symbol
    // make a turn that will decrease distance to output port
    // returns wire with updated segments

    failwithf "Not implemented"

let autoConnect (wModel: Model) (wId: ConnectionId) (startPos: XYPos) (endPos: XYPos) (sId: WireSegId) (segMap: Map<WireSegId, WireSegment>) : Map<WireSegId, WireSegment> =
    let seg = segMap.[sId]

    let updatePrev =
        match findPrevSegment wModel wId startPos with
        | Some x ->
            Map.add x {
                segMap.[x] with EndPos = seg.StartPos
            } segMap
        | None ->
            Map.map (fun _ v -> v) segMap

    match findNextSegment wModel wId endPos with
    | Some x ->
        Map.add x {
            segMap.[x] with StartPos = seg.EndPos
        } updatePrev
    | None ->
        Map.map (fun _ v -> v) updatePrev

let updateSingleWire (wModel: Model) (w: Wire) : Wire =
    let srcPos = Symbol.portPos wModel.Symbol w.SrcPort
    let tgtPos = Symbol.portPos wModel.Symbol w.TargetPort
    {w with Segments = initRouting w srcPos tgtPos}

// checks for symbol updates and updates wires accordingly
let updateWires (wModel: Model) : Model =
    let wireList =
        wModel.WX
        |> List.map (fun w ->
            updateSingleWire wModel w
        )
    {wModel with WX = wireList}

let updateSymWires (wModel: Model) : Model =
    let pIds = Symbol.isSelected wModel.Symbol
                |> List.fold (fun acc sym -> acc @ sym.Component.InputPorts @ sym.Component.OutputPorts) []
                |> List.map (fun p -> p.PortId)
    
    let wireList =
        wModel.WX
        |> List.map (fun w ->
            match List.contains w.SrcPort pIds || List.contains w.TargetPort pIds with
            | true ->
                let srcPos = Symbol.portPos wModel.Symbol w.SrcPort
                let tgtPos = Symbol.portPos wModel.Symbol w.TargetPort
                {w with Segments = initRouting w srcPos tgtPos}
            | false -> w
        )

    {wModel with WX = wireList}

/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.
let singleWireView (wModel: Model) = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let handleMouseMove =
                Hooks.useRef (fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent

                    Dragging (posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                )

            let viewWireStaticConnection: IProp seq =
                seq {
                    OnMouseUp (fun ev ->
                        let mousePos = posOf ev.pageX ev.pageY

                        document.removeEventListener("mousemove", handleMouseMove.current)
                        
                        EndDragging mousePos
                        |> props.Dispatch
                    )
                        
                    OnMouseDown (fun ev -> 
                        // See note above re coords wrong if zoom <> 1.0
                        let mousePos = posOf ev.pageX ev.pageY

                        SelectSegment mousePos
                        |> props.Dispatch
                        StartDragging mousePos
                        |> props.Dispatch

                        document.addEventListener("mousemove", handleMouseMove.current)
                    )
                }

            let wire = props.WireP
            let renderOffset = 0.

            let wireSegments =
                wire.Segments
                |> Map.map (fun _ v ->
                    let srcX = v.StartPos.X
                    let srcY = v.StartPos.Y
                    let tgtX = v.EndPos.X
                    let tgtY = v.EndPos.Y

                    let color = v.Color.Text()

                    line 
                        (Seq.append [
                            X1 (srcX + renderOffset);
                            Y1 (srcY + renderOffset);
                            X2 (tgtX + renderOffset);
                            Y2 (tgtY + renderOffset);
                            // Qualify these props to avoid name collision with CSSProp
                            SVGAttr.Stroke color
                            SVGAttr.FillOpacity 0
                            SVGAttr.StrokeWidth props.StrokeWidthP
                        ] viewWireStaticConnection) []
                )
                |> Map.toList
                |> List.map snd

            g [] ([] @ wireSegments)
    )

let view (wModel:Model) (dispatch: Dispatch<Msg>) =
    let wires = 
        wModel.WX
        |> List.map (fun w ->
            let props = {
                Key = w.Id
                WireP = w
                SrcP = Symbol.portPos wModel.Symbol w.SrcPort 
                TgtP = Symbol.portPos wModel.Symbol w.TargetPort 
                StrokeWidthP = "3px"
                Dispatch = dispatch}
            singleWireView wModel props)
    let symbols = Symbol.view wModel.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    g [] [(g [] wires); symbols]

/// dummy init for testing: real init would probably start with no wires.
/// this initialisation is not realistic - ports are not used
/// this initialisation depends on details of Symbol.Model type.
let init n () =
    let symbols, cmd = Symbol.init()
    let symIds = List.map (fun (sym:Symbol.Symbol) -> sym.Id) symbols
    let rng = Random 0
    let makeRandomWire() =
        let n = symIds.Length
        let s1, s2 =
            match rng.Next(0,n-1), rng.Next(0,n-2) with
            | r1,r2 when r1 = r2 -> 
                symbols.[r1],symbols.[n-1] // prevents wire target and source being same
            | r1,r2 -> 
                symbols.[r1],symbols.[r2]
        
        let srcPort = (List.head s1.Component.OutputPorts).PortId
        let tgtPort = (List.head s2.Component.InputPorts).PortId

        {
            Id = ConnectionId (uuid())
            SrcPort = srcPort
            TargetPort = tgtPort
            Segments = [] |> Map.ofList
            Color = Grey
        }
    
    List.map (fun i -> makeRandomWire()) [1..n]
    |> (fun wires -> updateWires {WX=wires;Symbol=symbols},Cmd.none)

let setWireColor (wModel: Model) (c: HighLightColor) : Wire list =
    let wList =
        getSelectedSegments wModel
        |> List.map fst
    
    wModel.WX
    |> List.map (fun w -> 
        match List.contains w.Id wList with
        | true -> {
            w with 
                Color = c
                Segments = w.Segments
                            |> Map.map (fun _ v -> {v with Color = c})
            }
        | false -> w
    )

let deselectSegments (wModel: Model) : Wire list = 
    wModel.WX
    |> List.map (fun w -> 
        let segMap = 
            w.Segments
            |> Map.map (fun _ v -> 
                {
                    v with
                        Color = w.Color
                        Selected = false
                }
            )
        {w with Segments = segMap}
    )

let selectSegment (wModel: Model) (pagePos: XYPos) : Wire list =
    match findClosestSegment wModel pagePos with
    | Some (wId, sId) ->
        deselectSegments wModel
        |> List.map (fun w -> 
            match wId with
            | x when x = w.Id ->
                let segMap =
                    Map.add sId {
                        w.Segments.[sId] with 
                            Color = Blue
                            Selected = true
                    } w.Segments
                
                {w with Segments = segMap}
            | _ -> w
        )
    | None -> wModel.WX

let selectMultipleSegments (wModel: Model) (sIds: WireSegId list) : Wire list =
    wModel.WX
    |> List.map (fun w ->
        let segMap =
            w.Segments
            |> Map.map (fun k v ->
                match List.tryFind (fun sId -> sId = k) sIds with
                | Some _ ->
                    {
                        v with 
                            Color = Blue
                            Selected = true
                    }
                | None -> v
            )

        {w with Segments = segMap}
    )

let addWire (wModel: Model) (srcPort: PortId) (tgtPort: PortId) : Wire list =
    wModel.WX @ [
        updateSingleWire wModel {
            Id = ConnectionId (uuid())
            SrcPort = srcPort
            TargetPort = tgtPort
            Segments = [] |> Map.ofList
            Color = Grey
        }
    ]

let deleteWires (wModel: Model) : Wire list =
    wModel.WX
    |> List.fold (fun acc w ->
        match Map.tryFindKey (fun _ v -> v.Selected) w.Segments with
        | Some _ -> acc
        | None -> acc @ [w]
    ) []

let startDragging (wModel: Model) (pagePos: XYPos) : Wire list =
    match findClosestSegment wModel pagePos with
    | Some (wId, sId) ->
        wModel.WX
        |> List.map (fun w -> 
            match wId with
            | x when x = w.Id ->
                let segMap =
                    Map.add sId {
                        w.Segments.[sId] with 
                            LastDragPos = pagePos
                            IsDragging = true
                    } w.Segments
                
                {w with Segments = segMap}
            | _ -> w
        )
    | None -> wModel.WX

let dragging (wModel: Model) (pagePos: XYPos) : Wire list =
    match getSelectedSegments wModel with
    | [] -> wModel.WX
    | x ->
        let wList, segList = x |> List.unzip

        let wire = findWire wModel wList.Head
        let seg = wire.Segments.[segList.Head]

        match isFirstOrLastSegment wModel wire wire.Segments.[segList.Head] with
        | false ->
            wModel.WX
            |> List.map (fun w -> 
                match List.contains w.Id wList with
                | true ->
                    let segMap =
                        w.Segments
                        |> Map.map (fun k v ->
                            match List.contains k segList with
                            | true -> 
                                let diff = posDiff pagePos v.LastDragPos
                                {
                                    v with
                                        StartPos =
                                            match v.Direction with
                                            | Horizontal -> 
                                                {
                                                    X = v.StartPos.X
                                                    Y = v.StartPos.Y + diff.Y
                                                }
                                            | Vertical ->
                                                {
                                                    X = v.StartPos.X + diff.X
                                                    Y = v.StartPos.Y
                                                }
                                        EndPos =
                                            match v.Direction with
                                            | Horizontal -> 
                                                {
                                                    X = v.EndPos.X
                                                    Y = v.EndPos.Y + diff.Y
                                                }
                                            | Vertical ->
                                                {
                                                    X = v.EndPos.X + diff.X
                                                    Y = v.EndPos.Y
                                                }
                                        LastDragPos = pagePos
                                }
                            | false -> v
                        )
                    
                    {w with Segments = autoConnect wModel w.Id seg.StartPos seg.EndPos seg.Id segMap}
                | false -> w
            )
        | true -> wModel.WX

let endDragging (wModel: Model) (pagePos: XYPos) : Wire list =
    match findClosestSegment wModel pagePos with
    | Some (wId, sId) ->
        wModel.WX
        |> List.map (fun w -> 
            match wId with
            | x when x = w.Id ->
                let segMap =
                    Map.add sId {
                        w.Segments.[sId] with
                            IsDragging = false
                    } w.Segments

                {w with Segments = segMap}
            | _ -> w
        )
    | None -> wModel.WX
    

let update (msg: Msg) (wModel: Model) : Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm, sCmd = Symbol.update sMsg wModel.Symbol
        
        match sMsg with
        | Symbol.Msg.StartDragging _ ->
            {
                wModel with
                    Symbol = sm
                    WX = deselectSegments wModel
            }, Cmd.map Symbol sCmd
        | Symbol.Msg.Dragging _ ->
            updateSymWires {wModel with Symbol = sm}, Cmd.map Symbol sCmd
        | Symbol.Msg.EndDragging _ ->
            {wModel with Symbol = sm}, Cmd.map Symbol sCmd
        | Symbol.Msg.SetSelected _ ->
            {
                wModel with
                    Symbol = sm
                    WX = deselectSegments wModel
            }, Cmd.map Symbol sCmd
        | _ -> {wModel with Symbol = sm}, Cmd.map Symbol sCmd
    | AddWire (srcPort, tgtPort) ->
        { wModel with WX = addWire wModel srcPort tgtPort }, Cmd.none
    | DeleteWires ->
        { wModel with WX = deleteWires wModel }, Cmd.none
    | SetWireColor c ->
        { wModel with WX = setWireColor wModel c }, Cmd.none
    | SelectSegment pos ->
        { wModel with WX = selectSegment wModel pos }, Cmd.none
    | DeselectSegments ->
        { wModel with WX = deselectSegments wModel }, Cmd.none
    | MouseMsg mMsg ->
        wModel, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))
    | StartDragging pos ->
        { wModel with WX = startDragging wModel pos }, Cmd.none
    | Dragging pos ->
        { wModel with WX = dragging wModel pos }, Cmd.none
    | EndDragging pos ->
        { wModel with WX = endDragging wModel pos }, Cmd.none
    | Discard -> wModel, Cmd.none

//---------------Other interface functions--------------------//

// calculates distance from point to wire segment
let orthDist (pos1: XYPos) (pos2: XYPos) (pos3: XYPos) =
    let shoelaceArea =
        abs(pos1.X * pos2.Y + pos2.X * pos3.Y + pos3.X * pos1.Y
            - pos1.Y * pos2.X - pos2.Y * pos3.X - pos3.Y * pos1.X)

    let segmentLength =
        sqrt((pos1.X - pos2.X) ** 2. + (pos1.X - pos2.X) ** 2.)
    
    shoelaceArea / segmentLength

/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Used
/// to determine which wire (if any) to select on a mouse click

let wireToSelectOpt (wModel: Model) (pos: XYPos) : ConnectionId option = 
    let wireDist = 
        wModel.WX
        |> List.map (fun w ->
            w.Segments
            |> Map.fold (fun (currMin, _) _ v ->
                let dist = orthDist v.StartPos v.EndPos pos
                (min currMin dist, w.Id)
            ) (Double.MaxValue, w.Id)
        )
    
    let sortedDist = List.sortBy fst wireDist
    
    match sortedDist.Head with
    | (d, wId) when d < 10. -> Some wId
    | _ -> None

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:ComponentId) : Component =
    failwithf "Not implemented"

let extractWires (wModel: Model) : Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:Component) =
    failwithf "Not Implemented"


