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

type BBox = {
    Corner: XYPos
    W: float
    H: float
}

type WireSegment = {
    Id: CommonTypes.WireSegId
    StartPos: XYPos
    EndPos: XYPos
    Color: CommonTypes.HighLightColor
    HostId: CommonTypes.ConnectionId
    Selected: bool
    LastDragPos: XYPos
    IsDragging: bool
    Direction: Direction
}

type Wire = {
    Id: CommonTypes.ConnectionId
    SrcPort: CommonTypes.InputPortId
    TargetPort: CommonTypes.OutputPortId
    Segments: WireSegment list
    Color: CommonTypes.HighLightColor
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
    | AddWire of CommonTypes.ConnectionId
    | DeleteWire of CommonTypes.WireSegId
    | SetWireColor of wId : CommonTypes.ConnectionId * CommonTypes.HighLightColor
    | HighLightSegmentColor of wId : CommonTypes.ConnectionId * sId: CommonTypes.WireSegId * CommonTypes.HighLightColor
    | UnhighLightSegmentColor of wId : CommonTypes.ConnectionId * sId: CommonTypes.WireSegId
    | MouseMsg of MouseT
    | StartDragging of wId : CommonTypes.ConnectionId * sId : CommonTypes.WireSegId * pagePos : XYPos
    | Dragging of wId : CommonTypes.ConnectionId * sId: CommonTypes.WireSegId * pagePos: XYPos
    | EndDragging of wId : CommonTypes.ConnectionId * sId: CommonTypes.WireSegId
    | Discard

//---------------------------------helper types and functions----------------//

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

// creates bouding box for a wire segment
let generateSegmentBBox (startPos: XYPos) (endPos: XYPos) : BBox =
    match posDiff endPos startPos with
    // left to right
    | x when x.X > 0. ->
        {
            Corner = {X = startPos.X
                      Y = startPos.Y - 5.}
            W = x.X
            H = 10.
        }
    // right to left
    | x when x.X < 0. ->
        {
            Corner = {X = endPos.X
                      Y = endPos.Y - 5.}
            W = abs x.X
            H = 10.
        }
    // top to bottom
    | x when x.Y > 0. ->
        {
            Corner = {X = startPos.X - 5.
                      Y = startPos.Y}
            W = 10.
            H = x.Y
        }
    // bottom to top
    | x when x.Y < 0. ->
        {
            Corner = {X = endPos.X - 5.
                      Y = endPos.Y}
            W = 10.
            H = abs x.Y
        }
    | _ -> failwithf "Not implemented"

// checks if point is inside wire segment bounding box
let inBBox (point: XYPos) (bbox: BBox) : bool =
    let x = point.X - bbox.Corner.X
    let y = point.Y - bbox.Corner.Y
    
    match x, y with
    | x, y when x <= bbox.W
             && x >= 0.
             && y <= bbox.H
             && y >= 0. -> true
    | _ -> false

/// look up wire in WireModel
let findWire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    wModel.WX
    |> List.find (fun w -> w.Id = wId)

let findPrevSegment (wModel: Model) (wId: CommonTypes.ConnectionId) (pos: XYPos) : WireSegment option =
    let wire = findWire wModel wId

    wire.Segments
    |> List.tryFind (fun s -> s.EndPos = pos)

let findNextSegment (wModel: Model) (wId: CommonTypes.ConnectionId) (pos: XYPos) : WireSegment option =
    let wire = findWire wModel wId

    wire.Segments
    |> List.tryFind (fun s -> s.StartPos = pos)

// selects a wire segment
let setSelectedSegment (wModel: Model) (sId : CommonTypes.WireSegId) : Model =
    let wireList =
        wModel.WX
        |> List.map (fun w ->
            let segList = 
                w.Segments
                |> List.map (fun s ->
                    match s.Id with
                    | x when x = sId -> {s with Selected = true}
                    | _ -> {s with Selected = false})
                    
            {w with Segments = segList}    
        )

    {wModel with WX = wireList}

// returns selected wire segment
let getSelectedSegment (wModel: Model) (wId : CommonTypes.ConnectionId) : WireSegment =
    let wire = findWire wModel wId

    wire.Segments
    |> List.find (fun s -> s.Selected)

// unselects all wire segments
let unselectAllSegments (wModel: Model) =
    let wireList = 
        wModel.WX
        |> List.map (fun w ->
            let segList = 
                w.Segments
                |> List.map (fun s -> {s with Selected = false})
            {w with Segments = segList}
        )
    {wModel with WX = wireList}

type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    SrcP: XYPos 
    TgtP: XYPos
    ColorP: string
    StrokeWidthP: string
    Dispatch : Dispatch<Msg>
    }

// creates wire segment
let makeWireSegment (wId : CommonTypes.ConnectionId) x1 y1 x2 y2 =
    let startPos = {X = x1
                    Y = y1}
    let endPos = {X = x2
                  Y = y2}

    let direction = match x2-x1, y2-y1 with
                    | x, _ when x <> 0. -> Horizontal
                    | _, y when y <> 0. -> Vertical
                    | _ -> failwithf "Wire is not straight!"
    
    {
        Id = CommonTypes.WireSegId (uuid())
        StartPos = startPos
        EndPos = endPos
        Selected = false
        LastDragPos = {X = 0.
                       Y = 0.}
        IsDragging = false
        Color = CommonTypes.Grey
        HostId = wId
        Direction = direction
    }

// hardcoded routing to initialise wires based on relative symbols positions
let initRouting (wId: CommonTypes.ConnectionId) (srcPos: XYPos) (tgtPos: XYPos) : WireSegment list =
    let srcX = srcPos.X
    let srcY = srcPos.Y
    let tgtX = tgtPos.X
    let tgtY = tgtPos.Y
    let midX = 0.5 * (srcX + tgtX)
    let midY = 0.5 * (srcY + tgtY)

    match (tgtX - srcX) with
    | x when x >= 20. -> 
        match (tgtY - srcY) with
        // case 2 & 3
        | y when y <> 0. ->
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeWireSegment wId srcX srcY (srcX + midX) srcY
            let (s2: WireSegment) = makeWireSegment wId (srcX + midX) srcY (srcX + midX) (srcY + midY)
            let (s3: WireSegment) = makeWireSegment wId (srcX + midX) (srcY + midY) tgtX tgtY
            s1::s2::s3::segList
        // case 1
        | y when y = 0. ->
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeWireSegment wId srcX srcY tgtX tgtY
            s1::segList
        | _ -> failwithf "Not implemented"
    | x when x < 20. ->
        match (tgtY - srcY) with
        // case 9
        | y when y < 0. ->
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeWireSegment wId srcX srcY (srcX+60.) srcY
            let (s2: WireSegment) = makeWireSegment wId (srcX+60.) srcY (srcX+60.) (srcY-60.)
            let (s3: WireSegment) = makeWireSegment wId (srcX+60.) (srcY-60.) (tgtX-60.) (tgtY-60.)
            let (s4: WireSegment) = makeWireSegment wId (tgtX-60.) (tgtY-60.) (tgtX-60.) tgtY
            let (s5: WireSegment) = makeWireSegment wId (tgtX-60.) tgtY tgtX tgtY
            s1::s2::s3::s4::s5::segList
        // case 8
        | y when y >= 0. ->
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeWireSegment wId srcX srcY (srcX+60.) srcY
            let (s2: WireSegment) = makeWireSegment wId (srcX+60.) srcY (srcX+60.) (srcY+60.)
            let (s3: WireSegment) = makeWireSegment wId (srcX+60.) (srcY+60.) (tgtX-60.) (tgtY+60.)
            let (s4: WireSegment) = makeWireSegment wId (tgtX-60.) (tgtY+60.) (tgtX-60.) tgtY
            let (s5: WireSegment) = makeWireSegment wId (tgtX-60.) tgtY tgtX tgtY
            s1::s2::s3::s4::s5::segList
        | _ -> failwithf "Not implemented"
    | _ -> failwithf "Not implemented"

// autoroutes a given wire
let autorouting (wModel: Model) (wId: CommonTypes.ConnectionId) : Model =
    // checks if wire segment intersects symbol
    let offsetSymbol (sym: Symbol.Symbol) (axis: Direction) (offset: float) : float =
        let leftSym = sym.Component.X
        let rightSym = sym.Component.X + sym.Component.W
        let topSym = sym.Component.Y
        let botSym = sym.Component.Y + sym.Component.H

        match axis with
        | Horizontal -> 
            match topSym < offset, offset < botSym with
            | true, true -> botSym - offset + 10.
            | _, _ -> 0.
        | Vertical ->
            match leftSym < offset, offset < rightSym with
            | true, true -> rightSym - offset + 10.
            | _, _ -> 0.
    
    // calculate total offset to bypass all symbols
    let calcOffset (offset: float) (axis: Direction) : float = 
        wModel.Symbol
        |> List.fold (fun acc sym -> acc + (offsetSymbol sym axis acc)) offset
        
    // offsets wire segment if obstacle is found
    let bypassObstacles (segments: WireSegment list) : WireSegment list =
        segments
        |> List.map (fun s ->
            let nextSegment = 
                match findNextSegment wModel s.HostId s.EndPos with
                | Some x -> x
                | None -> s

            match s.Direction with 
            | Horizontal -> 
                let offset = calcOffset s.StartPos.Y s.Direction
                {s with StartPos = {X = s.StartPos.X + offset
                                    Y = s.StartPos.Y}
                        EndPos = {X = s.EndPos.X + offset
                                  Y = s.EndPos.Y}}
                
            | Vertical ->
                let offset = calcOffset s.StartPos.X s.Direction
                {s with StartPos = {X = s.StartPos.X
                                    Y = s.StartPos.Y + offset}
                        EndPos = {X = s.EndPos.X
                                  Y = s.EndPos.Y + offset}}
        )

        // need to update start pos of next wire somehow
    
    let wireList = 
        wModel.WX
        |> List.map (fun w ->
            match w.Id with
            | x when x = wId -> {w with Segments = bypassObstacles w.Segments}
            | _ -> w
        )
    
    {wModel with WX = wireList}

// checks for symbol updates and updates wires accordingly
let updateWires (model : Model) = 
    let wireList =
        List.map (fun w ->
            let srcPort = Symbol.portPos model.Symbol w.SrcPort
            let targetPort = Symbol.portPos model.Symbol w.TargetPort
            let segList = initRouting w.Id srcPort targetPort
            {w with Segments = segList}
        ) model.WX
    {model with WX = wireList}


/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.
let singleWireView (wModel: Model) = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let wire = props.WireP

            let wireSegments = 
                wire.Segments
                |> List.map (fun segment ->
                    let srcX = segment.StartPos.X
                    let srcY = segment.StartPos.Y
                    let tgtX = segment.EndPos.X
                    let tgtY = segment.EndPos.Y

                    g [] [
                            polyline [
                                // OnMouseDown (fun ev -> 
                                //     // See note above re coords wrong if zoom <> 1.0
                                //     let mousePos = posOf ev.pageX ev.pageY
                                    
                                //     match wireToSelectOpt wModel mousePos with
                                //     | Some x -> HighLightSegmentColor ((fst x), (snd x), CommonTypes.Blue)
                                //                 |> props.Dispatch
                                //         // StartDragging(fst x, snd x, mousePos)
                                //         // |> props.Dispatch
                                //     | None -> Discard
                                //               |> props.Dispatch

                                //     // document.addEventListener("mousemove", handleMouseMove.current)
                                // )

                                // OnMouseUp (fun ev -> 
                                //     // See note above re coords wrong if zoom <> 1.0
                                //     SetColor CommonTypes.Blue
                                //     |> props.Dispatch
                                //     // document.addEventListener("mousemove", handleMouseMove.current)
                                // )

                                SVGAttr.Points (sprintf "%f %f, %f %f" srcX srcY tgtX tgtY)
                                // Qualify these props to avoid name collision with CSSProp
                                SVGAttr.Stroke props.ColorP
                                SVGAttr.FillOpacity 0
                                SVGAttr.StrokeWidth props.StrokeWidthP
                            ] []

                            text [
                                X (srcX + 40.); 
                                Y (srcY - 20.); 
                                Style [
                                    TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                                    DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                    FontSize "18px"
                                    FontWeight "Bold"
                                    Fill "Blue"
                                ]
                            ] [str <| sprintf "0..x"]
                        ]
                )

            // let handleMouseMove =
            //     Hooks.useRef(fun (ev : Types.Event) ->
            //         let ev = ev :?> Types.MouseEvent
            //         Dragging(props.Circle.Id, posOf ev.pageX ev.pageY)
            //     )
            // x,y coordinates here do not compensate for transform in Sheet
            // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
            
            g [] ([] @ wireSegments)
    )

let view (model:Model) (dispatch: Dispatch<Msg>) =
    let wires = 
        model.WX
        |> List.map (fun w ->
            let props = {
                key = w.Id
                WireP = w
                SrcP = Symbol.portPos model.Symbol w.SrcPort 
                TgtP = Symbol.portPos model.Symbol w.TargetPort 
                ColorP = w.Color.Text()
                StrokeWidthP = "2px"
                Dispatch = dispatch}
            singleWireView model props)
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    g [] [(g [] wires); symbols]

/// dummy init for testing: real init would probably start with no wires.
/// this initialisation is not realistic - ports are not used
/// this initialisation depends on details of Symbol.Model type.
let init n () =
    let symbols, cmd = Symbol.init()
    let symIds = List.map (fun (sym:Symbol.Symbol) -> sym.Id) symbols
    let rng = System.Random 0
    let makeRandomWire() =
        let n = symIds.Length
        let s1, s2 =
            match rng.Next(0,n-1), rng.Next(0,n-2) with
            | r1,r2 when r1 = r2 -> 
                symbols.[r1],symbols.[n-1] // prevents wire target and source being same
            | r1,r2 -> 
                symbols.[r1],symbols.[r2]
        let srcPos = posOf s1.Component.X s1.Component.Y
        let tgtPos = posOf s2.Component.X s2.Component.Y
        let id = CommonTypes.ConnectionId (uuid())
        {
            Id = id
            SrcPort = s1.port.Id
            TargetPort = s2.port.Id
            Segments = initRouting id srcPort targetPort
            Color = CommonTypes.Grey
        }
    
    List.map (fun i -> makeRandomWire()) [1..n]
    |> (fun wires -> {WX=wires;Symbol=symbols},Cmd.none)

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd
    | AddWire _ -> failwithf "Not implemented"
    | DeleteWire _ -> failwithf "Not implemented"
    | SetWireColor (wId, c) ->
        let wireList = 
            List.map (fun w -> 
                match wId with
                | x when x = w.Id -> 
                    {w with Color = c}
                | _ -> w
            ) model.WX
        { model with WX = wireList }, Cmd.none
    | HighLightSegmentColor (wId, sId, c) ->
        let wireList = 
            List.map (fun w -> 
                match wId with
                | x when x = w.Id -> 
                    let segList = 
                        List.map (fun (s : WireSegment) ->
                            match sId with
                            | y when y = s.Id ->
                                {s with Color = c}
                            | _ -> s
                        ) w.Segments
                    {w with Segments = segList}
                | _ -> w
            ) model.WX
        { model with WX = wireList }, Cmd.none
    | UnhighLightSegmentColor (wId, sId) ->
        let wireList = 
            List.map (fun w -> 
                match wId with
                | x when x = w.Id -> 
                    let segList = 
                        List.map (fun (s : WireSegment) ->
                            match sId with
                            | y when y = s.Id ->
                                {s with Color = w.Color}
                            | _ -> s
                        ) w.Segments
                    {w with Segments = segList}
                | _ -> w
            ) model.WX
        { model with WX = wireList }, Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))
    | StartDragging (wId, sId, pagePos) ->
        let wireList = 
            List.map (fun w -> 
                match wId with
                | x when x = w.Id -> 
                    let segList = 
                        List.map (fun (s : WireSegment) ->
                            match sId with
                            | y when y = s.Id ->
                                { s with
                                    LastDragPos = pagePos
                                    IsDragging = true
                                }
                            | _ -> s
                        ) w.Segments
                    {w with Segments = segList}
                | _ -> w
            ) model.WX
        { model with WX = wireList }, Cmd.none
    | Dragging (wId, sId, pagePos) ->
        let wireList = 
            List.map (fun w -> 
                match wId with
                | x when x = w.Id ->
                    let segList = 
                        List.map (fun (s : WireSegment) ->
                            match sId with
                            | y when y = s.Id ->
                                let diff = posDiff pagePos s.LastDragPos
                                { s with
                                    StartPos = posAdd s.StartPos diff
                                    EndPos = posAdd s.EndPos diff
                                    LastDragPos = pagePos
                                }
                            | _ -> s
                        ) w.Segments
                    {w with Segments = segList}
                | _ -> w
            ) model.WX
        { model with WX = wireList }, Cmd.none
    | EndDragging (wId, sId) ->
        let wireList = 
            List.map (fun w -> 
                match wId with
                | x when x = w.Id -> 
                    let segList = 
                        List.map (fun (s : WireSegment) ->
                            match sId with
                            | y when y = s.Id ->
                                { s with
                                    IsDragging = false
                                }
                            | _ -> s
                        ) w.Segments
                    {w with Segments = segList}
                | _ -> w
            ) model.WX
        { model with WX = wireList }, Cmd.none
    | Discard -> model, Cmd.none

//---------------Other interface functions--------------------//

// calculates distance from point to wire segment
let orthDist (pos1: XYPos) (pos2: XYPos) (pos3: XYPos) =
    let shoelaceArea =
        float(abs(pos1.X * pos2.Y + pos2.X * pos3.Y + pos3.X * pos1.Y
                - pos1.Y * pos2.X - pos2.Y * pos3.X - pos3.Y * pos1.X))

    let segmentLength =
        sqrt(float(abs(pos1.X - pos2.X)) ** 2. + float(abs(pos1.X - pos2.X)) ** 2.)
    
    shoelaceArea / segmentLength

/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Used
/// to determine which wire (if any) to select on a mouse click

let wireToSelectOpt (wModel: Model) (pos: XYPos) : (CommonTypes.ConnectionId * CommonTypes.WireSegId) option = 
    let min (a, aId) (b, bId) = if a < b then (a, aId) else (b, bId)

    let wireDist = List.map (fun w ->
        let firstDist = orthDist w.Segments.Head.StartPos w.Segments.Head.EndPos pos

        ((w.Segments
        |> List.fold (fun currMin s ->
            let dist = orthDist s.StartPos s.EndPos pos
            min currMin (dist, s.Id))
            (firstDist, w.Segments.Head.Id)), w)) wModel.WX
    
    let sortedDist = List.sortBy (fun (x, _) -> fst x) wireDist
    
    match sortedDist.Head with
    | ((d, sId), w) when d < 10. -> Some (w.Id, sId)
    | _ -> None

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"


