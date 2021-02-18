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
type WireSegment = {
    Id: CommonTypes.ConnectionId
    Vertices: (XYPos * XYPos)
    LastDragPos: XYPos
    IsDragging : bool
    Color: CommonTypes.HighLightColor
}

type Wire = {
    Id: CommonTypes.ConnectionId 
    SrcSymbol: CommonTypes.ComponentId
    TargetSymbol: CommonTypes.ComponentId
    // InputPort: CommonTypes.InputPortId
    // OutputPort: CommonTypes.OutputPortId
    Segments: WireSegment list
    }

type Model = {
    Symbol: Symbol.Model
    WX: Wire list
    Color: CommonTypes.HighLightColor
    }

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
    | StartDragging of wId : CommonTypes.ConnectionId * sId : CommonTypes.ConnectionId * pagePos : XYPos
    | Dragging of wId : CommonTypes.ConnectionId * sId: CommonTypes.ConnectionId * pagePos: XYPos
    | EndDragging of wId : CommonTypes.ConnectionId * sId: CommonTypes.ConnectionId

//---------------------------------helper types and functions----------------//

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}


/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    wModel.WX
    |> List.find (fun elem -> elem.Id = wId)

type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    SrcP: XYPos 
    TgtP: XYPos
    ColorP: string
    StrokeWidthP: string
    Dispatch : Dispatch<Msg>
    }


let orthDist pos1 pos2 pos3 =
    let shoelaceArea =
        float(abs(pos1.X * pos2.Y + pos2.X * pos3.Y + pos3.X * pos1.Y
                - pos1.Y * pos2.X - pos2.Y * pos3.X - pos3.Y * pos1.X))

    let segmentLength =
        sqrt(float(abs(pos1.X - pos2.X)) ** 2. + float(abs(pos1.X - pos2.X)) ** 2.)
    
    shoelaceArea / segmentLength

/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Used
/// to determine which wire (if any) to select on a mouse click

let wireToSelectOpt (wModel: Model) (pos: XYPos) : (CommonTypes.ConnectionId * CommonTypes.ConnectionId) option = 
    let min (a, aId) (b, bId) = if a < b then (a, aId) else (b, bId)

    let wireDist = List.map (fun w ->
        ((w.Segments
        |> List.fold (fun currMin s ->
            min currMin ((orthDist (fst s.Vertices) (snd s.Vertices) pos), s.Id))
            ((orthDist (fst w.Segments.Head.Vertices) (snd w.Segments.Head.Vertices) pos), w.Segments.Head.Id)), w)) wModel.WX
    
    let sortedDist = List.sortBy (fun (x, _) -> fst x) wireDist
    
    match sortedDist.Head with
    | ((d, sId), w) when d < 10. -> Some (w.Id, sId)
    | _ -> None


/// dummy init for testing: real init would probably start with no wires.
/// this initialisation is not realistic - ports are not used
/// this initialisation depends on details of Symbol.Model type.
let basicRouting (s1 : XYPos) (s2 : XYPos) =
    let srcX = s1.X
    let srcY = s1.Y
    let tgtX = s2.X
    let tgtY = s2.Y
    let midX = 0.5 * (srcX + tgtX)
    let midY = 0.5 * (srcY + tgtY)

    let makeSegment x1 y1 x2 y2 =
        {
            Id=CommonTypes.ConnectionId (uuid())
            Vertices = ({X = x1
                         Y = y1}, 
                        {X = x2
                         Y = y2})
            LastDragPos = {X = 0.
                           Y = 0.}
            IsDragging = false
            Color = CommonTypes.Grey
        }
    
    match (tgtX - srcX) with
    | x when x >= 0. -> 
        match (tgtY - srcY) with
        | y when y <> 0. -> 
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeSegment srcX srcY midX srcY
            let (s2: WireSegment) = makeSegment midX srcY midX tgtY
            let (s3: WireSegment) = makeSegment midX tgtY tgtX tgtY
            s1::s2::s3::segList
        | y when y = 0. ->
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeSegment srcX srcY midX srcY
            s1::segList
        | _ -> failwithf "Not implemented"
    | x when x < 0. ->
        match (tgtY - srcY) with
        | y when y < 0. ->
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeSegment srcX srcY (srcX+60.) srcY
            let (s2: WireSegment) = makeSegment (srcX+60.) srcY (srcX+60.) (srcY-60.)
            let (s3: WireSegment) = makeSegment (srcX+60.) (srcY-60.) (tgtX-60.) (tgtY-60.)
            let (s4: WireSegment) = makeSegment (tgtX-60.) (tgtY-60.) (tgtX-60.) tgtY
            let (s5: WireSegment) = makeSegment (tgtX-60.) tgtY tgtX tgtY
            s1::s2::s3::s4::s5::segList
        | y when y >= 0. ->
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeSegment srcX srcY (srcX+60.) srcY
            let (s2: WireSegment) = makeSegment (srcX+60.) srcY (srcX+60.) (srcY+60.)
            let (s3: WireSegment) = makeSegment (srcX+60.) (srcY+60.) (tgtX-60.) (tgtY+60.)
            let (s4: WireSegment) = makeSegment (tgtX-60.) (tgtY+60.) (tgtX-60.) tgtY
            let (s5: WireSegment) = makeSegment (tgtX-60.) tgtY tgtX tgtY
            s1::s2::s3::s4::s5::segList
        | _ -> failwithf "Not implemented"
    | _ -> failwithf "Not implemented"

let updateWires (model : Model) = 
    let wireList =
        List.map (fun w ->
            let segList = basicRouting (Symbol.symbolPos model.Symbol w.SrcSymbol) (Symbol.symbolPos model.Symbol w.TargetSymbol)
            {w with Segments = segList}
        ) model.WX
    { model with WX = wireList }

let autoRouting (model: Model) = failwithf "Not implemented"
// if x >= 0
// if y < 0 -> wire goes up more
// if y >= 0 -> wire goes down more

// if x < 0
// if y < 0 -> wire goes up more
// if y >= 0 -> wire goes down more


/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.
let singleWireView (wModel: Model) = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let wire = props.WireP

            let wireSegments = 
                wire.Segments
                |> List.map (fun segment ->
                    let srcX = (fst segment.Vertices).X
                    let srcY = (fst segment.Vertices).Y
                    let tgtX = (snd segment.Vertices).X
                    let tgtY = (snd segment.Vertices).Y

                    g [] [
                            polyline [
                                OnMouseDown (fun ev -> 
                                    // See note above re coords wrong if zoom <> 1.0
                                    let mousePos = posOf ev.pageX ev.pageY
                                    
                                    match wireToSelectOpt wModel mousePos with
                                    | Some x -> SetColor CommonTypes.Blue
                                                |> props.Dispatch
                                        // StartDragging(fst x, snd x, mousePos)
                                        // |> props.Dispatch
                                    | None -> SetColor CommonTypes.Red
                                            |> props.Dispatch
                                    // document.addEventListener("mousemove", handleMouseMove.current)
                                )

                                OnMouseUp (fun ev -> 
                                    // See note above re coords wrong if zoom <> 1.0
                                    SetColor CommonTypes.Blue
                                    |> props.Dispatch
                                    // document.addEventListener("mousemove", handleMouseMove.current)
                                )

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

let view (model:Model) (dispatch: Dispatch<Msg>)=
    let wires = 
        (updateWires model).WX
        |> List.map (fun w ->
            let props = {
                key = w.Id
                WireP = w
                SrcP = Symbol.symbolPos model.Symbol w.SrcSymbol 
                TgtP = Symbol.symbolPos model.Symbol w.TargetSymbol 
                ColorP = model.Color.Text()
                StrokeWidthP = "2px"
                Dispatch = dispatch}
            singleWireView model props)
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    g [] [(g [] wires); symbols]



let init n () =
    let symbols, cmd = Symbol.init()
    let symIds = List.map (fun (sym:Symbol.Symbol) -> sym.Id) symbols
    let rng = System.Random 0
    let makeRandomWire() =
        let n = symIds.Length
        let s1,s2 =
            match rng.Next(0,n-1), rng.Next(0,n-2) with
            | r1,r2 when r1 = r2 -> 
                symbols.[r1],symbols.[n-1] // prevents wire target and source being same
            | r1,r2 -> 
                symbols.[r1],symbols.[r2]
        {
            Id=CommonTypes.ConnectionId (uuid())
            SrcSymbol = s1.Id
            TargetSymbol = s2.Id
            Segments = basicRouting (posOf s1.Component.X s1.Component.Y) (posOf s2.Component.X s2.Component.Y)
        }
    List.map (fun i -> makeRandomWire()) [1..n]
    |> (fun wires -> {WX=wires;Symbol=symbols; Color=CommonTypes.Grey},Cmd.none)



let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd
    | AddWire _ -> failwithf "Not implemented"
    | SetColor c -> {model with Color = c}, Cmd.none
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
                                    Vertices = (posAdd (fst s.Vertices) diff, posAdd (snd s.Vertices) diff)
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

//---------------Other interface functions--------------------//



//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"
