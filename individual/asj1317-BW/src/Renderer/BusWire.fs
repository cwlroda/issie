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
    { StartPt: XYPos
      EndPt: XYPos
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
      SrcSymbol: CommonTypes.ComponentId
      TargetSymbol: CommonTypes.ComponentId
      WireSegments: WireSegment list
      WireColor: CommonTypes.HighLightColor
      WireWidth: int
      Selected: bool }



type Model = { Symbol: Symbol.Model; WX: Wire list }

type BoundingBox =
    { Pos: XYPos //Top left corner of box
      W: int
      H: int }
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

///--------------------Helpers Functions -------------------------------------///
/// Useful helper functions
let posOf x y = { X = x; Y = y }

/// Takes a set of coordinates and returns a new set of cooridantes adjusted according to the adjPos floats given
/// each in the X and Y respectively
let addjustedPos (orgPos: XYPos) ((adjPosX, adjPosY): (float * float)): XYPos =

    { X = orgPos.X + adjPosX
      Y = orgPos.Y + adjPosY }

//Given two points returns the coordinates for the point in the middle inbetween the given points
let midPt (aPos: XYPos) (bPos: XYPos): XYPos =
    { X = (aPos.X + bPos.X) / 2.
      Y = (aPos.X + bPos.Y) / 2. }

///Creates a BB type given two points and a width and height
/// Assumes that Pos is the mid point of the rectangule which the BBox represents
let createBoundingBox (w: int) (h: int) ((pt1, pt2): XYPos * XYPos): BoundingBox = { Pos = midPt pt1 pt2; W = w; H = h }

/// Takes a wire segment and the width of the wire as input and using its oritenation returns the correct BBox
let createSegBB (wireW: int) (seg: WireSegment): BoundingBox =
    match seg.Orien with
    | V -> createBoundingBox (4 + wireW) (int (seg.EndPt.Y - seg.StartPt.Y)) (seg.StartPt, seg.EndPt)
    | H -> createBoundingBox (int (seg.EndPt.X - seg.StartPt.X)) (4 + wireW) (seg.StartPt, seg.EndPt)

///Takes a point and a BBox and checks if the point are within the bounderies of the box
/// Returns true if the points is within the bounderires of the BBox
/// Otherwise returns false

let ptInBB (pt: XYPos) (bb: BoundingBox): bool =
    let diffX = pt.X - bb.Pos.X
    let diffY = pt.Y - bb.Pos.Y

    match diffX, diffY with
    | x, _ when abs (x) > ((float bb.W) / 2.) -> false
    | _, y when abs (y) > ((float bb.H) / 2.) -> false
    | _ -> true

///Takes as input a wire and a point and checks if the pt is within the BBox of any of the segments
/// Returns true if it is close to one of the segments
/// Returns false otherwise
let ptCloseToWire (pt: XYPos) (wire: Wire) =
    let ptCloseToSeg (seg: WireSegment): bool =
        seg |> createSegBB wire.WireWidth |> (ptInBB pt)

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



///Creates the wireSegement list which represents the verticies of the wire between the two points
let autoRoute (startPt: XYPos) (endPt: XYPos): WireSegment list =
    let midPt = (posOf endPt.X startPt.Y)

    [ { StartPt = startPt
        EndPt = midPt
        Orien = V }
      { StartPt = midPt
        EndPt = endPt
        Orien = H } ]

/// Creates a wire instance given the starting Port and the ending Port
let createWire (srcP: Symbol.Symbol) (tgtP: Symbol.Symbol): Wire =
    { Id = CommonTypes.ConnectionId(uuid ())
      SrcSymbol = srcP.Id
      TargetSymbol = tgtP.Id
      WireSegments = (autoRoute srcP.Pos tgtP.Pos)
      Selected = false
      WireWidth = 2
      WireColor = CommonTypes.Red }


/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    List.find (fun w -> w.Id = wId) wModel.WX


type WireRenderProps =
    { key: CommonTypes.ConnectionId
      WireP: Wire
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
            (List.pairwise [ props.SrcP
                             { X = props.TgtP.X; Y = props.SrcP.Y }
                             props.TgtP ]
             |> List.map (fun (a, b) -> wireSegView a b props.ColorP props.StrokeWidthP)
             |> ofList))


let view (model: Model) (dispatch: Dispatch<Msg>) =
    let wires =
        model.WX
        |> List.map
            (fun w ->
                let props =
                    { key = w.Id
                      WireP = w
                      SrcP = Symbol.symbolPos model.Symbol w.SrcSymbol
                      TgtP = Symbol.symbolPos model.Symbol w.TargetSymbol
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
            | r1, r2 when r1 = r2 -> symbols.[r1], symbols.[n - 1] // prevents wire target and source being same
            | r1, r2 -> symbols.[r1], symbols.[r2]

        createWire s1 s2

    List.map (fun i -> makeRandomWire ()) [ 1 .. n ]
    |> (fun wires -> { WX = wires; Symbol = symbols }, Cmd.none)

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        let sm, sCmd = Symbol.update sMsg model.Symbol
        { model with Symbol = sm }, Cmd.map Symbol sCmd
    | AddWire _ -> failwithf "Not implemented"
    | SetColor c -> failwithf "Not implemented \{model with Color = c\}, Cmd.none"
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol(Symbol.MouseMsg mMsg))


//---------------Other interface functions--------------------//

/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Used
/// to determine which wire (if any) to select on a mouse click
let wireToSelectOpt (wModel: Model) (pos: XYPos): CommonTypes.ConnectionId option =
    let wiresClose lst (w: Wire) =
        match (ptCloseToWire pos) w with
        | Some seg -> lst @ [ (w.Id, seg) ]
        | None -> lst


    let closestWire (lst: (CommonTypes.ConnectionId * WireSegment) list) =
        lst
        |> List.map (fun (w, seg) -> (w, (distPtToWire pos) seg))
        |> List.maxBy (fun (w, dist) -> -dist)
        |> fst


    match List.fold wiresClose [] wModel.WX with
    | [] -> None
    | [ (w, seg) ] -> Some w
    | wireLst -> Some(closestWire wireLst)


let setSetSelected (wModel: Model) (wID: CommonTypes.ConnectionId) =
    wModel.WX
    |> List.map
        (fun w ->
            if w.Id = wID then
                { w with Selected = true }
            else
                w)




let getSelected (wModel: Model): Wire list =
    wModel.WX |> List.filter (fun w -> w.Selected)

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId: CommonTypes.ComponentId): CommonTypes.Component = failwithf "Not implemented"

let extractWires (wModel: Model): CommonTypes.Component list = failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp: CommonTypes.Component) = failwithf "Not Implemented"
