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
    { Id: CommonTypes.ComponentId
      StartPt: XYPos //Left/bottom
      EndPt: XYPos //Top/Right
      Orien: Orientation
      ColourSeg: CommonTypes.HighLightColor }
/// type for buswires
/// for demo only. The real wires will
/// connect to Ports - not symbols, where each symbol has
/// a number of ports (see Issie Component and Port types) and have
/// extra information for highlighting, width, etc.
/// NB - how you define Ports for drawing - whether they correspond to
/// a separate datatype and Id, or whether port offsets from
/// component coordinates are held in some other way, is up to groups.
type ConnectedPorts =
    {
        SrcPort: CommonTypes.PortId list
        TargetPort: CommonTypes.PortId list
    }

type Wire =
    { Id: CommonTypes.ConnectionId
      SrcPort: CommonTypes.PortId
      SrcLabel: string
      TargetPort: CommonTypes.PortId
      TargetLabel: string
      LastPos: XYPos
      WireSegments: XYPos list
      WireColor: CommonTypes.HighLightColor
      WireWidth: int
      Error: string Option
      }

let BBoxSize = 2.0

type Model =
    { Symbol: Symbol.Model
      WX: Map<CommonTypes.ConnectionId, Wire>
      BB: Helpers.BBox list
      WireAnnotation: bool
    }


//----------------------------Message Type-----------------------------------//

/// Messages to update buswire model
/// These are OK for the demo - but not the correct messages for
/// a production system. In the real system wires must connection
/// to ports, not symbols. In addition there will be other changes needed
/// for highlighting, width inference, etc
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (CommonTypes.PortId * CommonTypes.PortId)
    | SetSelected of CommonTypes.ConnectionId
    | DeleteWire of CommonTypes.ConnectionId
    | UnselectAll
    | StartDragging of wId: CommonTypes.ConnectionId * pos: XYPos
    | Dragging of wId: CommonTypes.ConnectionId * pos: XYPos
    | EndDragging
    | SetColor of color: CommonTypes.HighLightColor
    | MouseMsg of MouseT
    | ToggleAnnotations


///--------------------Helpers Functions -------------------------------------///


///Takes a point and a BBox and checks if the point are within the bounderies of the box
/// Returns true if the points is within the bounderires of the BBox
/// Otherwise returns false


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
///Give list of verticies outputs a string that will define a polyline
let lineDef (verts: XYPos list) =

    List.map (fun pos -> $"{pos.X},{pos.Y}") verts

    |> List.fold (fun outstr str -> outstr + " " + str) ""

let boxDef (pos: XYPos) (w: float) (h: float): string =
    let tL = pos
    let tR = posOf (pos.X + w) (pos.Y)
    let bR = posOf (pos.X + w) (pos.Y - h)
    let bL = posOf (pos.X) (pos.Y - (float h))
    $"{bL.X},{bL.Y} {tL.X},{tL.Y} {tR.X},{tR.Y} {bR.X},{bR.Y}"


/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    match Map.tryFind wId wModel.WX with
    | Some vWire -> vWire
    | None -> failwithf "Invalid Id passed"

/// Takes a wire segment and the width of the wire as input and using its oritenation returns the correct BBox
let createSegBB (wireW: int) ((startPt, endPt): XYPos * XYPos) = // (seg: WireSegment): BBox =

    let relDiff = posDiff startPt endPt

    let adjTopLeft =
        { X =  -(BBoxSize + (float wireW)/2.)
          Y =  (BBoxSize + (float wireW)/2.) }

    let wHCalc (diff: float) =
        match abs (diff) < 1.0 with
        | true ->   2.*BBoxSize + (float wireW)
        | _ -> (abs diff) +  2.*BBoxSize + float (wireW)

    let topLeftCorner =
        match relDiff.X, relDiff.Y with 
        | x,y when ((abs y) > (abs x))  && y > 0. -> posAdd startPt adjTopLeft
        | x,y when ((abs y) > (abs x)) -> posAdd endPt adjTopLeft
        | x,_ when x > 0. -> posAdd endPt adjTopLeft
        |_ ,_ -> posAdd startPt adjTopLeft

    makeBBox topLeftCorner (wHCalc relDiff.X) (wHCalc relDiff.Y)

///Takes as input a wire and a point and checks if the pt is within the BBox of any of the segments
/// Returns true if it is close to one of the segments
/// Returns false otherwise
let ptCloseToWire (pt: XYPos) (wire: Wire) =
    let ptCloseToSeg segPts: bool =

        createSegBB wire.WireWidth segPts |> (ptInBB pt)

    List.pairwise wire.WireSegments
    |> List.exists ptCloseToSeg


let distPtToWire (pt: XYPos) (wire: Wire) =
    List.pairwise wire.WireSegments
    |> List.map (fun verts -> distPtToSeg pt verts)
    |> List.maxBy (fun dist -> -dist)

/// Routing
let roundSym (startPt: XYPos) (relShift: XYPos) (endPt: XYPos) =
    let midVert = posAdd startPt relShift
    let orLst = [ H; V; H ]
    [ startPt; midVert; endPt ]

///Creates the wireSegement list which represents the verticies of the wire between the two points
let autoRoute (wModel: Model) (startId: CommonTypes.PortId) (endId: CommonTypes.PortId) =
    let startPos = Symbol.portPos wModel.Symbol startId
    let endPos = Symbol.portPos wModel.Symbol endId
    let midPos = midPt startPos endPos


    let wireDir = posDiff endPos startPos
    (* let endBBox = Symbol.symbolBBox wModel.Symbol (Symbol.getHostId wModel.Symbol endId)
    let startBBox = Symbol.symbolBBox wModel.Symbol (Symbol.getHostId wModel.Symbol startId)

    let relEndHost = posDiff endBBox.Pos midPos
    let relStartHost = posDiff startBBox.Pos midPos *)

    let initialSegs, finalSegs =
        match wireDir.X with
        | x when x > 0. ->
            [ startPos
              { X = midPos.X; Y = startPos.Y } ],
            [ { X = midPos.X; Y = endPos.Y }
              endPos ]
 
        | _ ->
            [ startPos
              { X = startPos.X + 5.0; Y = startPos.Y }
              { X = startPos.X + 5.; Y = midPos.Y } ],
            [ { X = endPos.X - 5.0; Y = midPos.Y }
              { X = endPos.X - 5.0; Y = endPos.Y }
              endPos ]
   

    initialSegs @ finalSegs

let updateConnectionSeg (idx0, idx1) (move:XYPos) (segments: XYPos list) =
    List.mapi (fun idx vert ->
        match idx with
        | idx when idx0 = idx -> posDiff vert move
        | idx when idx1 = idx -> posDiff vert {X=0.0; Y=move.Y}
        | _ -> vert
        ) segments

let updateVerts (idx1, idx2) (move: XYPos) (orien: XYPos) (segments: XYPos list) =
    let relMove =
        match orien.X, orien.Y with
        | x, y when abs (x) < abs (y) -> { X = move.X; Y = 0.0 }
        | _ -> { X = 0.0; Y = move.Y }
        
    List.mapi
        (fun idx vert ->
            match idx with
            | idx when idx1 = idx ->
                posDiff vert relMove
            | idx when idx2 = idx ->
                posDiff vert relMove
            | _ -> vert)
        segments

let wiresConnectedToSymbol (wModel: Model) (symId: CommonTypes.ComponentId) : CommonTypes.ConnectionId list =
    let inLst pLst portId=
        List.exists (fun el -> el = portId) pLst
    let portConnected = Symbol.getPortsOfSymbol wModel.Symbol symId
    
    Map.toList wModel.WX
    |> List.filter (fun (wId, w) -> (inLst portConnected w.SrcPort) || (inLst portConnected w.TargetPort))
    |> List.map fst 

 ///Takes a position and a wireId, updates the position of the segment closest to the mouse
let manualRouting (wModel: Model) (wireId: CommonTypes.ConnectionId) (pos: XYPos) =
    let w = wire wModel wireId

    let isTarget wWidth segPts =
        segPts
        |> (createSegBB wWidth)
        |> (ptInBB w.LastPos)

    let segIdx =
        List.pairwise w.WireSegments
        |> List.findIndex (isTarget w.WireWidth)
    let move = posDiff w.LastPos pos
    

    let updatedWireSegs = 
            match segIdx with
                | idx when  idx = 0 -> 
                    updateConnectionSeg (idx, idx+1) move w.WireSegments
                | idx when idx = (List.length w.WireSegments)-1 ->  
                    updateConnectionSeg (idx-1, idx) move w.WireSegments
                | idx -> 
                    let orien = posDiff w.WireSegments.[idx] w.WireSegments.[idx + 1]
                    updateVerts (idx, idx+1) move orien w.WireSegments
                        

    Map.add
        wireId
        { w with
              WireSegments = updatedWireSegs
              LastPos = pos }
        wModel.WX

/// Given two port creates a wire connection and  which it auto routes
let createWire
    (wModel: Model)
    (port1: CommonTypes.PortId)
    (port2: CommonTypes.PortId)
    (conId: CommonTypes.ConnectionId Option) 
    : Wire =
    let getPortType = Symbol.portType wModel.Symbol

    let getType pId =
        (Symbol.portType wModel.Symbol pId)
        
    let getWidth pId =
        (Symbol.portWidth wModel.Symbol pId)

    let avaliableInput inputId=
        Map.forall (fun wId w -> w.TargetPort <> inputId) wModel.WX

    let typeValid: Result<CommonTypes.PortId* CommonTypes.PortId, string > =
        match getType port1, getType port2 with
        | pT1, pT2 when pT1 = pT2 -> 
            Error  $"Invalid Port Selection. The Ports cannot be both be {pT1}s."
        | p, _ when p = CommonTypes.PortType.Input -> Ok (port2, port1) 
        | _ ->  Ok (port1, port2)



    let (srcLabel,tgtLabel, widthValid): (string*string*Result<int, string >) =
        match getWidth port1, getWidth port2 with 
        | Some pW1, Some pW2 when pW1 <> pW2 -> $"{pW1}",$"{pW2}",Error  $"Invalid Port Selection. Wire widths dont match, port widths of {pW1} bit(s) does not match widths of {pW2} bits"
        | None, Some w -> "Undetermined",$"{w}", Error $"Invalid Port Selection. Wire widths dont match, port widths of None does not match {w}bits" 
        | Some w, None -> $"{w}", "Undetermined", Error $"Invalid Port Selection. Wire widths dont match, port widths of None does not match {w}bits" 
        | Some w, _ -> $"{w}",$"{w}", Ok w 
        | _,_ -> failwithf "Should not occur"


    let src, tgt, width, colour, err =
        match widthValid, typeValid with
        |Ok w, Ok (s,t) when (avaliableInput t) && w<2-> s,t, w, CommonTypes.HighLightColor.Grey, None
        |Ok w, Ok (s,t) when (avaliableInput t) -> s,t, 3, CommonTypes.HighLightColor.Blue, None
        |Ok w, Ok (s,t) -> s,t, 5, CommonTypes.HighLightColor.Red, Some "Invalid Input port selection. An input port cannot have multiple input wires"
        | Error errStr , Ok (s, t) when (avaliableInput t) ->s, t ,5, CommonTypes.HighLightColor.Red, Some errStr
        | Error errStr , Ok (s, t) ->s, t , 5, CommonTypes.HighLightColor.Red, Some "Invalid Input port selection. An input port cannot have multiple input wires"
        | _, Error errType -> port1, port2, 5, CommonTypes.HighLightColor.Red, Some errType
        
    let wId = 
        function | Some s -> s | None ->  CommonTypes.ConnectionId(uuid ())   
        
    let wSegLst = autoRoute wModel src tgt
 
    { Id = wId conId
      SrcPort = src
      SrcLabel = srcLabel
      TargetPort = tgt
      TargetLabel = tgtLabel
      WireSegments = []
      LastPos = wSegLst.[0]
      WireColor = colour
      WireWidth = width
      Error = err
      }



/// Updates the wire routing to adjust for any movement in symbols location, i.e. ports have moved 
let updateSelectedWires (wModel: Model) (wIdLst: CommonTypes.ConnectionId list) =
    let updateModel wId w =
        Map.add
            wId
            { w with
                  WireSegments = autoRoute wModel w.SrcPort w.TargetPort }
            wModel.WX

    let foldfunc wxModel wId =
        match Map.tryFind wId wxModel with
        | Some w -> updateModel wId w
        | None -> wxModel

    List.fold foldfunc wModel.WX wIdLst

let fitToGrid (wModel: Model) =
    let adj pt =
        
        match (int pt)%5 with
        | 0 -> 0.
        | v when v > 3 ->  5.0 - (float v)
        | v -> - float v
        |> (+) pt
    let fitwire vertLst =
        List.map (fun pt -> posOf (adj pt.X) (adj pt.Y)) vertLst
       

    Map.map (fun wIdd w -> {w with WireSegments = (fitwire w.WireSegments)}) wModel.WX

    



let addWire (wModel: Model)
    (port1: CommonTypes.PortId)
    (port2: CommonTypes.PortId) = 
    let w = createWire wModel port1 port2 None 

    let wSegLst = autoRoute wModel w.SrcPort w.TargetPort

    Map.add w.Id {w with WireSegments = wSegLst} wModel.WX

    
///Given a connectionId deletes the given wire
let deletWire (wModel: Model) (wId: CommonTypes.ConnectionId) = 
    Map.remove wId wModel.WX

/// Update the colour on the given wire
let setColor
    (wModel: Model)
    (c: CommonTypes.HighLightColor)
    (wId: CommonTypes.ConnectionId)
    : Map<CommonTypes.ConnectionId, Wire> =
    let updateWire =
        { (Map.find wId wModel.WX) with
              WireColor = c }

    Map.add wId updateWire wModel.WX
/// Update the colour
let setSelectedColor (wModel: Model) (wID: CommonTypes.ConnectionId): Map<CommonTypes.ConnectionId, Wire> =
    let w = wire wModel wID

    wModel.WX.Add(
        wID,
        { w with
              WireColor = CommonTypes.HighLightColor.Green }
    )

/// Reset the color of all the wires except those set in red to highlight error
let setUnselectedColor (wModel: Model): Map<CommonTypes.ConnectionId, Wire> =
    Map.map
        (fun wId w ->
            if w.Error = None then
                { w with
                      WireColor = CommonTypes.HighLightColor.Blue }
            else
                {w with WireColor = CommonTypes.HighLightColor.Red})
        wModel.WX

let endDrag  (wModel: Model): Map<CommonTypes.ConnectionId, Wire> =   
    let wxUpdate = 
        
        fitToGrid  wModel

    let modelUpdate = { wModel with WX = wxUpdate}
    setUnselectedColor modelUpdate

let startDrag (wModel: Model) (wId: CommonTypes.ConnectionId) (pos: XYPos): Map<CommonTypes.ConnectionId, Wire> =
    let wx =
        Map.add wId { (wire wModel wId) with LastPos = pos } wModel.WX

    setColor { wModel with WX = wx } CommonTypes.HighLightColor.Green wId




/// react virtual DOM SVG for one wire

type SegRenderProps =
    { Key: CommonTypes.ConnectionId
      Seg: XYPos list
      ColorP: string
      StrokeWidthP: string }
type LabelRenderProps =
    { 
        Key: CommonTypes.PortId
        Label: string
        ColorLabel: string
        Pos: XYPos
        }

type BBoxRenderProps = { Key: string; Box: BBox }

let singleBBox (box: BBoxRenderProps) =

    polygon [ SVGAttr.Points(boxDef box.Box.Pos box.Box.W box.Box.H)
              SVGAttr.StrokeWidth "1px"
              SVGAttr.Stroke "Black"
              SVGAttr.FillOpacity 0.2
              SVGAttr.Fill "Blue" ] []


let private renderBBox =
    FunctionComponent.Of(fun (props: BBoxRenderProps) -> singleBBox props)

let singleWireView =
    FunctionComponent.Of
        (fun (props: SegRenderProps) ->
            polyline [ SVGAttr.Points(lineDef props.Seg)
                       SVGAttr.Stroke props.ColorP     
                       SVGAttr.StrokeWidth props.StrokeWidthP
                       SVGAttr.FillOpacity 0.0 ] [])
let singleLabelView =
    FunctionComponent.Of
        (fun (props: LabelRenderProps) ->
            text 
                [ 
                    X props.Pos.X;
                    Y props.Pos.Y;
                    Style [
                    TextAnchor "left"
                    DominantBaseline "middle"
                    FontSize "10px"
                    Fill props.ColorLabel
                ]
             ] [str <| sprintf $"{props.Label}"])

///To illustrate where bbox sit
///
let createBB wLst =
    Map.toList wLst
    |> List.fold
        (fun lst (wId, w) ->
            (List.pairwise w.WireSegments
             |> List.map (fun seg -> (createSegBB w.WireWidth) seg))
            @ lst)
        []



let view (model: Model) (dispatch: Dispatch<Msg>) =
    let wires =
        model.WX
        |> Map.toList
        |> List.map
            (fun (wId, w) ->
                let props =
                    { Key = wId
                      Seg = w.WireSegments
                      ColorP = w.WireColor.ToString()
                      StrokeWidthP = $"%d{w.WireWidth}" }
                singleWireView props)
    
    let wireAnnotations = 
        if model.WireAnnotation
        then
            model.WX
            |> Map.toList
            |> List.fold (fun lst (wId, w) -> [(w.SrcPort, w.SrcLabel, w.WireColor, (posAdd w.WireSegments.[0] (posOf 5.0 (float w.WireWidth - 10.)))); (w.TargetPort, w.TargetLabel, w.WireColor, (posAdd w.WireSegments.[(w.WireSegments.Length)-1] (posOf -10.0 (float w.WireWidth - 10.))))]@lst) []
            |> List.map (fun (pId, pLabel,c,pos) -> 
                let labelProps = 
                    {
                        Key = pId
                        Label = pLabel
                        ColorLabel = "Black"
                        Pos = pos
                    }
                singleLabelView labelProps)
        else []  
    let bBoxs =
         createBB model.WX
        |> List.map
            (fun bb ->
                let propsBB = { Key = Helpers.uuid (); Box = bb }
                renderBBox propsBB)

    let symbols =
        Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))

    g [] [
        (g [] wires)
        (g [] wireAnnotations)
        (g [] bBoxs)
        symbols
    ]

/// dummy init for testing: real init would probably start with no wires.
let init n () =
    let symbols, cmd = Symbol.init ()

 
    { Symbol = symbols
      WX = Map.empty
      BB = []
      WireAnnotation = true
      }, Cmd.none


let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        let sm, sCmd = Symbol.update sMsg model.Symbol
        let wx = 
            match sMsg with
            | Symbol.Dragging (symId,_) -> 
                let wIdLst = wiresConnectedToSymbol model symId
                updateSelectedWires model wIdLst
            | _ -> model.WX
        { model with Symbol = sm; WX = wx }, Cmd.map Symbol sCmd
        
    | AddWire (wMsgId1, wMsgId2) ->
        let wxUpdated = addWire model wMsgId1 wMsgId2
        { model with WX = wxUpdated }, Cmd.none
    | DeleteWire wMsg -> 
        let wxUpdated = deletWire model wMsg
        { model with WX = wxUpdated }, Cmd.none
    | SetSelected wMsg ->
        let wxUpdated = (setSelectedColor model wMsg)
        { model with WX = wxUpdated }, Cmd.none
    | UnselectAll ->
        { model with
              WX = setUnselectedColor model },
        Cmd.none
    | Dragging (wMsgId, wMsgPos) ->
        let wxUpdated = manualRouting model wMsgId wMsgPos
        //let wxUpdated = updateSelectedWires model wMsgId
        { model with WX = wxUpdated }, Cmd.none
    | StartDragging (wMsgId, wMsgPos) ->
        let oldWX = model.WX
        let wxUpdated = startDrag model wMsgId wMsgPos
        (* let bb = createBB wxUpdated *)

        { model with
              WX = wxUpdated
              //BB = bb 
              },
        Cmd.none
    | EndDragging->
        let wxUpdated = endDrag model
        printf $"{wxUpdated}"
        { model with
              WX = wxUpdated},
        Cmd.none
    | SetColor c-> 
        (Map.fold (fun wModel wId w -> {wModel with WX = (setColor wModel c wId)}) model model.WX), Cmd.none
    | ToggleAnnotations ->
        {model with WireAnnotation = not model.WireAnnotation}, Cmd.none

    | MouseMsg mMsg-> model, Cmd.ofMsg (Symbol(Symbol.MouseMsg mMsg))
        


//---------------Other interface functions--------------------//


/// Give position finds the wire which is within a few pixels. If there are multiple chooses the closest one
let getTargetedWire (wModel: Model) (pos: XYPos): CommonTypes.ConnectionId Option =
    let isTarget w =
        match ptCloseToWire pos w with
        | true -> [ (w.Id, w) ]
        | false -> []

    let closestWire (lst: (CommonTypes.ConnectionId * Wire) List): CommonTypes.ConnectionId =
        List.map (fun (wId, w) -> (wId, distPtToWire pos w)) lst
        |> List.maxBy (fun (w, dist) -> -dist)
        |> fst

    let wiresCloseToPt =
        Map.fold (fun lst kWire vWire -> (isTarget vWire) @ lst) [] wModel.WX

    match wiresCloseToPt with
    | [ (wId, w) ] -> Some wId
    | [] -> None
    | lst -> Some(closestWire lst)


let getWiresInTargetBBox (wModel: Model) (bbox: BBox): CommonTypes.ConnectionId list =
    let wireInBBox (w: Wire) =
        List.exists (fun vert -> ptInBB vert bbox) w.WireSegments

    Map.fold
        (fun lst k w ->
            if wireInBBox w then
                [ k ] @ lst
            else
                lst)
        []
        wModel.WX

let getErrors (wModel: Model) : (string*XYPos) list =
    let err = 
        Map.fold (fun lst wId w -> 
            match w.Error with
            | Some errStr -> [errStr, (Symbol.portPos wModel.Symbol w.SrcPort)]@lst
            | None -> lst
            ) [] wModel.WX
    printf $"{err}"
    err
//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId: CommonTypes.ComponentId): CommonTypes.Component = failwithf "Not implemented"

let extractWires (wModel: Model): CommonTypes.Component list = failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp: CommonTypes.Component) = failwithf "Not Implemented"
