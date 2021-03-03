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



(* type ConnectedPorts =
    {
        SrcPort: CommonTypes.PortId list
        TargetPort: CommonTypes.PortId list
    }
 *)
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
      Error: string Option }

let BBOXSIZE = 2.0

type Model =
    { Symbol: Symbol.Model
      WX: Map<CommonTypes.ConnectionId, Wire>
      WireAnnotation: bool }


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
    | MouseMsg of MouseT // Not used but left for compatibility with supplied skeleton code
    | ToggleAnnotations // Used to show ability to turn on and off wire annotation


///--------------------Helpers Functions -------------------------------------///

let inLst v lst = List.exists (fun el -> v = el) lst

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

/// Takes as input a relative position between two points and outputs true if the two original points are horzontal and false otherwise
let isVertical (relPos: XYPos) : bool =
    abs(relPos.X)<abs(relPos.Y) 

///Give list of verticies outputs a string that will define a polyline
let lineDef (verts: XYPos list) =

    List.map (fun pos -> $"{pos.X},{pos.Y}") verts

    |> List.fold (fun outstr str -> outstr + " " + str) ""

/// Give list of string which is string that defines polygon of input BBox
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
let createSegBB (wireW: int) ((startPt, endPt): XYPos * XYPos) =
    let relDiff = posDiff startPt endPt

    let adjTopLeft =
        { X = -(BBOXSIZE + (float wireW) / 2.)
          Y = (BBOXSIZE + (float wireW) / 2.) }

    let wHCalc (diff: float) =
        match abs (diff) < 1.0 with
        | true -> 2. * BBOXSIZE + (float wireW)
        | _ -> (abs diff) + 2. * BBOXSIZE + float (wireW)

    let topLeftCorner =
        match relDiff.X, relDiff.Y with
        | x, y when (isVertical relDiff) && y > 0. -> posAdd startPt adjTopLeft
        | x, y when (isVertical relDiff) -> posAdd endPt adjTopLeft
        | x, _ when x > 0. -> posAdd endPt adjTopLeft
        | _, _ -> posAdd startPt adjTopLeft

    makeBBox topLeftCorner (wHCalc relDiff.X) (wHCalc relDiff.Y)

///Takes as input a wire and a point and checks if the pt is within the bboxes defining the wire
/// Returns true if it is close to one of the segments, returns false otherwise
let isTargetWire (pt: XYPos) (wire: Wire) =
    let ptCloseToSeg (segPts: XYPos * XYPos): bool =

        createSegBB wire.WireWidth segPts |> (ptInBB pt)

    List.pairwise wire.WireSegments
    |> List.exists ptCloseToSeg


let distPtToWire (pt: XYPos) (wire: Wire) =
    List.pairwise wire.WireSegments
    |> List.map (fun verts -> distPtToSeg pt verts)
    |> List.maxBy (~-)

let isTargetSeg wWidth pos segVerts =
    segVerts |> (createSegBB wWidth) |> (ptInBB pos)


    
/// Routing
(* let roundSym (startPt: XYPos) (relShift: XYPos) (endPt: XYPos) =
    let midVert = posAdd startPt relShift
    [ startPt; midVert; endPt ] *)

///Creates the wireSegement list which represents the verticies of the wire between the two points
let autoRoute (wModel: Model) (startId: CommonTypes.PortId) (endId: CommonTypes.PortId) =
    let startPos = Symbol.portPos wModel.Symbol startId
    let endPos = Symbol.portPos wModel.Symbol endId
    let midPos = midPt startPos endPos


    let wireDir = posDiff endPos startPos
    let getRelSymInfo portId  =
        let symBB = 
            Symbol.getHostId wModel.Symbol portId
            |> Symbol.symbolBBox wModel.Symbol
            
        posDiff symBB.Pos (Symbol.portPos wModel.Symbol portId), symBB.Pos, symBB.H


    let relStart,startSym, startH =  getRelSymInfo startId
    let relEnd,endSym, endH = getRelSymInfo endId
    
    let defSeg  pos wAdj hAdj=
        match wireDir.X with 
        | x when x > 0. -> [ pos; { X = midPos.X; Y = pos.Y } ]
        | _ -> [ pos; { X = pos.X + hAdj; Y = pos.Y }; { X = pos.X + hAdj; Y =  wAdj  } ]
    
        

    let initialSegs, finalSegs =
        defSeg  startPos (midPos.Y) 5., List.rev (defSeg endPos (midPos.Y) -5.) 

    initialSegs @ finalSegs

let movePortConnectionSeg (idx0, idx1) (move: XYPos) (segments: XYPos list) =
    List.mapi
        (fun idx vert ->
            match idx with
            | idx when idx0 = idx -> posDiff vert move
            | idx when idx1 = idx -> posDiff vert { X = 0.0; Y = move.Y }
            | _ -> vert)
        segments

let moveSegment (idx1, idx2) (move: XYPos) (orien: XYPos) (segments: XYPos list) =
    let relMove =
        if isVertical orien then { X = move.X; Y = 0.0 }
        else { X = 0.0; Y = move.Y }


    List.mapi
        (fun idx vert ->
            match idx with
            | idx when idx1 = idx -> posDiff vert relMove
            | idx when idx2 = idx -> posDiff vert relMove
            | _ -> vert)
        segments

let allWiresOfSymbol (wModel: Model) (symId: CommonTypes.ComponentId): CommonTypes.ConnectionId list =
    let portConnected =
        Symbol.getPortsOfSymbol wModel.Symbol symId

    Map.toList wModel.WX
    |> List.filter
        (fun (wId, w) ->
            (inLst w.SrcPort portConnected)
            || (inLst w.TargetPort portConnected))
    |> List.map fst

///Takes a position and a wireId, updates the position of the segment closest to the mouse
let manualRoutingAdj (wModel: Model) (wireId: CommonTypes.ConnectionId) (pos: XYPos) =
    let w = wire wModel wireId

    let segIdx =
        List.pairwise w.WireSegments
        |> List.tryFindIndex (isTargetSeg w.WireWidth w.LastPos)

    let move = posDiff w.LastPos pos


    let updatedWireSegs =
        match segIdx with
        | Some idx when idx = 0 -> w.WireSegments
        //updateConnectionSeg (idx, idx+1) move w.WireSegments
        | Some idx when idx = (List.length w.WireSegments) - 2 -> w.WireSegments
        //updateConnectionSeg (idx-1, idx) move w.WireSegments
        | Some idx ->
            let orien =
                posDiff w.WireSegments.[idx] w.WireSegments.[idx + 1]

            moveSegment (idx, idx + 1) move orien w.WireSegments
        | None -> w.WireSegments

    Map.add
        wireId
        { w with
              WireSegments = updatedWireSegs
              LastPos = pos }
        wModel.WX

let typesValid (port1, port2) (wModel: Model): Result<CommonTypes.PortId * CommonTypes.PortId, string> =

    let getType pId = (Symbol.portType wModel.Symbol pId)

    match getType port1, getType port2 with
    | pT1, pT2 when pT1 = pT2 -> Error $"Invalid Port Selection. The Ports cannot be both be {pT1}s."
    | p, _ when p = CommonTypes.PortType.Input -> Ok(port2, port1)
    | _ -> Ok(port1, port2)


let getWidthLabel (port1, port2) (wModel: Model): (string * string * Result<int, string>) =
    let getWidth pId = (Symbol.portWidth wModel.Symbol pId)

    match getWidth port1, getWidth port2 with
    | Some pW1, Some pW2 when pW1 <> pW2 ->
        $"{pW1}",
        $"{pW2}",
        Error
            $"Invalid Port Selection. Wire widths dont match, port widths of {pW1} bit(s) does not match widths of {pW2} bits"
    | None, Some w ->
        "Undetermined",
        $"{w}",
        Error $"Invalid Port Selection. Wire widths dont match, port widths of None does not match {w}bits"
    | Some w, None ->
        $"{w}",
        "Undetermined",
        Error $"Invalid Port Selection. Wire widths dont match, port widths of None does not match {w}bits"
    | Some w, _ -> $"{w}", $"{w}", Ok w
    | _, _ -> failwithf "Should not occur"

let notAvaliableInput (wModel: Model) (inputId: CommonTypes.PortId): bool =
    Map.exists (fun wId w -> w.TargetPort = inputId) wModel.WX


/// Given two port creates a wire instance. Does not route the Wire
let createWire
    (wModel: Model)
    (port1: CommonTypes.PortId)
    (port2: CommonTypes.PortId)
    (conId: CommonTypes.ConnectionId Option)
    (segRouting: XYPos list)
    : Wire =

    let (srcLabel, tgtLabel, widthValid) = getWidthLabel (port1, port2) wModel
    let validSrcTgt = typesValid (port1, port2) wModel

    let src, tgt, width, colour, err =
        match widthValid, validSrcTgt with
        | _, Ok (s, t) when (notAvaliableInput wModel t) ->
            s,
            t,
            5,
            CommonTypes.HighLightColor.Red,
            Some "Invalid Input port selection. An input port cannot have multiple input wires"
        | Ok w, Ok (s, t) when w < 2 -> s, t, w, CommonTypes.HighLightColor.Grey, None
        | Ok w, Ok (s, t) -> s, t, 3, CommonTypes.HighLightColor.Blue, None
        | Error errStr, Ok (s, t) -> s, t, 5, CommonTypes.HighLightColor.Red, Some errStr
        | _, Error errType -> port1, port2, 5, CommonTypes.HighLightColor.Red, Some errType

    let wId =
        function
        | Some s -> s
        | None -> CommonTypes.ConnectionId(uuid ())

    let wSegLst =
        if List.isEmpty segRouting then
            autoRoute wModel src tgt
        else
            segRouting

    { Id = wId conId
      SrcPort = src
      SrcLabel = srcLabel
      TargetPort = tgt
      TargetLabel = tgtLabel
      WireSegments = wSegLst
      LastPos = posOf 0.0 0.0
      WireColor = colour
      WireWidth = width
      Error = err }

let getWireColor (w: Wire): CommonTypes.HighLightColor =
    (match w with
     | w when w.Error <> None -> CommonTypes.HighLightColor.Red
     | w when w.WireWidth > 1 -> CommonTypes.HighLightColor.Blue
     | w -> CommonTypes.HighLightColor.Blue)



/// Updates the wire routing to adjust for any movement in symbols location, i.e. ports have moved
let updateSelectedWires (wModel: Model) (wIdLst: CommonTypes.ConnectionId list) =
    printf $"{wIdLst}"

    let updateModel w =
        { w with
              WireSegments = autoRoute wModel w.SrcPort w.TargetPort }

    Map.map
        (fun wId w ->
            if (inLst wId wIdLst) then
                updateModel w
            else
                w)
        wModel.WX

let fitToGrid (wModel: Model) =
    let adjToGrid pt =
        match (int pt) % 5 with
        | 0 -> 0.
        | v when v > 3 -> 5.0 - (float v)
        | v -> -float v
        |> (+) pt

    let fitwire vertLst =
        List.map (fun pt -> posOf (adjToGrid pt.X) (adjToGrid pt.Y)) vertLst


    Map.map
        (fun wIdd w ->
            { w with
                  WireSegments = (fitwire w.WireSegments) })
        wModel.WX




let addWire (wModel: Model) (port1: CommonTypes.PortId) (port2: CommonTypes.PortId) =
    let newWire = createWire wModel port1 port2 None []
    Map.add newWire.Id newWire wModel.WX


///Given a connectionId deletes the given wire
let deletWire (wModel: Model) (wId: CommonTypes.ConnectionId) = Map.remove wId wModel.WX

/// Update the colour on the given wire
let setWireColor (wModel: Model) (wId: CommonTypes.ConnectionId) (c: CommonTypes.HighLightColor): Wire =
    { (Map.find wId wModel.WX) with
          WireColor = c }


/// Update the colour
let setSelectedColor (wModel: Model) (wId: CommonTypes.ConnectionId): Map<CommonTypes.ConnectionId, Wire> =
    let w = wire wModel wId

    let updatedWire =
        setWireColor wModel wId CommonTypes.HighLightColor.Green

    Map.add wId updatedWire wModel.WX



/// Reset the color of all the wires except those set in red to highlight error
let setUnselectedColor (wModel: Model): Map<CommonTypes.ConnectionId, Wire> =
    Map.map (fun wId w -> getWireColor w |> setWireColor wModel wId) wModel.WX

let endDrag (wModel: Model): Map<CommonTypes.ConnectionId, Wire> =
    let wxUpdate = fitToGrid wModel

    let modelUpdate = { wModel with WX = wxUpdate }
    setUnselectedColor modelUpdate

let startDrag (wModel: Model) (wId: CommonTypes.ConnectionId) (pos: XYPos): Map<CommonTypes.ConnectionId, Wire> =
    let wx =
        Map.add wId { (wire wModel wId) with LastPos = pos } wModel.WX

    let updatedModel = { wModel with WX = wx }
    Map.add wId (setWireColor updatedModel wId CommonTypes.HighLightColor.Green) updatedModel.WX


/// react virtual DOM SVG for one wire

type SegRenderProps =
    { Key: CommonTypes.ConnectionId
      Seg: XYPos list
      ColorP: string
      StrokeWidthP: string }

type LabelRenderProps =
    { Key: CommonTypes.PortId
      Label: string
      ColorLabel: string
      Pos: XYPos }

type BBoxRenderProps = { Key: string; Box: BBox }



let private renderBBox =
    FunctionComponent.Of
        (fun (props: BBoxRenderProps) ->
            polygon [ SVGAttr.Points(boxDef props.Box.Pos props.Box.W props.Box.H)
                      SVGAttr.StrokeWidth "1px"
                      SVGAttr.Stroke "Black"
                      SVGAttr.FillOpacity 0.2
                      SVGAttr.Fill "Blue" ] [])

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
            text [ X props.Pos.X
                   Y props.Pos.Y
                   Style [ TextAnchor "left"
                           DominantBaseline "middle"
                           FontSize "10px"
                           Fill props.ColorLabel ] ] [
                str <| sprintf $"{props.Label}"
            ])

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
        if model.WireAnnotation then
            model.WX
            |> Map.toList
            |> List.fold
                (fun lst (wId, w) ->
                    [ (w.SrcPort,
                       w.SrcLabel,
                       w.WireColor,
                       (posAdd w.WireSegments.[0] (posOf 5.0 (float w.WireWidth - 10.))))
                      (w.TargetPort,
                       w.TargetLabel,
                       w.WireColor,
                       (posAdd w.WireSegments.[(w.WireSegments.Length) - 1] (posOf -10.0 (float w.WireWidth - 10.)))) ]
                    @ lst)
                []
            |> List.map
                (fun (pId, pLabel, c, pos) ->
                    let labelProps =
                        { Key = pId
                          Label = pLabel
                          ColorLabel = "Black"
                          Pos = pos }

                    singleLabelView labelProps)
        else
            []

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
///Dummy function to initialize for demo
let init n () =
    let symbols, cmd = Symbol.init ()

    { Symbol = symbols
      WX = Map.empty
      WireAnnotation = true },
    Cmd.none


let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        let sm, sCmd = Symbol.update sMsg model.Symbol

        let wx =
            match sMsg with
            | Symbol.Dragging (symId, _)
            | Symbol.EndDragging symId ->
                let wIdLst = allWiresOfSymbol model symId
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
        let wxUpdated = manualRoutingAdj model wMsgId wMsgPos
        { model with WX = wxUpdated }, Cmd.none
    | StartDragging (wMsgId, wMsgPos) ->
        let wxUpdated = startDrag model wMsgId wMsgPos
        { model with WX = wxUpdated }, Cmd.none
    | EndDragging ->
        let wxUpdated = endDrag model
        printf $"{wxUpdated}"
        { model with WX = wxUpdated }, Cmd.none
    | SetColor c ->
        let wxUpdated =
            Map.map (fun wId w -> setWireColor model wId c) model.WX

        { model with WX = wxUpdated }, Cmd.none
    | ToggleAnnotations ->
        { model with
              WireAnnotation = not model.WireAnnotation },
        Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol(Symbol.MouseMsg mMsg))


//---------------Other interface functions--------------------//


/// Give position finds the wire which is within a few pixels. If there are multiple chooses the closest one
let getTargetedWire (wModel: Model) (pos: XYPos): CommonTypes.ConnectionId Option =
    let closestWire (lst: (CommonTypes.ConnectionId * Wire) List): CommonTypes.ConnectionId =
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


let getErrors (wModel: Model): (string * XYPos) list =
    Map.fold
        (fun lst wId w ->
            match w.Error with
            | Some errStr ->
                [ errStr, (Symbol.portPos wModel.Symbol w.SrcPort) ]
                @ lst
            | None -> lst)
        []
        wModel.WX

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId: CommonTypes.ComponentId): CommonTypes.Component = failwithf "Not implemented"

let extractWires (wModel: Model): CommonTypes.Component list = failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp: CommonTypes.Component) = failwithf "Not Implemented"
