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
type Wire = {
    Id: CommonTypes.ConnectionId 
    SrcPort: CommonTypes.PortId
    TargetPort: CommonTypes.PortId
    Selected: bool
    LastDragPos: XYPos
    DragOffset: XYPos
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
    | AddWire of (CommonTypes.PortId * CommonTypes.PortId)
    | SetColor of CommonTypes.HighLightColor
    | MouseMsg of MouseT
    | SetSelected of CommonTypes.ConnectionId
    | UnselectAll
    | DeleteWire of CommonTypes.ConnectionId
    | StartDrag of CommonTypes.ConnectionId * XYPos
    | Dragging of CommonTypes.ConnectionId * XYPos
    | EndDrag

let getErrors (model : Model) : Error list =
    model.WX
    |> List.map (fun w ->
        match w.Id with
        | CommonTypes.ConnectionId id ->
            {
                Msg = "Dummy error with wire " + id
                Pos = Symbol.portPos model.Symbol w.TargetPort
            }
    )

let getTargetedWire (wModel : Model) (pos : XYPos) : CommonTypes.ConnectionId Option = 
    let pointOnWire (wire : Wire) (point : XYPos) : bool =
        let src = Symbol.portPos wModel.Symbol wire.SrcPort
        let dst = Symbol.portPos wModel.Symbol wire.TargetPort

        let wireBox = extendBBoxForError (bboxFromDiagonals src dst) 10.

        if not (containsPoint pos wireBox) then
            false
        else
            // y = mx + c
            let m = (dst.Y - src.Y)/(dst.X - src.X)
            let c  = dst.Y - m*dst.X

            if c > 3000. || c < -3000. then
                true
            else
                let err = (point.Y - m*point.X - c)
                let err = if err < 0. then (err * -1.) else err

                err < 10.

    let res = 
        wModel.WX
        |> List.tryFind (
            fun wire ->
                pointOnWire wire pos
        )
    match res with
    | Some wire -> Some wire.Id
    | None -> None

/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    failwithf "Not impelmented"

type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    SrcP: XYPos 
    TgtP: XYPos
    Offset: XYPos
    ColorP: string
    StrokeWidthP: string }

/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.

let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            line [
                X1 (props.SrcP.X + props.Offset.X)
                Y1 (props.SrcP.Y + props.Offset.Y)
                X2 (props.TgtP.X + props.Offset.X)
                Y2 (props.TgtP.Y + props.Offset.Y)
                // Qualify these props to avoid name collision with CSSProp
                SVGAttr.Stroke props.ColorP
                SVGAttr.StrokeWidth props.StrokeWidthP ] [])


let view (model:Model) (dispatch: Dispatch<Msg>)=
    let wires = 
        model.WX
        |> List.map (fun w ->
            let props = {
                key = w.Id
                WireP = w
                SrcP = Symbol.portPos model.Symbol w.SrcPort
                TgtP = Symbol.portPos model.Symbol w.TargetPort
                Offset = w.DragOffset
                ColorP = if w.Selected then "blue" else "teal"
                StrokeWidthP = if w.Selected then "2.5px" else "2px" }
            singleWireView props)
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    g [] [(g [] wires); symbols]

/// dummy init for testing: real init would probably start with no wires.
/// this initialisation is not realistic - ports are not used
/// this initialisation depends on details of Symbol.Model type.
let init () =
    let symbols, cmd = Symbol.init()
    {WX=[];Symbol=symbols; Color=CommonTypes.Red},Cmd.none

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let delWires = 
            match sMsg with
            | Symbol.DeleteSymbols sIds ->
                sIds
                |> List.map (fun sId ->
                    let symPorts = Symbol.getPortsOfSymbol model.Symbol sId
                    model.WX
                    |> List.filter (fun w -> List.contains w.SrcPort symPorts || List.contains w.TargetPort symPorts)
                )
                |> List.collect id
                |> List.map (fun w -> w.Id)

            | _ -> []
        let newWx =
            model.WX
            |> List.filter (fun w -> not(List.contains w.Id delWires))
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm; WX=newWx}, Cmd.map Symbol sCmd
    | AddWire (sPid, tPid) ->
        let wires =
            model.WX
            |> List.append [{
                Id = CommonTypes.ConnectionId ( uuid() )
                SrcPort = sPid
                TargetPort = tPid
                Selected = false
                DragOffset = posOf 0. 0.
                LastDragPos = posOf 0. 0.
            }]
        {model with WX=wires},Cmd.none
    | SetColor c -> {model with Color = c}, Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))
    | SetSelected wId ->
        let newWx =
            model.WX
            |> List.map (fun w ->
                if w.Id = wId then
                    {w with Selected = true}
                else
                    w
            )

        {model with WX = newWx}, Cmd.none
    | UnselectAll ->
        let newWx =
            model.WX
            |> List.map (fun w ->
                {w with Selected = false}
            )

        {model with WX = newWx}, Cmd.none

    | DeleteWire wId ->
        let newWx = List.collect (fun el -> if el.Id = wId then [] else [el]) model.WX
        {model with WX = newWx}, Cmd.none

    | StartDrag (wId, pos) ->
        let newWx =
            model.WX
            |> List.map (fun w ->
                if w.Id = wId then
                    {w with
                        LastDragPos = pos}
                else
                    w    
            )
        {model with WX = newWx}, Cmd.none
    | Dragging (wId, pos) ->
        let newWx =
            model.WX
            |> List.map (fun w ->
                if w.Id = wId then
                    let diff = posDiff pos w.LastDragPos
                    {w with
                        DragOffset = posAdd w.DragOffset diff
                        LastDragPos = pos}
                else
                    w    
            )
        {model with WX = newWx}, Cmd.none
    | EndDrag ->
        let newWx =
            model.WX
            |> List.map (fun w ->
                {w with
                    DragOffset = posOf 0. 0.}
            )
        {model with WX = newWx}, Cmd.none

//---------------Other interface functions--------------------//

/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Used
/// to determine which wire (if any) to select on a mouse click
let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option = 
    failwith "Not implemented"

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"
