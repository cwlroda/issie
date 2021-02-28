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
    SrcSymbol: CommonTypes.ComponentId
    TargetSymbol: CommonTypes.ComponentId
    Color: CommonTypes.HighLightColor
    DragPositions: (XYPos * XYPos) option
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
    | AddWire of (CommonTypes.ConnectionId * CommonTypes.ConnectionId)
    | Delete of CommonTypes.ConnectionId
    | Select of CommonTypes.ConnectionId
    | UnselectAll
    | StartDrag of CommonTypes.ConnectionId * XYPos
    | Dragging of CommonTypes.ConnectionId * XYPos
    | EndDrag

/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    failwithf "Not impelmented"

type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    SrcP: XYPos 
    TgtP: XYPos
    ColorP: string
    DragPositions: (XYPos * XYPos) option
    StrokeWidthP: string }

// Interface functions

let getTargetedWire (model: Model) (pos: XYPos) : CommonTypes.ConnectionId option =
    let closestWire = model.WX
                      |> List.map (fun w ->
                          let distFrom c =
                              let a = Symbol.symbolPos model.Symbol w.SrcSymbol
                              let b = Symbol.symbolPos model.Symbol w.TargetSymbol

                              let normalize p =
                                  let length = posLength p
                                  posOf (p.X / length) (p.Y / length)

                              let ac = posDiff c a
                              let ab = posDiff b a
                              let bc = posDiff c b

                              let dot p1 p2 =
                                  let normP1 = normalize p1
                                  let normP2 = normalize p2
                                  normP1.X * normP2.X + normP1.Y * normP2.Y

                              if dot ab ac < 0. then
                                  posLength <| posDiff a c
                              else if dot ab bc > 0. then
                                  posLength <| posDiff b c
                              else 
                                  abs(ab.X * ac.Y - ab.Y * ac.X) / posLength ab

                          let dist = distFrom pos
                          (w, dist)
                      )
                      |> List.sortBy snd
                      |> List.tryHead

    match closestWire with
    | Some (w, d) when d < 10. -> Some w.Id
    | _ -> None

/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.

let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let (offsetX, offsetY) =
                match props.DragPositions with
                | Some (dStart, dEnd) -> (dEnd.X - dStart.X, dEnd.Y - dStart.Y)
                | None -> (0., 0.)

            line [
                X1 (props.SrcP.X + offsetX)
                Y1 (props.SrcP.Y + offsetY)
                X2 (props.TgtP.X + offsetX)
                Y2 (props.TgtP.Y + offsetY)
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
                SrcP = Symbol.symbolPos model.Symbol w.SrcSymbol 
                TgtP = Symbol.symbolPos model.Symbol w.TargetSymbol 
                ColorP = w.Color.Text()
                DragPositions = w.DragPositions
                StrokeWidthP = "2px" }
            singleWireView props)
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
            Color = CommonTypes.Red
            DragPositions=None
        }
    List.map (fun i -> makeRandomWire()) [1..n]
    |> (fun wires -> {WX=wires;Symbol=symbols},Cmd.none)

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let wires = match sMsg with
                    | Symbol.DeleteSymbols sIdLst ->
                        let sIdSet = Set.ofList sIdLst
                        model.WX
                        |> List.filter (fun w ->
                            not (
                                Set.contains w.SrcSymbol sIdSet
                                || Set.contains w.TargetSymbol sIdSet
                            )
                        )
                    | _ -> model.WX
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm;WX=wires}, Cmd.map Symbol sCmd
    | AddWire _ -> failwithf "Not implemented"
    | Delete wId ->
        let wires = model.WX
                    |> List.filter (fun w -> w.Id <> wId)

        { model with WX=wires }
        , Cmd.none
    | StartDrag (wId, p) ->
        let wires =
            model.WX
            |> List.map (fun w -> 
                if w.Id = wId then
                    {w with DragPositions=Some (p, p)}
                else
                    w
            )

        {model with WX=wires}, Cmd.none
    | Dragging (wId, p) ->
        let wires =
            model.WX
            |> List.map (fun w -> 
                if w.Id = wId then
                    let start = match w.DragPositions with
                                | Some (start, _) -> start
                                | None -> failwithf "Called Dragging before StartDrag!"
                    {w with DragPositions=Some (start, p)}
                else
                    w
            )

        {model with WX=wires}, Cmd.none
    | EndDrag ->
        let wires =
            model.WX
            |> List.map (fun w -> {w with DragPositions=None})

        {model with WX=wires}, Cmd.none
    | Select wId ->
        let wires =
            model.WX
            |> List.map (fun w -> 
                if w.Id = wId then
                    {w with Color=CommonTypes.Blue}
                else
                    {w with Color=CommonTypes.Red}
            )

        {model with WX=wires}, Cmd.none
    | UnselectAll ->
        let wires =
            model.WX
            |> List.map (fun w -> {w with Color=CommonTypes.Red})

        {model with WX=wires}, Cmd.none



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



    



