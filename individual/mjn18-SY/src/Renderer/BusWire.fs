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
    // SrcPort: CommonTypes.Port // This has to be an output port
    TargetSymbol: CommonTypes.ComponentId
    // TargetPort: CommonTypes.Port // This has to be an input port
    
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




/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    List.find (fun elem -> elem.Id=wId) wModel.WX
    

type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    SrcP: XYPos 
    TgtP: XYPos
    ColorP: string
    StrokeWidthP: string }

/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.

let singleWireView = 
    fun (props: WireRenderProps) ->
        let srcX = props.SrcP.X 
        let srcY = props.SrcP.Y
        let midX = 0.5 * (props.SrcP.X + props.TgtP.X)
        let midY = 0.5 * (props.SrcP.Y + props.TgtP.Y)
        let tgtX = props.TgtP.X
        let tgtY = props.TgtP.Y
        [
        polyline [
            match srcX - tgtX with
            | x when x < 0. -> SVGAttr.Points (sprintf "%f %f, %f %f, %f %f, %f %f" srcX srcY midX srcY midX tgtY tgtX tgtY)
            | x when x = 0. -> SVGAttr.Points (sprintf "%f %f, %f %f" srcX srcY tgtX tgtY)
            | _ ->
                match srcY-tgtY with 
                | x when x <= 0. -> SVGAttr.Points (sprintf "%f %f, %f %f, %f %f, %f %f, %f %f, %f %f" srcX srcY (srcX+60.) srcY (srcX+60.) (tgtY+60.) (tgtX-60.) (tgtY+60.) (tgtX-60.) (tgtY) tgtX tgtY)
                | _ -> SVGAttr.Points (sprintf "%f %f, %f %f, %f %f, %f %f, %f %f, %f %f" srcX srcY (srcX+60.) srcY (srcX+60.) (tgtY-60.) (tgtX-60.) (tgtY-60.) (tgtX-60.) (tgtY) tgtX tgtY)
            // Qualify these props to avoid name collision with CSSProp
            SVGAttr.Stroke props.ColorP
            SVGAttr.FillOpacity 0
            SVGAttr.StrokeWidth props.StrokeWidthP ] []
        // text [ // a demo text svg element
        //                 X (srcX + 40.); 
        //                 Y (srcY - 20.); 
        //                 Style [
        //                     TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
        //                     DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
        //                     FontSize "18px"
        //                     FontWeight "Bold"
        //                     Fill "Blue" // demo font color
        //                 ]
        //             ] [str <| sprintf "0..x"]
        ]
let busWidthView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let srcX = props.SrcP.X 
            let srcY = props.SrcP.Y
            let midX = 0.5 * (props.SrcP.X + props.TgtP.X)
            let midY = 0.5 * (props.SrcP.Y + props.TgtP.Y)
            let tgtX = props.TgtP.X
            let tgtY = props.TgtP.Y
            
            polyline [
                SVGAttr.Points (sprintf "%f %f, %f %f, %f %f, %f %f" srcX srcY midX srcY midX tgtY tgtX tgtY)
                
                // Qualify these props to avoid name collision with CSSProp
                SVGAttr.Stroke props.ColorP
                SVGAttr.FillOpacity 0
                SVGAttr.StrokeWidth props.StrokeWidthP ] [] 
            )

let view (model:Model) (dispatch: Dispatch<Msg>)=
    let wires = 
        model.WX
        |> List.map (fun w ->
            let props = {
                key = w.Id
                WireP = w
                SrcP = Symbol.symbolPos model.Symbol w.SrcSymbol 
                TgtP = Symbol.symbolPos model.Symbol w.TargetSymbol 
                ColorP = model.Color.Text()
                StrokeWidthP = "2px" }
            singleWireView props)
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    g [] [(g [] (List.concat wires)); symbols]

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
        }
    // List.map (fun i -> makeRandomWire()) [[1..n]]
    List.map (fun i -> makeRandomWire()) [[1..n]]
    |> (fun wires -> {WX=[];Symbol=symbols; Color=CommonTypes.Red},Cmd.none)

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd
    | AddWire _ -> failwithf "Not implemented"
    | SetColor c -> {model with Color = c}, Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))

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



    



