module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open BBox

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//


/// Model to generate one symbol (skeleton). Id is a unique Id 
/// for the symbol shared with Issie Component type.
/// The real type will obviously be much larger.
/// Complex information that never changes (other than Id) should 
/// probably not be here, but looked up via some function
/// from a more compact form, so that comparison of two Symbols to
/// determine are they the same is fast.
type Symbol =
    {
        Pos: XYPos
        LastDragPos : XYPos
        IsSelected : bool
        IsDragging : bool
        Id : CommonTypes.ComponentId
    }


type Model = Symbol list

//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    /// coords not adjusted for top-level zoom
    | DeselectAll
    | Select of sId: CommonTypes.ComponentId
    | SelectOnly of sId: CommonTypes.ComponentId
    | ToggleSelect of sId: CommonTypes.ComponentId
    | StartDragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    | Dragging of pagePos: XYPos
    | EndDragging
    | AddCircle of XYPos // used by demo code to add a circle
    | DeleteSymbol of sId:CommonTypes.ComponentId 
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface


//---------------------------------helper types and functions----------------//



let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}


//-----------------------------Skeleton Model Type for symbols----------------//

//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)

let BBoxFromSymbol (sym: Symbol) =
    let pos = posDiff sym.Pos (posOf 20. 20.)
    BBox.toBBox pos.X pos.Y 40. 40.

let getTargetedSymbol (symModel: Model) (p: XYPos) : Symbol option =
    symModel
    |> List.filter (fun sym -> BBoxFromSymbol sym |> containsPoint p)
    |> List.tryHead

let getSymbolsInBBox (symModel: Model) (bb: BBox) : CommonTypes.ComponentId list =
    symModel
    |> List.filter (fun sym -> BBoxFromSymbol sym |> overlaps bb)
    |> List.map (fun sym -> sym.Id)


//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (pos:XYPos) =
    {
        Pos = pos
        LastDragPos = {X=0. ; Y=0.} // initial value can always be this
        IsSelected = false
        IsDragging = false
        Id = CommonTypes.ComponentId (Helpers.uuid()) // create a unique id for this symbol
    }


/// Dummy function for test. The real init would probably have no symbols.
let init () =
    List.allPairs [1..14] [1..14]
    |> List.map ((fun (x,y) -> {X = float (x*64+30); Y=float (y*64+30)}) >> createNewSymbol)
    , Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddCircle pos -> 
        createNewSymbol pos :: model, Cmd.none
    | DeleteSymbol sId -> 
        List.filter (fun sym -> sym.Id <> sId) model, Cmd.none
    | DeselectAll ->
        model
        |> List.map (fun sym ->
            { sym with IsSelected=false }
        ), Cmd.none
    | Select sId ->
        model
        |> List.map (fun sym ->
            if sId = sym.Id then
                {sym with IsSelected = true}
            else
                sym
        ), Cmd.none
    | SelectOnly sId ->
        model
        |> List.map (fun sym ->
            if sId = sym.Id then
                {sym with IsSelected = true}
            else
                {sym with IsSelected = false}
        ), Cmd.none
    | ToggleSelect sId ->
        model
        |> List.map (fun sym ->
            if sId = sym.Id then
                {sym with IsSelected = (not sym.IsSelected)}
            else
                sym
        ), Cmd.none
    | Dragging pagePos ->
        model
        |> List.map (fun sym ->
            if sym.IsSelected then
                if sym.IsDragging then
                    let diff = posDiff pagePos sym.LastDragPos
                    { sym with
                        Pos = posAdd sym.Pos diff
                        LastDragPos = pagePos
                    }
                else
                    { sym with
                        LastDragPos = pagePos
                        IsDragging = true
                    }
            else
                sym
        )
        , Cmd.none

    | EndDragging ->
        model
        |> List.map (fun sym ->
            { sym with IsDragging = false }
        )
        , Cmd.none
    | MouseMsg mT -> 
        match (mT.Op, mT.Pos) with
        | (Down, p) ->
            model
            |> List.map (fun sym ->
                let bb = BBoxFromSymbol sym
                if containsPoint p bb then
                    { sym with IsSelected = true }
                else
                    sym
            ), Cmd.none
        | _ -> model, Cmd.none
    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderCircleProps =
    {
        Circle : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

/// View for one symbol with caching for efficient execution when input does not change
let private renderCircle =
    FunctionComponent.Of(
        fun (props : RenderCircleProps) ->
            let color =
                if props.Circle.IsSelected then
                    "lightblue"
                else
                    "grey"

            let bb = BBoxFromSymbol props.Circle

            polygon [ // Bus decoder square
                    SVGAttr.Points <| sprintf "%f,%f %f,%f %f,%f %f,%f" bb.Pos.X bb.Pos.Y bb.Pos.X (bb.Pos.Y + bb.Bounds.Height) (bb.Pos.X + bb.Bounds.Width) (bb.Pos.Y + bb.Bounds.Height) (bb.Pos.X + bb.Bounds.Width) bb.Pos.Y
                    SVGAttr.StrokeWidth "1px"
                    SVGAttr.Stroke "Red"
                    SVGAttr.Fill color] []
    , "Circle"
    , equalsButFunctions
    )

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = CommonTypes.ComponentId id} as circle) ->
        renderCircle 
            {
                Circle = circle
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList


/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"

/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth 
        (wId: CommonTypes.ConnectionId) 
        (outputPortNumber: int) 
        (inputPortWidths: int option list) : int option =
    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"
