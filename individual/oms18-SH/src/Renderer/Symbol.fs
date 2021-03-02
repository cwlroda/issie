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
        InputPort: CommonTypes.PortId
        OutputPort: CommonTypes.PortId
        HighlightInputPort: bool
        HighlightOutputPort: bool
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
    | AddSymbol of CommonTypes.ComponentType * XYPos
    | Select of CommonTypes.ComponentId list
    | StartDragging of CommonTypes.ComponentId list * XYPos
    /// coords not adjusted for top-level zoom
    | Dragging of CommonTypes.ComponentId list * XYPos
    | EndDragging
    | DeleteSymbols of sId:CommonTypes.ComponentId list
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface
    | HighlightPorts of CommonTypes.PortId list


//---------------------------------helper types and functions----------------//

//-----------------------------Skeleton Model Type for symbols----------------//

//---------------Other interface functions--------------------//

let symbolPos (model: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) model
    |> (fun sym -> sym.Pos)

let BBoxFromSymbol (sym: Symbol) =
    let pos = posDiff sym.Pos (posOf 20. 20.)
    BBox.toBBox pos.X pos.Y 40. 40.

let getPortBBoxes (sym: Symbol) : BBox * BBox =
    let bbSym = BBoxFromSymbol sym
    let x = bbSym.Pos.X
    let y = bbSym.Pos.Y
    let w = bbSym.Width
    let h = bbSym.Height
    let size = 10.
    (toBBox x y size size), (toBBox (x + w - size) (y + h - size) size size)

let portPos (model: Model) (pId: CommonTypes.PortId) : XYPos =
    List.find (fun sym -> sym.InputPort = pId || sym.OutputPort = pId) model
    |> (fun sym ->
        let bb = BBoxFromSymbol sym
        if sym.InputPort = pId then
            posOf (bb.Pos.X + 5.) (bb.Pos.Y + 5.)
        else
            posOf (bb.Pos.X + bb.Width - 5.) (bb.Pos.Y + bb.Height - 5.)
    )

let getAllSymbols (model: Model) : CommonTypes.ComponentId list =
    model
    |> List.map (fun sym -> sym.Id)

let getPortsFromSymbols (model: Model) (symbols: CommonTypes.ComponentId list) : CommonTypes.PortId list =
    let sIdSet = Set.ofList symbols

    model
    |> List.collect (fun sym ->
        if Set.contains sym.Id sIdSet then
            [sym.InputPort;sym.OutputPort]
        else
            []
    )

let getAllPorts (model: Model) : CommonTypes.PortId list =
    model
    |> List.collect (fun sym -> [sym.InputPort;sym.OutputPort])

let portsInRange (model: Model) (p: XYPos) (range : float) : CommonTypes.PortId list =
    model
    |> List.collect (fun sym ->
        let (inputBB, outputBB) = getPortBBoxes sym
        [
            (sym.InputPort, posAdd inputBB.Pos (posOf 5. 5.))
            (sym.OutputPort, posAdd outputBB.Pos (posOf 5. 5.))
        ])
    |> List.filter (fun (_, pPos) ->
        let diff = posDiff p pPos
        diff.X * diff.X + diff.Y * diff.Y <= range * range
    )
    |> List.map fst

let getTargetedSymbol (model: Model) (p: XYPos) : CommonTypes.ComponentId option =
    model
    |> List.filter (fun sym -> BBoxFromSymbol sym |> containsPoint p)
    |> List.map (fun sym -> sym.Id)
    |> List.tryHead

let getSymbolsInBBox (model: Model) (bb: BBox) : CommonTypes.ComponentId list =
    model
    |> List.filter (fun sym -> BBoxFromSymbol sym |> overlaps bb)
    |> List.map (fun sym -> sym.Id)

let getTargetedPort (model: Model) (p: XYPos) : CommonTypes.PortId option =
    model
    |> List.collect (fun sym ->
        let (inputBB, outputBB) = getPortBBoxes sym
        [
            (sym.InputPort, inputBB)
            (sym.OutputPort, outputBB)
        ]
    )
    |> List.filter (fun (_, bb) -> containsPoint p bb)
    |> List.map fst
    |> List.tryHead


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
        InputPort = CommonTypes.PortId (Helpers.uuid())
        OutputPort = CommonTypes.PortId (Helpers.uuid())
        HighlightInputPort = false
        HighlightOutputPort = false
    }


/// Dummy function for test. The real init would probably have no symbols.
let init () =
    List.allPairs [1..14] [1..14]
    |> List.map ((fun (x,y) -> {X = float (x*60+30); Y=float (y*60+30)}) >> createNewSymbol)
    , Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (_, pos) -> 
        createNewSymbol pos :: model, Cmd.none
    | DeleteSymbols sIdLst -> 
        let sIdSet = Set.ofList sIdLst
        List.filter (fun sym -> not <| Set.contains sym.Id sIdSet) model, Cmd.none
    | Select sIdLst ->
        let sIdSet = Set.ofList sIdLst

        model
        |> List.map (fun sym ->
            if Set.contains sym.Id sIdSet then
                {sym with IsSelected = true}
            else
                {sym with IsSelected = false}
        ), Cmd.none
    | StartDragging (sIdLst, pos) ->
        let sIdSet = Set.ofList sIdLst

        model
        |> List.map (fun sym ->
            if Set.contains sym.Id sIdSet then
                { sym with
                    LastDragPos = pos
                }
            else
                sym
        )
        , Cmd.none
    | Dragging (sIdLst, pos) ->
        let sIdSet = Set.ofList sIdLst

        model
        |> List.map (fun sym ->
            if Set.contains sym.Id sIdSet then
                let diff = posDiff pos sym.LastDragPos
                { sym with
                    Pos = posAdd sym.Pos diff
                    LastDragPos = pos
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
    | HighlightPorts pIdList ->
        let portIdSet = Set.ofList pIdList
            
        model
        |> List.map (fun sym ->
            let highlightInputPort = Set.contains sym.InputPort portIdSet
            let highlightOutputPort = Set.contains sym.OutputPort portIdSet
            { sym with
                HighlightInputPort=highlightInputPort
                HighlightOutputPort=highlightOutputPort
            }
        ), Cmd.none
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
            let (inputBB, outputBB) = getPortBBoxes props.Circle
            let inputPortColor =
                if props.Circle.HighlightInputPort then
                    "Green"
                else
                    "grey"
            let outputPortColor =
                if props.Circle.HighlightOutputPort then
                    "Green"
                else
                    "grey"

            g [] [
                rect [ // Bus decoder square
                        X bb.Pos.X
                        Y bb.Pos.Y
                        SVGAttr.Width bb.Width
                        SVGAttr.Height bb.Height
                        SVGAttr.StrokeWidth "2px"
                        SVGAttr.Stroke "grey"
                        SVGAttr.Fill color] []
                rect [ // Bus decoder square
                        X inputBB.Pos.X
                        Y inputBB.Pos.Y
                        SVGAttr.Width inputBB.Width
                        SVGAttr.Height inputBB.Height
                        SVGAttr.StrokeWidth "1px"
                        SVGAttr.Stroke "Green"
                        SVGAttr.Fill inputPortColor] []
                rect [ // Bus decoder square
                        X outputBB.Pos.X
                        Y outputBB.Pos.Y
                        SVGAttr.Width outputBB.Width
                        SVGAttr.Height outputBB.Height
                        SVGAttr.StrokeWidth "1px"
                        SVGAttr.Stroke "Green"
                        SVGAttr.Fill outputPortColor] []
            ]
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
    |> List.rev
    |> ofList


