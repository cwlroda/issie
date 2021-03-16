﻿module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes


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
        LastDragPos : XYPos
        IsDragging : bool
        Id : ComponentId
        Component : Component
        Selected : bool
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
    // | StartDraggingDummy of sId : ComponentId * pagePos: XYPos
    // | DraggingDummy of sId : ComponentId * pagePos: XYPos
    | StartDragging of sId : ComponentId list * pagePos: XYPos
    | Dragging of sIdLst: ComponentId list * pagePos: XYPos
    // | EndDraggingDummy of sId : ComponentId
    | EndDragging
    // | AddSymbol of comp:Component*pos:XYPos // used by demo code to add a circle
    //| DeleteSymbol of sId:ComponentId 
    | AddSymbol of sType: ComponentType * pos: XYPos
    | DeleteSymbols of sIdLst: ComponentId list
    | UpdateSymbolModelWithComponent of Component // Issie interface
    // | SetSelectedDummy of topLeft:XYPos * bottomRight:XYPos // 
    | SetSelected of sIdLst:ComponentId list
    // | MouseOverPort of port : Port // Used for Dummy Code
    // | MouseOutPort of port : Port // Used for Dummy Code
    | HighlightPorts of pId : PortId list
    | UnhighlightPorts


//---------------------------------helper types and functions----------------//


let posDiff (a:XYPos) (b:XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd  (a:XYPos) (b:XYPos) =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let withinSelectedBoundary (compTopLeft:XYPos) (compBotRight:XYPos) (boundTopLeft:XYPos) (boundBotRight:XYPos) :bool =
    match compTopLeft,compBotRight with
        | point1,point2 when (point1.X >= boundTopLeft.X) && (point2.X <= boundBotRight.X) && (point1.Y >= boundTopLeft.Y) && (point2.Y <= boundBotRight.Y) -> true
        | _ -> false

let combinedPortsList (sym:Symbol) : Port list =
    let filledPortList (portList:Port list) : Port list = 
        List.filter (fun (port:Port) -> 
            port.PortNumber  <>  None
        ) portList
    filledPortList sym.Component.InputPorts @ filledPortList sym.Component.OutputPorts

let getPortsFromSymbols (symModel:Model) (sIdLst:ComponentId list) : PortId list =
    let sIdSet = Set.ofList sIdLst

    symModel
    |> List.find (fun sym -> Set.contains sym.Id sIdSet)
    |> combinedPortsList
    |> List.map (fun port -> port.PortId)

let getAllSymbols (symModel:Model) : ComponentId list =
    symModel
    |> List.map (fun sym->sym.Id)
    

let allPortsInModel (symModel:Model) : Port list = 
    symModel
    |>List.fold (
        fun acc elem ->  
            List.append acc (combinedPortsList elem)
    ) ([]:Port list) 

let findSymbolFromPort (symModel: Model) (port:Port) : Symbol =
    symModel
    |> List.find (fun sym -> sym.Component.Id = port.HostId) 

let getTargetedSymbol (symModel: Model) (pos:XYPos) : ComponentId Option = 
    let foundSymbol = 
        symModel
        |> List.tryFind 
            (fun (sym:Symbol)-> 
                (sym.Component.X <= pos.X)
                && (sym.Component.X+sym.Component.W>=pos.X)
                && (sym.Component.Y <= pos.Y)
                && (sym.Component.Y + sym.Component.H >= pos.Y)
            ) 
    match foundSymbol with
        | Some sym -> (Some sym.Id)
        | None -> None

let getSymbolsInTargetArea (symModel:Model) (bbox:BBox) : ComponentId List =
    symModel
    |> List.filter
        (fun (sym:Symbol) ->
            let symBBox = pointsToBBox (posOf sym.Component.X sym.Component.Y) (posOf (sym.Component.X+sym.Component.W) (sym.Component.Y+sym.Component.H))
            overlaps symBBox bbox
        )
    |> List.map
        (fun (sym:Symbol) -> sym.Id)

let findPort (symModel: Model) (portId: PortId) : Port =
        allPortsInModel symModel
        |> List.find(
            fun (port:Port) -> port.PortId = portId
        )

let portPos (symModel: Model) (portId: PortId) : XYPos = 

    let foundPort = findPort symModel portId
    let foundSymbol = findSymbolFromPort symModel foundPort
    {
        X = foundPort.PortPos.X + foundSymbol.Component.X
        Y = foundPort.PortPos.Y + foundSymbol.Component.Y
    }

let getTargetedPort (symModel:Model) (pos:XYPos) : PortId Option =

    let nearbyPorts = 
        allPortsInModel symModel
        |>List.filter
            (fun (port:Port) ->
                portPos symModel port.PortId
                |> posDist pos < 20.
            )
        |>List.sortBy
            (fun (port:Port) ->
                portPos symModel port.PortId
                |> posDist pos
            )
    match nearbyPorts with
    | nearestPort::_ -> Some nearestPort.PortId
    | [] -> None
    
    

let symbolPos (symModel: Model) (sId: ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> {X=sym.Component.X;Y=sym.Component.Y})






let portType (symModel: Model) (portId: PortId) : PortType = 
    let foundPort = findPort symModel portId
    foundPort.PortType

let portWidth (symModel: Model) (portId: PortId) : int option = 
    let foundPort = findPort symModel portId
    match foundPort.Width with
    | PortWidth x -> Some x


let getSymbolFromSymbolId (symModel:Model) (symId:ComponentId) : Symbol = 
    symModel
    |> List.find(
        fun sym -> sym.Id = symId
    )

let getHostId (model:Model) (portId:PortId) : ComponentId = 
    (findPort model portId).HostId

let symbolType (model:Model) (compId:ComponentId) : ComponentType = 
    (model
    |>List.find (fun sym -> sym.Id = compId)
    ).Component.Type


let symbolBBox (model:Model) (compId:ComponentId) : BBox =
    let foundSymbol = 
        model
        |>List.find (fun sym -> sym.Id = compId)

    {
        Pos = {X=foundSymbol.Component.X; Y=foundSymbol.Component.Y}
        Width = foundSymbol.Component.W
        Height = foundSymbol.Component.H
    }

let portsInRange (model:Model) (mousePos:XYPos) (range:float) : PortId list =
    
    
    let nearbyPorts = 
        allPortsInModel model
        |>List.filter
            (fun (port:Port) ->
                portPos model port.PortId
                |> posDist mousePos < range
            )
        |>List.sortBy
            (fun (port:Port) ->
                portPos model port.PortId
                |> posDist mousePos
            )
    List.map( fun port -> port.PortId) nearbyPorts
    // model
    // |> allPortsInModel
    // |> List.filter( fun x -> ((posDist x.PortPos mousePos) < range))
    // |> List.map(fun x -> x.PortId)
    // // model
    // // |> List.fold (fun (acc:PortId list) (elem:Symbol) ->
    // //     combinedPortsList elem
    // //     |> List.map
    // //         (fun port -> port.PortId)
    // //     |> List.append acc
    // // ) []

let mulOfFive (input:float)  : float = 
    10. * float (int (input / 10.))
    
//-----------------------------Skeleton Model Type for symbols----------------//

//------------------------------Create Symbols---------------------//
let rng = System.Random 0
let rng2() = rng.Next(0,2)
let createSpecificComponent (hostID: ComponentId) (position:XYPos) (compType:ComponentType) (labelName:string) : Component =

    let compX,compY,compW,compH =
        match compType with 
        | Not | And | Or | Xor | Nand | Nor | Xnor -> mulOfFive position.X, mulOfFive position.Y,mulOfFive 60.,mulOfFive 105.
        | DFF | DFFE -> mulOfFive position.X, mulOfFive position.Y,mulOfFive 100.,mulOfFive 105.
        | Mux2 | Demux2 | DFF | DFFE -> mulOfFive position.X, mulOfFive position.Y,mulOfFive 100.,mulOfFive 120.
        | NbitsAdder _ -> mulOfFive position.X, mulOfFive position.Y,mulOfFive 150.,mulOfFive 130.
        | Input _ | Output _ | Constant _->  mulOfFive position.X, mulOfFive position.Y,mulOfFive 100.,mulOfFive 30.
        | RAM _ | RegisterE _-> mulOfFive position.X, mulOfFive position.Y,mulOfFive 200.,mulOfFive 150.
        | ROM _ | Register _ -> mulOfFive position.X, mulOfFive position.Y,mulOfFive 200.,mulOfFive 90.
        | AsyncROM _ -> mulOfFive position.X, mulOfFive position.Y,mulOfFive 200.,mulOfFive 110.
        | Decode4 -> mulOfFive position.X, mulOfFive position.Y, mulOfFive 100.,mulOfFive 150.
        | IOLabel -> mulOfFive position.X, mulOfFive position.Y, mulOfFive 100.,mulOfFive 30.
        | MergeWires | SplitWire _ -> mulOfFive position.X, mulOfFive position.Y, mulOfFive 100.,mulOfFive 100.
        | BusSelection _ -> mulOfFive position.X, mulOfFive position.Y, mulOfFive 200., mulOfFive 90.
        |_ ->  mulOfFive position.X, mulOfFive position.Y,mulOfFive 60.,mulOfFive 100.


    let portTemplate (portNumber:int) (portType: PortType) (portPos:float) (portWidth:PortWidth) :Port=
        let offset = 
            match portType with 
            |PortType.Input -> 0.
            |PortType.Output -> compW        
        {
            PortId = PortId (uuid())
            PortNumber =  Some (PortNumber (portNumber))
            PortType = portType
            PortPos = {X=offset; Y = mulOfFive (20. + ((float portNumber) + 1.) * portPos ) }
            HostId = hostID
            Hover = PortHover false
            Width = portWidth
        }

    
    let (inputPorts, outputPorts): (Port list * Port list) =
        match compType with 
        | IOLabel ->
            (
                let inputPortList = 
                    [{
                        PortId = PortId (uuid())
                        PortNumber = Some (PortNumber (0))
                        PortType = PortType.Input
                        PortPos = {X=0.; Y = mulOfFive (compH/2.)}
                        HostId = hostID
                        Hover = PortHover false
                        Width = PortWidth 0
                    }]
                let outputPortList = 
                    [{
                        PortId = PortId (uuid())
                        PortNumber = Some (PortNumber (0))
                        PortType = PortType.Output
                        PortPos = {X=compW; Y = mulOfFive (compH/2.)}
                        HostId = hostID
                        Hover = PortHover false
                        Width = PortWidth 0
                    }]
                inputPortList,outputPortList
            )
        | Input n ->
            (
                let inputPortList = 
                    [{
                        PortId = PortId (uuid())
                        PortNumber =  None
                        PortType = PortType.Output
                        PortPos = {X = mulOfFive (compW/2.); Y = compH}
                        HostId = hostID
                        Hover = PortHover false
                        Width = PortWidth 0
                    }]
                let outputPortList = 
                    [
                        portTemplate 0 PortType.Output (compH/2.) (PortWidth n)
                    ]
                inputPortList,outputPortList
            )
        |Output n ->
            (
                let outputPortList = 
                    [{
                        PortId = PortId (uuid())
                        PortNumber =  None
                        PortType = PortType.Input
                        PortPos = {X= mulOfFive (compW/2.); Y = compH}
                        HostId = hostID
                        Hover = PortHover false
                        Width = PortWidth 0
                    }]
                let inputPortList = 
                    [
                        portTemplate 0 PortType.Input (compH/2.) (PortWidth n)
                    ]
                inputPortList,outputPortList
            )
        | And | Or | Xor | Nand | Nor | Xnor ->
            (
                let inputPortList = 
                    [0;1]
                    |>List.map (fun x -> 
                        portTemplate x PortType.Input (( compH - 20. )/3.) (PortWidth 1)
                    )
                let outputPortList = 
                    [
                        portTemplate 0 PortType.Output (( compH - 20. )/2.) (PortWidth 1)
                    ]
                inputPortList,outputPortList
            )
        | Not ->
            (
                let inputPortList = 
                    [
                        portTemplate 0 PortType.Input (( compH - 10. )/2.) (PortWidth 1)
                    ]
                let outputPortList = 
                    [
                        portTemplate 0 PortType.Output (( compH - 10. )/2.) (PortWidth 1)
                    ]
                inputPortList,outputPortList
            )
        | Mux2 -> 
            (
                let inputPortList =
                    [0;1]
                    |>List.map (fun x -> 
                        portTemplate x PortType.Input (( compH - 20. )/4.) (PortWidth 1)
                    )
                    |> List.append [(portTemplate 2 PortType.Input (( compH - 20. )/4.) (PortWidth 1))]
                let outputPortList =
                    [
                        portTemplate 0 PortType.Output ((compH - 20.)/2. ) (PortWidth 1)
                    ]
                inputPortList, outputPortList
            )

        | Demux2 ->
            (
                let inputPortList = 
                    [
                        portTemplate 0 PortType.Input ((compH - 20.)/3. ) (PortWidth 1)
                    ]
                    |> List.append [(portTemplate 1 PortType.Input (( compH - 20. )/3.) (PortWidth 1))]
                let outputPortList = 
                    [0;1]
                    |>List.map (fun x -> 
                        portTemplate x PortType.Output (( compH - 20. )/3.) (PortWidth 1)
                    )
                inputPortList, outputPortList
            )

        | NbitsAdder n->
            (
                let inputPortList = 
                    [1;2]
                    |>List.map (fun x ->
                        portTemplate x PortType.Input (( compH - 20. )/4.) (PortWidth n)
                    )
                    |> List.append ([portTemplate 0 PortType.Input (( compH - 20. )/4.) (PortWidth 1)])
                let outputPortList = 
                    [portTemplate 0 PortType.Output (( compH - 20. )/3.) (PortWidth n)]
                    |>List.append [portTemplate 1 PortType.Output (( compH - 20. )/3.) (PortWidth 1)]
                inputPortList, outputPortList
            )
        | DFF ->
            (
                let inputPortList = 
                    [portTemplate 0 PortType.Input (( compH - 20. )/2.) (PortWidth 1)]
                let outputPortList = 
                    [portTemplate 0 PortType.Output ((compH - 20.) / 2.) (PortWidth 1)]
                inputPortList, outputPortList
            )
        | DFFE -> 
            (
                let inputPortList = 
                    [portTemplate 0 PortType.Input (( compH - 20. )/3.) (PortWidth 1)]
                    |> List.append [portTemplate 1 PortType.Input (( compH - 20. )/3.) (PortWidth 1)]
                let outputPortList = 
                    [portTemplate 0 PortType.Output ((compH - 20.) / 2.) (PortWidth 1)]
                inputPortList, outputPortList
            )
        | Register n->
            (
                let inputPortList = 
                    [portTemplate 0 PortType.Input (( compH - 20. )/2.) (PortWidth n)]
                let outputPortList = 
                    [portTemplate 0 PortType.Output ((compH - 20.) / 2.) (PortWidth n)]
                inputPortList, outputPortList
            )
        | RegisterE n ->
            (
                let inputPortList = 
                    [portTemplate 0 PortType.Input (( compH - 20. )/3.) (PortWidth n)]
                    |> List.append ([portTemplate 1 PortType.Input (( compH - 20. )/3.) (PortWidth 1)])
                let outputPortList = 
                    [portTemplate 0 PortType.Output ((compH - 20.) / 2.) (PortWidth n)]
                inputPortList, outputPortList
            )
        | AsyncROM mem | ROM mem ->
            (
                let inputPortList = 
                    [portTemplate 0 PortType.Input (( compH - 20. )/2.) (PortWidth mem.AddressWidth)]
                let outputPortList = 
                    [portTemplate 0 PortType.Output ((compH - 20.) / 2.) (PortWidth mem.WordWidth)]
                inputPortList, outputPortList
            )
        | RAM mem ->
            (
                let inputPortList = 
                    [portTemplate 0 PortType.Input (( compH - 20. )/4.) (PortWidth mem.AddressWidth)]
                    @ [portTemplate 1 PortType.Input (( compH - 20. )/4.) (PortWidth mem.WordWidth)]
                    @ [portTemplate 2 PortType.Input (( compH - 20. )/4.) (PortWidth 1)]
                let outputPortList = 
                    [portTemplate 0 PortType.Output ((compH - 20.) / 2.) (PortWidth mem.WordWidth)]
                inputPortList, outputPortList
            )
        | Constant (width,_) ->
            (
                let inputPortList = 
                    [{
                        PortId = PortId (uuid())
                        PortNumber =  None
                        PortType = PortType.Output
                        PortPos = {X=mulOfFive (compW/2.); Y = compH}
                        HostId = hostID
                        Hover = PortHover false
                        Width = PortWidth 0
                    }]
                let outputPortList = 
                    [portTemplate 0 PortType.Output ((compH - 20.) / 2.) (PortWidth width)]
                inputPortList, outputPortList
            )
        | BusSelection (outputWidth,leastSB) ->
            (
                let inputPortList = 
                    [portTemplate 0 PortType.Input (( compH - 20. )/2.) (PortWidth (leastSB+outputWidth))]
                let outputPortList = 
                    [portTemplate 0 PortType.Output ((compH - 20.) / 2.) (PortWidth outputWidth)]
                inputPortList, outputPortList
            )
        | MergeWires ->
            (
                let inputPortList = 
                    [portTemplate 0 PortType.Input (( compH - 20. )/3.) (PortWidth 0)]
                    |>List.append [portTemplate 1 PortType.Input (( compH - 20. )/3.) (PortWidth 0)]
                let outputPortList = 
                    [
                        portTemplate 0 PortType.Output (( compH - 20. )/2.) (PortWidth 1)
                    ]
                inputPortList,outputPortList
            )
        | SplitWire n->
            (
                let outputPortList = 
                    [portTemplate 0 PortType.Output (( compH - 20. )/3.) (PortWidth 0)]
                    |>List.append [portTemplate 1 PortType.Output (( compH - 20. )/3.) (PortWidth n)]
                    
                let inputPortList = 
                    [
                        portTemplate 0 PortType.Input (( compH - 20. )/2.) (PortWidth 0)
                    ]
                inputPortList,outputPortList
            )
        | Decode4 ->
            (
                let inputPortList = 
                    [portTemplate 0 PortType.Input (( compH - 20. )/3.) (PortWidth 2)]
                    |> List.append ([portTemplate 1 PortType.Input (( compH - 20. )/3.) (PortWidth 1)])
                let outputPortList = 
                    [0..3]
                    |>List.map (fun x -> 
                        portTemplate x PortType.Output (( compH - 20. )/5.) (PortWidth 1)
                    )
                inputPortList, outputPortList
            )
        | _ -> [],[]
    {
        Id = hostID
        Type = compType
        Label = labelName
        InputPorts = inputPorts
        OutputPorts = outputPorts
        X = compX
        Y = compY
        H = compH
        W = compW
    }



        


//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.

let createNewSymbol ()  =
    let rng0 () = rng.Next (0,9)
    let rngComponent () = rng.Next(0,25)
    let memory () = {AddressWidth = rng0(); WordWidth = rng0(); Data=Map.empty} 
    let compType = 
        match (rngComponent ()) with
        | 0 -> Not
        | 1 -> And
        | 2 -> Or
        | 3 -> Xor
        | 4 -> Nand
        | 5 -> Nor
        | 6 -> Xnor
        | 7 -> Mux2
        | 8 -> NbitsAdder (rng0 ())
        | 9 -> DFF
        | 10 -> DFFE
        | 11 -> Register (rng0 ())
        | 12 -> RegisterE (rng0())
        | 13 -> AsyncROM (memory ())
        | 14 -> ROM (memory())
        | 15 -> RAM (memory())
        | 16 -> Decode4
        | 17 -> Input (rng0 ())
        | 18 -> Output (rng0 ())
        | 19 -> IOLabel
        | 20 -> Demux2
        | 21 -> MergeWires
        | 22 -> BusSelection (rng0(),rng0())
        | 23 -> Constant (rng0(), rng0())
        | _ -> SplitWire (rng0())

        //| 3 -> testComponentDemux2 ()
        //| _ -> testComponentMux2 ()
    let rng1 () = rng.Next(0,800)
    let compId = ComponentId (Helpers.uuid())
    let comp = 
        createSpecificComponent compId ({X= float(rng1 ());Y = float (rng1 ()) }) compType (string (rng.Next (0,100)))
    
    {
        LastDragPos = {X=0. ; Y=0.}
        IsDragging = false
        Id = compId
        Component = comp
        Selected =
            match rng.Next(0,2) with
            | 0-> false
            | 1 -> true
            | _ -> true
    }
    

/// Dummy function for test. The real init would probably have no symbols.
let init () =
    
    // let createTestList (testComponent :Component) (xIn,yIn) =
    
    //     {testComponent with X = testComponent.X + xIn; Y=testComponent.Y+yIn}

    // List.allPairs [100.;200.;300.] [40.; 150.;300.]
    // |> List.map (createTestList testComponent)
    // |> List.map (createNewSymbol (rng.Next(2)))
    //Factorised to the below expression
    [1..10]
    |> List.map (fun x -> createNewSymbol ()) 
    // ([]:Symbol list)
    , Cmd.none

let setSelectedFunction (topLeft:XYPos, topRight:XYPos) (model:Model) : Model =
    model
    |> List.map (fun sym ->
        if (withinSelectedBoundary {X=sym.Component.X; Y=sym.Component.Y} {X=sym.Component.X + sym.Component.W; Y=sym.Component.Y + sym.Component.H} topLeft topRight) then
            {sym with
                Selected = true  
            }
        else
            {sym with
                Selected = false
            }
    )

let updateSymbolModelWithComponent (symModel: Model) (comp:Component) :Model =
    symModel
    |> List.map (
        fun sym -> 
            match sym.Id with
            |x when x = comp.Id-> 
                {sym with
                    Component = comp
                }
            |_ -> sym
        )


/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    // | AddSymbol (comp,pos)-> 
    | AddSymbol (sType, pos) -> 
        let compId = ComponentId (Helpers.uuid())
        let comp =
            createSpecificComponent compId pos sType (string (rng.Next (0,100)))
        let sym = 
            {
                LastDragPos = {X=0. ; Y=0.}
                IsDragging = false
                Id = compId
                Component = comp
                Selected = false
            }
        
        sym:: model, Cmd.none
    // | DeleteSymbol sId -> 
    //     List.filter (fun sym -> sym.Id <> sId) model, Cmd.none
    | DeleteSymbols _ -> 
        List.filter (fun sym -> not sym.Selected) model , Cmd.none
    // | SetSelectedDummy (topLeft, topRight) ->
    //     (setSelectedFunction (topLeft, topRight) model), Cmd.none

    | UpdateSymbolModelWithComponent comp ->
        updateSymbolModelWithComponent model comp, Cmd.none

    // | StartDraggingDummy (sId, pagePos) ->
    //     let sIdSymbol:Symbol = 
    //         (List.filter (fun x -> x.Id = sId) model)
    //         |>List.head
    //     let startDrag =
    //         List.map(fun sym ->
    //                 if sym.Selected then
    //                     {sym with 
    //                         LastDragPos = pagePos
    //                         IsDragging = true
    //                     }
    //                 else sym
    //             )
    //     (match sIdSymbol with
    //     |x when not x.Selected ->  
    //             setSelectedFunction ({X=x.Component.X; Y=x.Component.Y}, {X=x.Component.X + x.Component.W; Y = x.Component.Y + x.Component.H}) model
    //                 |> startDrag
    //     |_-> 
    //         model
    //             |> startDrag

    //     ), Cmd.none

    
    |SetSelected (sIdLst) ->
        model
        |> List.map (fun sym -> 
            if List.tryFind (fun sId -> sId = sym.Id) sIdLst <> None then
                {sym with
                    Selected = true
                }
            else 
                {sym with
                    Selected = false
                }
        ) , Cmd.none


    | StartDragging (sIdLst, pagePos) ->
        (
            sIdLst
            |> List.map (getSymbolFromSymbolId model)
            |> List.fold (fun acc elem ->
                acc
                |> List.map (fun sym ->
                    if sym.Id = elem.Id then
                        {sym with
                            LastDragPos = pagePos
                            IsDragging = true
                        }
                    else sym
                )
            ) model
        ), Cmd.none

    | Dragging (sIdLst, pagePos) ->
        model
        |> List.map(fun sym ->
            if List.tryFind (fun sId -> sId = sym.Id) sIdLst <> None then
                let diff = posDiff pagePos sym.LastDragPos
                {sym with
                    Component = {sym.Component with X = sym.Component.X + diff.X; Y = sym.Component.Y + diff.Y}
                    LastDragPos = pagePos
                }
                else
                    sym
            ), Cmd.none
    
    | EndDragging ->
        model
        |> List.map (fun sym ->
            if sym.IsDragging then
                {sym with
                    IsDragging = false
                    Component=
                        {sym.Component with 
                            X = mulOfFive sym.Component.X
                            Y = mulOfFive sym.Component.Y
                        }
                }
            else sym
        ), Cmd.none

    | HighlightPorts pIdLst ->
        model
        |> List.map(fun sym ->
            {sym with
                Component = 
                {sym.Component with
                    InputPorts = 
                        sym.Component.InputPorts
                        |> List.map (fun port ->
                            if List.tryFind (fun pId -> pId = port.PortId) pIdLst <> None then
                                {port with   
                                    Hover = PortHover true
                                }
                            else
                                {port with
                                    Hover = PortHover false
                                }
                        )
                    OutputPorts = 
                        sym.Component.OutputPorts
                        |> List.map (fun port ->
                            if List.tryFind (fun pId -> pId = port.PortId) pIdLst <> None then
                                {port with   
                                    Hover = PortHover true
                                }
                            else
                                {port with
                                    Hover = PortHover false
                                }
                        )
                }   
            }
        ), Cmd.none
         
    | UnhighlightPorts ->
        model
        |> List.map (
            (fun sym ->
                {sym with
                    Component = 
                        {sym.Component with
                            InputPorts =
                                sym.Component.InputPorts
                                |> List.map (fun checkPort ->
                                {checkPort with   
                                        Hover = PortHover false
                                    }
                                )
                        }
                }
            ) >> 
            (fun sym ->
                {sym with
                    Component = 
                        {sym.Component with
                            InputPorts =
                                sym.Component.OutputPorts
                                |> List.map (fun checkPort ->
                                {checkPort with   
                                        Hover = PortHover false
                                    }
                                )
                        }
                }
            )
        )
        ,Cmd.none
    
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags



//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderSymbolProps =
    {
        Symbol : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        Key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

/// View for one symbol with caching for efficient execution when input does not change
let private renderSymbol (model:Model) =
    
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->

            let fillColor =
                if props.Symbol.Selected then
                //if props.Symbol.IsDragging then
                    "dodgerblue"
                else
                    "cyan"
                    
            let outlineColor = 
                "black"

            let width = props.Symbol.Component.W
            let height = props.Symbol.Component.H
            let topLeft:XYPos = {X=props.Symbol.Component.X;Y=props.Symbol.Component.Y}
            let topRight:XYPos = {X=props.Symbol.Component.X+width; Y=props.Symbol.Component.Y}
            let bottomRight:XYPos = {X=props.Symbol.Component.X+width; Y=props.Symbol.Component.Y+height}
            let bottomLeft:XYPos = {X=props.Symbol.Component.X; Y=props.Symbol.Component.Y+height}
            
               
            let inputPorts = props.Symbol.Component.InputPorts
            let outputPorts = props.Symbol.Component.OutputPorts
            
            let componentType = props.Symbol.Component.Type

            let componentName =
                match props.Symbol.Component.Type with
                | Mux2 -> "MUX"
                | Demux2 -> "DEMUX"
                | NbitsAdder n ->   (string n) + "-Bit Adder"
                | DFF -> "DFF"
                | DFFE -> "DFF"
                | Register n | RegisterE n -> "REG" + (string n)
                | AsyncROM _ -> "Async-ROM"
                | ROM _ -> "ROM"
                | RAM _ -> "RAM"
                | And | Nand  -> "&"
                | Not-> "1"
                | Or | Nor-> "≥"
                | Xor | Xnor -> "=1"
                | Decode4 -> "Decode"
                | IOLabel -> "Label"
                | MergeWires -> "Merge"
                | SplitWire n -> "Split-" + (string n)
                | BusSelection (m,n) -> "Bus Select [" + (string (m+n-1)) + ".." + (string n) + "]"
                | Input _ -> "Input"
                | Output _ -> "Output"
                | Constant (_,n) -> "Constant -"  + string n
                |_ -> "NameNotNeeded"

            
            
            //----------------------------Static Components----------------------------//
            let viewBoxStaticComponent : IProp seq =
                seq {
                        Style [
                                TextAnchor "left" 
                                FontSize "14px"
                                UserSelect UserSelectOptions.None
                            ]
                    }
            let viewBoxInternalStaticLabelStyle: IProp seq = 
                seq {Style [
                            UserSelect UserSelectOptions.None
                            TextAnchor "middle"
                            FontSize "20px"
                        ]
                }
            let viewBoxLabel : ReactElement =
                text 
                        (Seq.append [
                            X topLeft.X
                            Y (topLeft.Y - 20.)
                        ] viewBoxStaticComponent) [str <| componentName + " - " + (string props.Symbol.Component.Label)]

            let viewBoxClock (bottomLeft:XYPos): ReactElement =
                g [] 
                    [
                        polygon 
                            (Seq.append [
                                SVGAttr.Points (sprintf "%f %f, %f %f, %f %f" bottomLeft.X (bottomLeft.Y-5.) (bottomLeft.X+10.) (bottomLeft.Y-10.) (bottomLeft.X) (bottomLeft.Y-15.))
                                SVGAttr.Fill "black"
                                SVGAttr.Stroke "black"
                                SVGAttr.StrokeWidth 2
                            ] viewBoxStaticComponent) []
                    
                        text 
                            (Seq.append [
                                X (bottomLeft.X + 12.)
                                Y (bottomLeft.Y - 17.)
                            ] viewBoxStaticComponent ) 
                         [str <| "CLK"]
                    ]
                

            let viewPortsStaticComponent (portType:PortType): IProp seq = 
                seq {
                    Style [
                        match portType with
                        |PortType.Input -> TextAnchor "left" // left/right/middle: horizontal algnment vs (X,Y)
                        |PortType.Output -> TextAnchor "end"
                        DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                        FontSize "18px"
                        FontWeight "Bold"
                        Fill "black" // demo font color
                        UserSelect UserSelectOptions.None
                    ]
                }
            let viewPortLinesStaticComponent (x:Port) : IProp seq = 
                seq {
                    match x.Hover with
                    |PortHover false ->
                        SVGAttr.Fill fillColor
                        SVGAttr.Stroke outlineColor
                        SVGAttr.StrokeWidth 6
                    |_ -> 
                        SVGAttr.Fill "red"
                        SVGAttr.Stroke "red"
                        SVGAttr.StrokeWidth 8
                }
            let viewPortBusIndicatorLinesStaticComponent (x:Port) : IProp seq =
                seq {
                match x.Hover with
                |PortHover false ->
                    SVGAttr.Fill fillColor
                    SVGAttr.Stroke outlineColor
                    SVGAttr.StrokeWidth 3
                |_ -> 
                    SVGAttr.Fill "red"
                    SVGAttr.Stroke "red"
                    SVGAttr.StrokeWidth 4
            }

            let viewPortBusIndicatorTextStaticComponent (x:Port) : IProp seq =
                seq {
                Style [
                        
                        TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                        DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                        FontSize "12px"
                        FontWeight "Bold"
                        Fill "Blue" // demo font color
                        UserSelect UserSelectOptions.None
                    ]
            }
            

            

            //----------------------------viewBox Functions---------------------------//
            let viewBoxOutput =
                [
                    polygon
                        (Seq.append [
                            SVGAttr.Points (sprintf "%f %f, %f %f, %f %f , %f %f, %f %f" topLeft.X  (0.5*(topLeft.Y+bottomLeft.Y)) (topLeft.X+10.) topLeft.Y topRight.X topRight.Y bottomRight.X bottomRight.Y (bottomLeft.X+10.) bottomLeft.Y )
                            SVGAttr.Fill fillColor
                            SVGAttr.Stroke outlineColor
                            SVGAttr.StrokeWidth 2
                        ] (Seq.append viewBoxStaticComponent viewBoxInternalStaticLabelStyle))  []
                    viewBoxLabel
                    text 
                        (Seq.append [
                            X (topLeft.X + 0.5*(topRight.X - topLeft.X)); 
                            Y (topLeft.Y + 6.); 
                        ] (Seq.append viewBoxStaticComponent viewBoxInternalStaticLabelStyle)) [str <| componentName];
                    viewBoxLabel
                ]

            let viewBoxInput =
                [  
                    polygon
                        (Seq.append [
                            SVGAttr.Points (sprintf "%f %f, %f %f, %f %f , %f %f, %f %f" topLeft.X  topLeft.Y (topRight.X-10.) topRight.Y topRight.X (0.5*(topRight.Y+bottomRight.Y)) (bottomRight.X-10.) bottomRight.Y bottomLeft.X bottomLeft.Y)
                            SVGAttr.Fill fillColor
                            SVGAttr.Stroke outlineColor
                            SVGAttr.StrokeWidth 2
                        ] (Seq.append viewBoxStaticComponent viewBoxInternalStaticLabelStyle))  []
                    viewBoxLabel
                    text 
                        (Seq.append [
                            X (topLeft.X + 0.5*(topRight.X - topLeft.X)); 
                            Y (topLeft.Y + 6.); 
                        ] (Seq.append viewBoxStaticComponent viewBoxInternalStaticLabelStyle)) [str <| componentName];
                    viewBoxLabel
                ]
                

            let viewBoxConstant n =
                [
                    polygon
                        (Seq.append [
                            SVGAttr.Points (sprintf "%f %f, %f %f, %f %f , %f %f, %f %f" topLeft.X  topLeft.Y (topRight.X-10.) topRight.Y topRight.X (0.5*(topRight.Y+bottomRight.Y)) (bottomRight.X-10.) bottomRight.Y bottomLeft.X bottomLeft.Y)
                            SVGAttr.Fill fillColor
                            SVGAttr.Stroke outlineColor
                            SVGAttr.StrokeWidth 2
                        ] (Seq.append viewBoxStaticComponent viewBoxInternalStaticLabelStyle))  []
                    viewBoxLabel
                    
                    text 
                        (Seq.append [
                            X (topLeft.X + 0.5*(topRight.X - topLeft.X)); 
                            Y (topLeft.Y + 6.); 
                        ] (Seq.append viewBoxStaticComponent viewBoxInternalStaticLabelStyle)) [str <| string n];
                    viewBoxLabel
                ]

            let viewBoxType1 =
                [
                    rect
                        (
                            Seq.append [
                            X topLeft.X 
                            Y topLeft.Y
                            SVGAttr.Rx 5.
                            SVGAttr.Ry 5.
                            SVGAttr.Width width
                            SVGAttr.Height height
                            SVGAttr.Fill fillColor
                            SVGAttr.Stroke outlineColor
                            SVGAttr.StrokeWidth 2
                            
                        ] viewBoxStaticComponent) []
                    viewBoxLabel   
                        
                    text 
                        (Seq.append [
                            X (topLeft.X + 0.5*(topRight.X - topLeft.X)); 
                            Y (topLeft.Y + 6.); 
                        ] (Seq.append viewBoxStaticComponent viewBoxInternalStaticLabelStyle)) [str <| componentName];
                    match componentType with
                    | ROM _ | RAM _ | Register _ | RegisterE _ | DFF | DFFE -> viewBoxClock bottomLeft
                    | _ -> nothing
                ]

            let lineLength = 24.


            let viewPortLinesType1 (compType:ComponentType)=
                let generateLines (portList:Port list) : ReactElement list =
                    List.map(fun (x:Port) ->
                    // let parentSymbol () = findSymbolFromPort model x
                    let absPos () = posAdd  topLeft x.PortPos
                    match compType with
                    | Not | And |Nand |Or|Nor|Xor|Xnor|Mux2|Demux2
                    |NbitsAdder _|DFF|DFFE|RegisterE _|Register _|AsyncROM _
                    |ROM _|RAM _|Decode4|IOLabel|MergeWires|SplitWire _
                    |BusSelection _->

                        let dynamicContent =
                            match x.PortType with
                            | PortType.Input -> ((absPos()).X-lineLength), ((absPos()).X - 18.)
                            | PortType.Output -> ((absPos()).X+lineLength), ((absPos()).X + 18.)
                        let (PortNumber portNumber) =    
                            match x.PortNumber with
                            |Some a -> a
                            |None -> PortNumber -1
                        match portNumber with
                        |portCheck when portCheck >= 0 ->
                            g[] [
                                line 
                                    (Seq.append [
                                        X1 (absPos()).X
                                        X2 (fst dynamicContent)
                                        Y2 (absPos()).Y
                                        Y1 (absPos()).Y
                                    ] (viewPortLinesStaticComponent x))[]

                                line 
                                    (Seq.append [
                                        X1 (snd dynamicContent)
                                        X2 (snd dynamicContent)
                                        match x.Hover with
                                        |PortHover false ->
                                            Y2 ((absPos()).Y + 6.)
                                            Y1 ((absPos()).Y - 6.)
                                        |_ ->
                                            Y2 ((absPos()).Y + 8.)
                                            Y1 ((absPos()).Y - 8.)
                                    ] (viewPortBusIndicatorLinesStaticComponent x))[]
                                text (Seq.append [
                                    X (snd dynamicContent)
                                    match x.Hover with
                                    |PortHover false ->
                                        Y ((absPos()).Y - 20.)
                                    |_ ->
                                        Y ((absPos()).Y - 22.)
                                    ] (viewPortBusIndicatorTextStaticComponent x)) [str <| string (x.Width)]
                                match compType with 
                                | ComponentType.Not | ComponentType.Nand | ComponentType.Nor | ComponentType.Xnor ->
                                    (
                                        match x.PortType with
                                        |PortType.Output -> 
                                            line
                                                (Seq.append [
                                                    X1 ((absPos()).X+12.)
                                                    X2 (absPos()).X
                                                    match x.Hover with
                                                    |PortHover false ->
                                                        Y2 ((absPos()).Y - 12.)
                                                        Y1 (absPos()).Y
                                                    |_ ->
                                                        Y2 ((absPos()).Y - 14.)
                                                        Y1 ((absPos()).Y)
                                                ] (viewPortBusIndicatorLinesStaticComponent x))[]
                                        |_ -> nothing
                                    )
                                | _ -> nothing
                            ]
                        |_ -> nothing
                    
                    |_ -> nothing
                    ) portList
                        
                (generateLines inputPorts) @ (generateLines outputPorts)


            //----------------------------viewPorts Functions---------------------------//
            let viewPortsInput : ReactElement list  = []
            let viewPortsOutput : ReactElement list = []                     

            
                
            let generatePorts (port:Port) (portLabel:string) : ReactElement =
                text (
                    Seq.append [
                        match port.PortType with
                        |PortType.Input -> 
                            X (topLeft.X + port.PortPos.X + 6.)
                            Y (topLeft.Y + port.PortPos.Y - 10.)
                        |PortType.Output ->
                            X (topLeft.X + port.PortPos.X - 6.)
                            Y (topLeft.Y + port.PortPos.Y - 10.)
                    ] (viewPortsStaticComponent port.PortType)
                ) [str<|portLabel]

            let viewPortsType1 (compType:ComponentType): ReactElement list = 
                let inputList =
                    List.map (fun port->
                        let portNumber = 
                            match port.PortNumber with 
                            | Some (PortNumber portNum) -> portNum
                            | None -> -1
                        match compType with
                        |Decode4 ->
                            (
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Sel"
                                |portNum when portNum = 1 -> generatePorts port "Data"
                                |_ -> nothing
                            )
                        |Mux2 ->
                            (
                                match portNumber with
                                |portNum when portNum < 2 -> generatePorts port (string portNumber)
                                |portNum when portNum = 2 -> generatePorts port "Sel"
                                |_ -> nothing
                            )
                        |Demux2 ->
                            (
                                match portNumber with
                                |portNum when portNum < 1 -> generatePorts port (string portNumber)
                                |portNum when portNum = 1 -> generatePorts port "Sel"
                                |_ -> nothing
                            )
                        |NbitsAdder _ ->
                            (
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Cin"
                                |portNum when portNum = 1 -> generatePorts port "A"
                                |portNum when portNum = 2 -> generatePorts port "B"
                                |_ -> nothing
                            )
                        |DFF ->
                            (
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "D"
                                |_ -> nothing
                            )
                        |DFFE ->
                            (
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "D"
                                |portNum when portNum = 1 -> generatePorts port "EN"
                                |_ -> nothing
                            )
                        |Register _ ->
                            (
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Data-In"
                                |_ -> nothing
                            )
                        |RegisterE _ ->
                            (
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Data-In"
                                |portNum when portNum = 1 -> generatePorts port "EN"
                                |_ -> nothing
                            )
                        |ROM _ | AsyncROM _ ->
                            (
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Addr"
                                |_ -> nothing
                            )
                        |RAM _ ->
                            (
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Addr"
                                |portNum when portNum = 1 -> generatePorts port "Data-In"
                                |portNum when portNum = 2 -> generatePorts port "Write"
                                |_ -> nothing
                            )
                        |_ -> nothing
                            
                    ) inputPorts
                let outputList =
                    List.map (fun port->
                        let portNumber = 
                            match port.PortNumber with 
                            | Some (PortNumber portNum) -> portNum
                            | None -> -1
                        match compType with
                        |Demux2|Decode4 -> generatePorts port (string portNumber)
                        |DFF|DFFE -> generatePorts port "Q"
                        |Register _|RegisterE _ -> generatePorts port "Data-Out" 
                        |AsyncROM _|ROM _ -> generatePorts port "Data"
                        |RAM _ -> generatePorts port "Data-Out"
                        |NbitsAdder _ ->
                            match portNumber with
                            |portNum when portNum = 0 -> generatePorts port "Sum"
                            |portNum when portNum = 1 -> generatePorts port "Cout"
                            |_ -> nothing
                        |_-> nothing
                   
                    ) outputPorts
                inputList @ outputList

            //----------------------------viewPortLines Functions---------------------------//
            
            let viewPortLinesOutput =
                let (PortWidth wid) = (List.head inputPorts).Width
                [g [] 
                    [
                        line (Seq.append [
                            X1 topLeft.X
                            X2 (topLeft.X - lineLength)
                            Y1 (0.5*(topLeft.Y+bottomLeft.Y))
                            Y2 (0.5*(topLeft.Y+bottomLeft.Y))
                        ] (viewPortLinesStaticComponent (List.head inputPorts)))[]

                        line (Seq.append [
                            X1 (topLeft.X - 5.)
                            X2 (topLeft.X - 7.)
                            match (List.head inputPorts).Hover with
                            |PortHover false ->
                                Y2 (0.5*(topRight.Y+bottomRight.Y) + 6.)
                                Y1 (0.5*(topRight.Y+bottomRight.Y) - 6.)
                            |_ ->
                                Y2 (0.5*(topRight.Y+bottomRight.Y) + 8.)
                                Y1 (0.5*(topRight.Y+bottomRight.Y) - 8.)
                        ] (viewPortBusIndicatorLinesStaticComponent (List.head inputPorts)))[]

                        
                        text (Seq.append [
                            X (topLeft.X - 5.)
                            match (List.head inputPorts).Hover with
                            |PortHover false ->
                                Y (0.5*(topRight.Y+bottomRight.Y) - 20.)
                            |_ ->
                                Y (0.5*(topRight.Y+bottomRight.Y) - 22.)
                        ] (viewPortBusIndicatorTextStaticComponent (List.head inputPorts))) [str <| string wid]
                    ]
                ]
            let viewPortLinesInput = 
                let (PortWidth wid) = (List.head outputPorts).Width
                [g [] 
                    [
                        line (Seq.append [
                            X1 topRight.X
                            X2 (topRight.X + lineLength)
                            Y1 (0.5*(topRight.Y+bottomRight.Y))
                            Y2 (0.5*(topRight.Y+bottomRight.Y))
                        ] (viewPortLinesStaticComponent (List.head outputPorts))) []

                        line (Seq.append [
                            X1 (topRight.X + 5.)
                            X2 (topRight.X + 7.)
                            match (List.head outputPorts).Hover with
                            |PortHover false ->
                                Y2 (0.5*(topRight.Y+bottomRight.Y) + 6.)
                                Y1 (0.5*(topRight.Y+bottomRight.Y) - 6.)
                            |_ ->
                                Y2 (0.5*(topRight.Y+bottomRight.Y) + 8.)
                                Y1 (0.5*(topRight.Y+bottomRight.Y) - 8.)
                        ] (viewPortBusIndicatorLinesStaticComponent (List.head outputPorts)))[]

                        text (Seq.append [
                            X (topRight.X + 5.)
                            match (List.head outputPorts).Hover with
                            |PortHover false ->
                                Y (0.5*(topRight.Y+bottomRight.Y) - 20.)
                            |_ ->
                                Y (0.5*(topRight.Y+bottomRight.Y) - 22.)
                        ] (viewPortBusIndicatorTextStaticComponent (List.head outputPorts))) [str <| string wid]
                    ]
                ]
                

            //----------------------------Combine All View Functions---------------------------//
            let viewOverall (compType : ComponentType) = 
                match compType with 
                | ComponentType.Input _ -> viewBoxInput @ viewPortsInput @ viewPortLinesInput
                | ComponentType.Output _ -> viewBoxOutput @ viewPortsOutput @ viewPortLinesOutput
                | ComponentType.Constant (_,n) -> (viewBoxConstant n) @ viewPortsInput @ viewPortLinesInput
                | ComponentType.Not | ComponentType.And | ComponentType.Or | ComponentType.Xor | ComponentType.Nand | ComponentType.Nor | ComponentType.Xnor |ComponentType.Mux2 | ComponentType.Demux2 |ComponentType.NbitsAdder _ | ComponentType.DFF | ComponentType.DFFE | ComponentType.Register _ | ComponentType.RegisterE _ | ComponentType.AsyncROM _| ComponentType.ROM _| ComponentType.RAM _| ComponentType.Decode4 | ComponentType.IOLabel | ComponentType.MergeWires | ComponentType.SplitWire _ | ComponentType.BusSelection _
                    -> viewBoxType1 @ (viewPortLinesType1 compType) @ (viewPortsType1 compType)
                |_ -> [nothing]


            g [Style [
                            // TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                            DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                            FontSize "10px"
                            FontWeight "Bold"
                            Fill "Black" // demo font color
                        ]] (viewOverall componentType)

    , "Component"
    , equalsButFunctions
    )

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = ComponentId id} as symbol) ->
        renderSymbol model
            {
                Symbol = symbol
                Dispatch = dispatch
                Key = id
            }
    )
    |> ofList


//---------------Other interface functions--------------------//


/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth (wId: ConnectionId) (outputPortNumber: int) (inputPortWidths: int option list) : int option =
    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:ComponentId) : Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : Component list = 
    failwithf "Not implemented"
