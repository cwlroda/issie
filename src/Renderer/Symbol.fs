module Symbol
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


type Model = Map<ComponentId, Symbol>

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
    | AddSymbol of sType: ComponentType * pos: XYPos * label: string
    | DeleteSymbols of sIdLst: ComponentId list
    | UpdateSymbolModelWithComponent of Component // Issie interface
    // | SetSelectedDummy of topLeft:XYPos * bottomRight:XYPos // 
    | SetSelected of sIdLst:ComponentId list
    // | MouseOverPort of port : Port // Used for Dummy Code
    // | MouseOutPort of port : Port // Used for Dummy Code
    | HighlightPorts of pId : PortId list
    | UnhighlightPorts
    | WidthInferrer of (PortId*PortId)
    | DeleteInference of (PortId*PortId)


//---------------------------------helper types and functions----------------//


let processingString (inputString) (thresholdLength) = 
    let beforeProcessingLength = String.length inputString
    if beforeProcessingLength > thresholdLength then
            let intermediateStep = 
                (
                    String.mapi (fun ind chr -> 
                        if ind < thresholdLength then chr else '\000'
                    ) 
                    >> String.filter (fun x -> x <> '\000') 
                ) inputString
            intermediateStep + ".."
        else inputString

let posDiff (a:XYPos) (b:XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd  (a:XYPos) (b:XYPos) =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let withinSelectedBoundary (compTopLeft:XYPos) (compBotRight:XYPos) (boundTopLeft:XYPos) (boundBotRight:XYPos) :bool =
    match compTopLeft,compBotRight with
        | point1,point2 when (point1.X >= boundTopLeft.X) && (point2.X <= boundBotRight.X) && (point1.Y >= boundTopLeft.Y) && (point2.Y <= boundBotRight.Y) -> true
        | _ -> false



let combinedPortsMap (sym: Symbol) : Map<PortId, Port> =
    let filledPortList (portMap: Map<PortId, Port>) : Map<PortId, Port> = 
        portMap
        |> Map.filter (fun _ port -> port.PortNumber <> None)

    Map.fold (fun acc k v -> Map.add k v acc) (filledPortList sym.Component.InputPorts) (filledPortList sym.Component.OutputPorts)

let findPortFromNumAndType (sym: Symbol) (portNum: PortNumber) (portType: PortType) =
    combinedPortsMap sym
    |> Map.toList
    |> List.find ( fun (_,port) -> (port.PortNumber = Some portNum && port.PortType= portType))
    |> snd


let getPortsFromSymbols (symModel: Model) (sIdLst: ComponentId list) : PortId list =
    let symList =
        symModel
        |> Map.filter (fun symId _ -> List.contains symId sIdLst)
        |> Map.toList
        |> List.map snd

    symList
    |> List.fold (fun acc sym ->
        acc @ (
            combinedPortsMap sym
            |> Map.toList
            |> List.map fst
        )
    ) []

let getAllSymbols (symModel: Model) : ComponentId list =
    symModel
    |> Map.toList
    |> List.map fst

let allPortsInModel (symModel: Model) : Map<PortId, Port> = 
    symModel
    |> Map.fold (fun acc _ elem -> 
        Map.fold (fun acc k v ->
            Map.add k v acc
        ) acc (combinedPortsMap elem)
    ) Map.empty

let findSymbolFromPort (symModel: Model) (port: Port) : Symbol =
    symModel.[port.HostId]

let getTargetedSymbol (symModel: Model) (pos:XYPos) : ComponentId Option = 
    let foundSymId = 
        symModel
        |> Map.tryFindKey 
            (fun _ sym -> 
                (sym.Component.X <= pos.X)
                && (sym.Component.X + sym.Component.W >= pos.X)
                && (sym.Component.Y <= pos.Y)
                && (sym.Component.Y + sym.Component.H >= pos.Y)
            )
    match foundSymId with
        | Some symId -> Some symId
        | None -> None

let getSymbolsInTargetArea (symModel:Model) (bbox:BBox) : ComponentId List =
    symModel
    |> Map.filter
        (fun _ (sym:Symbol) ->
            let symBBox = pointsToBBox (posOf sym.Component.X sym.Component.Y) (posOf (sym.Component.X+sym.Component.W) (sym.Component.Y+sym.Component.H))
            overlaps symBBox bbox
        )
    |> Map.toList
    |> List.map fst

let findPort (symModel: Model) (portId: PortId) : Port =
    (allPortsInModel symModel).[portId]

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
        |> Map.filter
            (fun _ port ->
                portPos symModel port.PortId
                |> posDist pos < 20.
            )
        |> Map.toList
        |> List.map snd
        |> List.sortBy
            (fun port ->
                portPos symModel port.PortId
                |> posDist pos
            )

    match nearbyPorts with
    | nearestPort::_ -> Some nearestPort.PortId
    | [] -> None

let symbolPos (symModel: Model) (sId: ComponentId) : XYPos = 
    Map.find sId symModel
    |> (fun sym -> {X=sym.Component.X;Y=sym.Component.Y})

let portType (symModel: Model) (portId: PortId) : PortType = 
    let foundPort = findPort symModel portId
    foundPort.PortType

let portWidth (symModel: Model) (portId: PortId) : int option = 
    let foundPort = findPort symModel portId

    match foundPort.Width with
    | PortWidth x -> Some x

let getSymbolFromSymbolId (symModel: Model) (symId: ComponentId) : Symbol = 
    Map.find symId symModel

let getHostId (symModel: Model) (portId: PortId) : ComponentId = 
    (findPort symModel portId).HostId

let symbolType (symModel: Model) (compId: ComponentId) : ComponentType = 
    (Map.find compId symModel).Component.Type

let symbolLabel (symModel: Model) (compId: ComponentId) : string = 
    (Map.find compId symModel).Component.Label

let symbolBBox (symModel: Model) (compId: ComponentId) : BBox =
    let foundSymbol = 
        Map.find compId symModel

    {
        Pos = {X=foundSymbol.Component.X; Y=foundSymbol.Component.Y}
        Width = foundSymbol.Component.W
        Height = foundSymbol.Component.H
    }

let subtractPortWidth (pw1:PortWidth) (pw2:PortWidth) :PortWidth = 
    let (PortWidth w1) = pw1
    let (PortWidth w2) = pw2
    PortWidth (w1-w2)

let addPortWidth (pw1:PortWidth) (pw2:PortWidth): PortWidth =
    let (PortWidth w1) = pw1
    let (PortWidth w2) = pw2
    PortWidth (w1+w2)

let portsInRange (model: Model) (mousePos: XYPos) (range: float) : PortId list =
    let nearbyPorts = 
        allPortsInModel model
        |> Map.filter
            (fun _ port ->
                portPos model port.PortId
                |> posDist mousePos < range
            )
        |> Map.toList
        |> List.map snd
        |> List.sortBy
            (fun port ->
                portPos model port.PortId
                |> posDist mousePos
            )

    List.map(fun port -> port.PortId) nearbyPorts


let mulOfFive (input:float)  : float = 
    10. * float (int (input / 10.))
    
//-----------------------------Skeleton Model Type for symbols----------------//

//------------------------------Create Symbols---------------------//
let rng = System.Random 0
let rng2() = rng.Next(0,2)
let createSpecificComponent (hostID: ComponentId) (position:XYPos) (compType:ComponentType) (labelName:string) : Component =
    let compX = position.X
    let compY = position.Y
    let compW,compH =
        match compType with 
        | Not | And | Or | Xor | Nand | Nor | Xnor -> 60., 70.
        | DFF -> 100., 80.                              | DFFE -> 100., 110.
        | Mux2 -> 100., 140.                            | Demux2 -> 100., 110. 
        | RAM _ -> 200., 140.                           | RegisterE _-> 200., 110.
        | NbitsAdder _ -> 150., 140.                    | Decode4 -> 100., 170.
        | Input _ | Output _ | Constant _-> 100., 40.   | IOLabel -> 100., 40.
        | ROM _ | Register _ -> 200., 90.               | AsyncROM _ -> 200., 80.
        | MergeWires | SplitWire _ -> 100., 110.        | BusSelection _ -> 200., 70.
        | Custom customParams ->  
            let maxPorts =
                if (List.length customParams.InputLabels) > (List.length customParams.OutputLabels) then
                    (List.length customParams.InputLabels)
                else
                    (List.length customParams.OutputLabels)
               
            200.,  (20. + 30.* (1. + float (maxPorts)))
            

    let portTemplate (portExist:bool) (portNumber:int) (portType: PortType) (portWidth:PortWidth) (considerTitle:bool) (totalPorts:int):Port=
        match portExist with
        | true ->
            let yPosCalc= 
                match considerTitle with
                | true -> (20. + float (portNumber + 1) * (compH - 20.) / float (totalPorts + 1))
                | false -> (compH /2.)
            let offset = 
                match portType with 
                |PortType.Input -> 0.
                |PortType.Output -> compW        
            {
                PortId = PortId (uuid())
                PortNumber =  Some (PortNumber (portNumber))
                PortType = portType
                PortPos = {X=offset; Y = mulOfFive yPosCalc }
                HostId = hostID
                Hover = PortHover false
                Width = portWidth
            }
        |false ->
            {
                PortId = PortId (uuid())
                PortNumber =  None
                PortType = portType
                PortPos = {X=0.; Y=0. }//(20. + ((float portNumber) + 1.) * portPos ) }
                HostId = hostID
                Hover = PortHover false
                Width = portWidth
            }

    let (inputPorts, outputPorts): (Map<PortId, Port> * Map<PortId, Port>) =
        match compType with
        | IOLabel ->
            let inputPortMap = 
                [portTemplate (true) (0) (PortType.Input) (PortWidth 0) (false) (1)]  
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth 0) (false) (1)]  
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList
            
            inputPortMap, outputPortMap
        | Input n ->
            let inputPortMap = 
                [portTemplate (false) (0) (PortType.Input) (PortWidth 0) (false) (0)] 
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
            //portTemplate (portExist:bool) (portNumber:int) (portType: PortType) (portWidth:PortWidth) (considerTitle:bool) (totalPorts:int)
                [portTemplate (true) (0) (PortType.Output) (PortWidth n) (false) (1)] 
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap
        |Output n ->
            let outputPortMap = 
            //portTemplate (portExist:bool) (portNumber:int) (portType: PortType) (portWidth:PortWidth) (considerTitle:bool) (totalPorts:int)
                [portTemplate (false) (0) (PortType.Output) (PortWidth 0) (false) (0)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let inputPortMap = 
            //portTemplate (portExist:bool) (portNumber:int) (portType: PortType) (portWidth:PortWidth) (considerTitle:bool) (totalPorts:int)
                [portTemplate (true) (0) (PortType.Input) (PortWidth n) (false) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap
        | Custom customParams ->
            let inputPortMap = 
                if List.isEmpty customParams.InputLabels then
                    [portTemplate (false) (0) (PortType.Input) (PortWidth 0) (false) (0)]  
                    |> List.map (fun port -> (port.PortId, port))
                    |> Map.ofList
                else
                    customParams.InputLabels
                    |> List.mapi (fun i (_,portNum) ->
                        portTemplate (true) (i) (PortType.Input) (PortWidth portNum) (true) (List.length customParams.InputLabels) 
                    )
                    |> List.map (fun port -> (port.PortId, port))
                    |> Map.ofList
            
            let outputPortMap = 
                if List.isEmpty customParams.OutputLabels then
                    [portTemplate (false) (0) (PortType.Output) (PortWidth 0) (false) (0)] 
                    |> List.map (fun port -> (port.PortId, port))
                    |> Map.ofList
                else
                    customParams.OutputLabels
                    |> List.mapi (fun i (_,portNum) ->
                        portTemplate (true) (i) (PortType.Output) (PortWidth portNum) (true) (List.length customParams.OutputLabels) 
                        //portTemplate portNum PortType.Output (( compH - 20. ) / ((float (List.length customParams.OutputLabels)) + 1.)) (PortWidth 1) 
                    )
                    |> List.map (fun port -> (port.PortId, port))
                    |> Map.ofList

            inputPortMap,outputPortMap

        | And | Or | Xor ->
            let inputPortMap = 
                [0;1]
                |> List.map (fun portNumber ->
                    portTemplate (true) (portNumber) (PortType.Input) (PortWidth 1) (true) (2) 
                )
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth 1) (true) (1)] 
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap,outputPortMap

        | Nand | Nor | Xnor ->
            let inputPortMap = 
                [0;1]
                |> List.map (fun portNumber -> 
                    portTemplate (true) (portNumber) (PortType.Input) (PortWidth 1) (true) (2) 
                    // portTemplate portNumber PortType.Input (( compH - 20. )/3.) (PortWidth 1)
                )
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                let temp = portTemplate (true) (0) (PortType.Output) (PortWidth 1) (true) (1) 
                [{temp with 
                    PortPos =
                        {temp.PortPos with
                            X = compW+15.
                        }
                }]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList
            inputPortMap, outputPortMap

        | Not ->
            let inputPortMap = 
                [portTemplate (true) (0) (PortType.Input) (PortWidth 1) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                let temp = portTemplate (true) (0) (PortType.Output) (PortWidth 1) (true) (1) 
                [{temp with 
                    PortPos =
                        {temp.PortPos with
                            X = compW+15.
                        }
                }]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap,outputPortMap

        | Mux2 ->
            let inputPortMap =
                [0;1;2]
                |> List.map (fun portNumber -> 
                    portTemplate (true) (portNumber) (PortType.Input) (PortWidth 1) (true) (3)
                )
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap =
                [portTemplate (true) (0) (PortType.Output) (PortWidth 1) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

        | Demux2 ->
            let inputPortMap = 
                [0;1]
                |> List.map (fun portNumber -> 
                    portTemplate (true) (portNumber) (PortType.Input) (PortWidth 1) (true) (2)
                )
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [0;1]
                |>List.map (fun portNumber -> 
                    portTemplate (true) (portNumber) (PortType.Output) (PortWidth 1) (true) (2)
                )
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

        | NbitsAdder n->
            let inputPortMap = 
                [1;2]
                |>List.map (fun portNumber ->
                    portTemplate (true) (portNumber) (PortType.Input) (PortWidth n) (true) (3)
                )
                |> List.append ([portTemplate (true) (0) (PortType.Input) (PortWidth 1) (true) (3)])
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth n) (true) (2)]
                |>List.append [portTemplate (true) (1) (PortType.Output) (PortWidth 1) (true) (2)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

        | DFF ->
            let inputPortMap = 
                [portTemplate (true) (0) (PortType.Input) (PortWidth 1) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth 1) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

        | DFFE ->
            let inputPortMap = 
                [0;1]
                |>List.map (fun portNumber ->
                    portTemplate (true) (portNumber) (PortType.Input) (PortWidth 1) (true) (2)
                )
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth 1) (true) (2)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

        | Register n->
            let inputPortMap = 
                [portTemplate (true) (0) (PortType.Input) (PortWidth n) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth n) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

        | RegisterE n ->
            let inputPortMap = 
                [portTemplate (true) (0) (PortType.Input) (PortWidth n) (true) (2)]
                |> List.append ([portTemplate (true) (1) (PortType.Input) (PortWidth 1) (true) (2)])
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth n) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

        | AsyncROM mem | ROM mem ->
            let inputPortMap = 
                [portTemplate (true) (0) (PortType.Input) (PortWidth mem.AddressWidth) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth mem.WordWidth) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

        | RAM mem ->
            let inputPortMap = 
                [portTemplate (true) (0) (PortType.Input) (PortWidth mem.AddressWidth) (true) (3)]
                @ [portTemplate (true) (1) (PortType.Input) (PortWidth mem.WordWidth) (true) (3)]
                @ [portTemplate (true) (2) (PortType.Input) (PortWidth 1) (true) (3)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap =
                [portTemplate (true) (0) (PortType.Output) (PortWidth mem.WordWidth) (true) (1)] 
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

        | Constant (width,_) ->
            let inputPortMap =
                [portTemplate (false) (0) (PortType.Input) (PortWidth 0) (false) (0)] 
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth width) (false) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

        | BusSelection (outputWidth,leastSB) ->
            let inputPortMap = 
                [portTemplate (true) (0) (PortType.Input) (PortWidth (leastSB+outputWidth)) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth outputWidth) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

        | MergeWires ->
            let inputPortMap = 
                [0;1]
                |>List.map (fun portNumber ->
                    portTemplate (true) (portNumber) (PortType.Input) (PortWidth 0) (true) (2)
                )
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth 0) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap,outputPortMap

        | SplitWire n->
            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth 0) (true) (2)]
                |>List.append [portTemplate (true) (1) (PortType.Output) (PortWidth n) (true) (2)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList
                
            let inputPortMap = 
                [portTemplate (true) (0) (PortType.Input) (PortWidth 0) (true) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap,outputPortMap

        | Decode4 ->
            let inputPortMap = 
                [portTemplate (true) (0) (PortType.Input) (PortWidth 2) (true) (2)]
                |> List.append ([portTemplate (true) (1) (PortType.Input) (PortWidth 1) (true) (2)])
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [0..3]
                |>List.map (fun portNumber -> 
                    portTemplate (true) (portNumber) (PortType.Output) (PortWidth 1) (true) (4)
                    // portTemplate x PortType.Output (( compH - 20. )/5.) (PortWidth 1)
                )
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

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

let createNewSymbol ()  =
    let rng0 () = rng.Next (1,10)
    let rngComponent () = rng.Next(0,26)
    let memory () = {AddressWidth = rng0(); WordWidth = rng0(); Data=Map.empty}

    let randomName () = 
        let nameRng1 () = rng.Next (1,21)
        let nameRng2 () = rng.Next (1,6)
        let nameRng3 () = rng.Next(1,27)
        let firstLetter () = 
            match nameRng1 () with
            | 1 -> "B" | 2 -> "C" | 3 -> "D" | 4 -> "F" | 5 -> "G"
            | 6 -> "H" | 7 -> "J" | 8 -> "K" | 9 -> "L" | 10 -> "M"
            | 11 -> "N" | 12 -> "P" | 13 -> "Q" | 14 -> "R" | 15 -> "S"
            | 16 -> "T" | 17 -> "V" | 18 -> "W" | 19 -> "Y" | _ -> "Z"
        let secondLetter () = 
            match nameRng2 () with
            | 1 -> "a" | 2 -> "e" | 3 -> "i" | 4 -> "o" | _ -> "u"
        let thirdLetter () = 
            match nameRng3 () with
            | 1 -> "a" | 2 -> "b" | 3 -> "c" | 4 -> "d" | 5 -> "e"
            | 6 -> "f" | 7 -> "g" | 8 -> "h" | 9 -> "i" | 10 -> "j"
            | 11 -> "k" | 12 -> "l" | 13 -> "m" | 14 -> "n" | 15 -> "o"
            | 16 -> "p" | 17 -> "q" | 18 -> "r" | 19 -> "s" | 20 -> "t"
            | 21 -> "u" | 22 -> "v" | 23 -> "w" | 24 -> "x" | 25 -> "y" | _ -> "z"
        firstLetter() + secondLetter() + thirdLetter()
        

    let compType = 
        let (customComp:CustomComponentType) = 
            let labels (inputOutput:bool) = 
                let inOrOut = 
                    match inputOutput with
                    |true -> "TestInput"
                    |false -> "TestOutput"

                [0..rng.Next(1,5)]
                |> List.map (fun i -> ((string i + inOrOut), rng.Next(0,10)))
            {
                Name = "\"Our\" Custom Component"
                InputLabels = labels true
                OutputLabels = labels false
            }
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
        | 23 -> 
            let cons = rng0()
            let wid = int ((log(float cons)/log(2.))+1.)
            Constant (wid, cons)
        | 24 -> SplitWire (rng0())
        | _ -> Custom customComp
        
    let rng1 () = rng.Next(0,800)
    let compId = ComponentId (Helpers.uuid())
    let comp = 
        createSpecificComponent compId ({X= float(rng1 ());Y = float (rng1 ()) }) compType ((randomName ()) + (string(rng.Next (0,10))))
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
    [1..10]
    |> List.map (fun x -> createNewSymbol ())
    |> List.map (fun sym -> (sym.Id, sym))
    |> Map.ofList
    , Cmd.none

let setSelectedFunction (topLeft: XYPos, topRight: XYPos) (symModel: Model) : Model =
    symModel
    |> Map.map (fun _ sym ->
        if (withinSelectedBoundary {X=sym.Component.X; Y=sym.Component.Y} {X=sym.Component.X + sym.Component.W; Y=sym.Component.Y + sym.Component.H} topLeft topRight) then
            {sym with
                Selected = true  
            }
        else
            {sym with
                Selected = false
            }
    )


let updateSymbolModelWithComponent (symModel: Model) (comp: Component) : Model =
    symModel
    |> Map.add comp.Id {symModel.[comp.Id] with Component = comp}

let widthInference (symModel: Model) (pid1: PortId) (pid2: PortId) (addOrDelete: bool): Model =
    let p1 = pid1 |> findPort symModel
    let p2 = pid2 |> findPort symModel
    let (srcPort, tgtPort) = 
        match p1.PortType with
        |PortType.Input -> (p2, p1)
        |_ -> (p1, p2)

    let tgtSym =
        tgtPort
        |> findSymbolFromPort symModel

    match tgtSym.Component.Type with
    | SplitWire n -> 

        let tgtPortNew = 
            {tgtPort with
                Width = 
                    match addOrDelete with 
                    | true -> srcPort.Width
                    | false -> PortWidth 0
            }
        let variedOutputPort = findPortFromNumAndType tgtSym (PortNumber 0) (PortType.Output)
        let variedOutputPortNew = 
            {variedOutputPort with
                Width = 
                    match addOrDelete with
                    | true -> subtractPortWidth tgtPortNew.Width (PortWidth n)
                    | _ -> PortWidth 0
            }
        let tgtCompNew =
                {tgtSym.Component with
                    InputPorts = Map.add tgtPort.PortId tgtPortNew tgtSym.Component.InputPorts
                    OutputPorts = Map.add variedOutputPort.PortId variedOutputPortNew tgtSym.Component.OutputPorts
                }
        
        updateSymbolModelWithComponent symModel tgtCompNew
    | IOLabel ->
        let tgtPortNew = 
            {tgtPort with
                Width = 
                    match addOrDelete with 
                    | true -> srcPort.Width
                    | false -> PortWidth 0
            }
        let variedOutputPort = findPortFromNumAndType tgtSym (PortNumber 0) (PortType.Output)
        let variedOutputPortNew = 
            {variedOutputPort with
                Width = 
                    match addOrDelete with
                    | true -> tgtPortNew.Width
                    | _ -> PortWidth 0
            }
        let tgtCompNew = 
            {tgtSym.Component with
                InputPorts = Map.add tgtPort.PortId tgtPortNew tgtSym.Component.InputPorts
                OutputPorts = Map.add variedOutputPort.PortId variedOutputPortNew tgtSym.Component.OutputPorts
            }
        updateSymbolModelWithComponent symModel tgtCompNew
    | MergeWires ->
        let tgtPortNew = 
            {tgtPort with
                Width = 
                    match addOrDelete with 
                    | true -> srcPort.Width
                    | false -> PortWidth 0
            }
        let variedOutputPort = findPortFromNumAndType tgtSym (PortNumber 0) (PortType.Output)
        let otherInputPort:Port = 
            match tgtPort.PortNumber with
            | Some (PortNumber 0) -> findPortFromNumAndType tgtSym (PortNumber 1) (PortType.Input)
            | _ -> findPortFromNumAndType tgtSym (PortNumber 0) (PortType.Input)
        
        let variedOutputPortNew = 
            {variedOutputPort with
                Width = 
                    match addOrDelete with
                    | true -> addPortWidth tgtPortNew.Width otherInputPort.Width
                    | _ -> PortWidth 0
            }
        let tgtCompNew = 
            {tgtSym.Component with
                InputPorts = Map.add tgtPort.PortId tgtPortNew tgtSym.Component.InputPorts
                OutputPorts = Map.add variedOutputPort.PortId variedOutputPortNew tgtSym.Component.OutputPorts
            }
        updateSymbolModelWithComponent symModel tgtCompNew
    |_ -> symModel
/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (sType, pos, label) ->
        let compId = ComponentId (uuid())
        let comp = createSpecificComponent compId pos sType label
        let sym = 
            {
                LastDragPos = {X=0. ; Y=0.}
                IsDragging = false
                Id = compId
                Component = comp
                Selected = false
            }
        
        Map.add compId sym model, Cmd.none

    | DeleteSymbols _ ->
        Map.filter (fun _ sym -> not sym.Selected) model , Cmd.none
    
    | UpdateSymbolModelWithComponent comp ->
        updateSymbolModelWithComponent model comp, Cmd.none

    | SetSelected (sIdLst) ->
        model
        |> Map.map (fun _ sym -> 
            if List.contains sym.Id sIdLst then
                {sym with
                    Selected = true
                }
            else 
                {sym with
                    Selected = false
                }
        ) , Cmd.none

    | StartDragging (sIdLst, pagePos) ->
        model
        |> Map.map (fun _ sym -> 
            if List.contains sym.Id sIdLst then
                {sym with
                    LastDragPos = pagePos
                    IsDragging = true
                }
            else 
                sym
        ) , Cmd.none

    | Dragging (sIdLst, pagePos) -> 
        model
        |> Map.map (fun _ sym -> 
            if List.contains sym.Id sIdLst then
                let diff = posDiff pagePos sym.LastDragPos
                let symBBox = symbolBBox model sym.Id
                let symsInNewBBox = getSymbolsInTargetArea model {symBBox with Pos = posAdd symBBox.Pos diff}

                let movedSym =
                    {sym with
                        Component = {sym.Component with X = sym.Component.X + diff.X; Y = sym.Component.Y + diff.Y}
                        LastDragPos = pagePos
                    }

                match symsInNewBBox with
                | [id] when id = sym.Id -> movedSym
                | [] -> movedSym
                | _ -> sym
            else
                sym
            ), Cmd.none
    
    | EndDragging ->
        model
        |> Map.map (fun _ sym ->
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
        |> Map.map(fun _ sym ->
            {sym with
                Component = 
                {sym.Component with
                    InputPorts = 
                        sym.Component.InputPorts
                        |> Map.map (fun _ port ->
                            if List.contains port.PortId pIdLst then
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
                        |> Map.map (fun _ port ->
                            if List.contains port.PortId pIdLst then
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
        |> Map.map (
            (fun _ sym ->
                {sym with
                    Component = 
                        {sym.Component with
                            InputPorts =
                                sym.Component.InputPorts
                                |> Map.map (fun _ checkPort ->
                                {checkPort with   
                                        Hover = PortHover false
                                    }
                                )
                        }
                }
            ) >> 
            (fun _ sym ->
                {sym with
                    Component = 
                        {sym.Component with
                            InputPorts =
                                sym.Component.OutputPorts
                                |> Map.map (fun _ checkPort ->
                                {checkPort with   
                                        Hover = PortHover false
                                    }
                                )
                        }
                }
            )
        )
        ,Cmd.none

    | WidthInferrer (pid1,pid2) ->
        widthInference model pid1 pid2 true, Cmd.none

    | DeleteInference (pid1,pid2) ->
        widthInference model pid1 pid2 false, Cmd.none

    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderSymbolProps =
    {
        Symbol : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

/// View for one symbol with caching for efficient execution when input does not change
let private renderSymbol (model:Model) =
    
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->

            let fillColor =
                if props.Symbol.Selected then
                //if props.Symbol.IsDragging then
                    "#00d1b2"
                else
                    "#d3d3d3"
                    
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
            let selectedBool = props.Symbol.Selected

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
                | Custom customComp -> processingString customComp.Name 15

            //----------------------------Static Components----------------------------//
            let viewBoxStaticComponent : IProp seq =
                seq {
                        Style [
                                TextAnchor "left" 
                                FontSize "14px"
                                UserSelect UserSelectOptions.None
                            ]
                    }
            let viewboxExternalStaticLabelStyle: IProp seq = 
                let txtColor = 
                    match selectedBool with
                    | true -> "darkslategray"
                    | false -> "darkslategrey"
                
                let txtAnchor = 
                    match componentType with
                    | Input _ | Constant _ -> "start"
                    | _ -> "end"

                seq{Style [
                        UserSelect UserSelectOptions.None
                        TextAnchor txtAnchor
                        FontSize "14px"
                        Fill txtColor
                        // FontFamily "system-ui"
                        FontStyle "italic"
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
                let fullName = (string props.Symbol.Component.Label)

                let outputString = 
                    match width with
                    | x when x < 100. ->
                        processingString fullName 5
                    | x when x < 150. ->
                        processingString fullName 6
                    | x when x < 200. ->
                        processingString fullName 8
                    | _ ->
                        processingString fullName 10

                match componentType with 
                |Input _ |Constant _ ->
                    text 
                        (Seq.append [
                            X (bottomLeft.X + 3.)
                            Y (bottomLeft.Y - 17.)
                        ] viewboxExternalStaticLabelStyle) [str <| outputString]
                |_ -> 
                    text 
                        (Seq.append [
                            X (bottomRight.X - 3.)
                            Y (bottomRight.Y - 17.)
                        ] viewboxExternalStaticLabelStyle) [str <| outputString]

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
            
            let viewPortLinesStaticComponent2 : IProp seq = 
                seq {
                        SVGAttr.Fill fillColor
                        SVGAttr.Stroke outlineColor
                        SVGAttr.StrokeWidth 3
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
                            Y (topLeft.Y + 4.); 
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
                            Y (topLeft.Y + 4.); 
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
                            Y (topLeft.Y + 4.); 
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

                    match componentType with
                    | MergeWires -> 
                        let portPos1 =
                            topLeft.Y + (findPortFromNumAndType props.Symbol (PortNumber 0) (PortType.Input)).PortPos.Y

                        let portPos2 = 
                            topLeft.Y + (findPortFromNumAndType props.Symbol (PortNumber 1) (PortType.Input)).PortPos.Y
                            
                        let portPos3 =
                            topLeft.Y + (findPortFromNumAndType props.Symbol (PortNumber 0) (PortType.Output)).PortPos.Y

                        let midPosX = (topLeft.X + (width/2.))
                            
                        line 
                            (Seq.append
                                [
                                    X1 (topLeft.X + (width/2.))
                                    X2 (topRight.X)
                                    Y1 (portPos3)
                                    Y2 (portPos3)
                                ] viewPortLinesStaticComponent2) []

                        path
                            (Seq.append
                                [
                                    SVGAttr.D (sprintf
                                        "M %f %f
                                        L %f %f
                                        Q %f %f %f %f
                                        L %f %f
                                        Q %f %f %f %f
                                        L %f %f"
                                        topLeft.X portPos1
                                        (midPosX - 5.) portPos1
                                        midPosX portPos1 midPosX (portPos1 + 5.)
                                        midPosX (portPos2 - 5.)
                                        midPosX portPos2 (midPosX - 5.) portPos2
                                        topLeft.X portPos2
                                    )

                                ] viewPortLinesStaticComponent2) []

                    | SplitWire _ ->
                        let portPos1 =
                            topLeft.Y + (findPortFromNumAndType props.Symbol (PortNumber 0) (PortType.Input)).PortPos.Y
                        
                        let portPos2 = 
                            topLeft.Y + (findPortFromNumAndType props.Symbol (PortNumber 0) (PortType.Output)).PortPos.Y
                            
                        let portPos3 =
                            topLeft.Y + (findPortFromNumAndType props.Symbol (PortNumber 1) (PortType.Output)).PortPos.Y
                        
                        let midPosX = (topLeft.X + (width/2.))

                        line 
                            (Seq.append
                                [
                                X1 topLeft.X
                                X2 midPosX
                                Y1 portPos1
                                Y2 portPos1
                                ] viewPortLinesStaticComponent2)[]

                        path
                            (Seq.append
                                [
                                    SVGAttr.D (sprintf
                                        "M %f %f
                                        L %f %f
                                        Q %f %f %f %f
                                        L %f %f
                                        Q %f %f %f %f
                                        L %f %f"
                                        topRight.X portPos2
                                        (midPosX + 5.) portPos2
                                        midPosX portPos2 midPosX (portPos2 + 5.)
                                        midPosX (portPos3 - 5.)
                                        midPosX portPos3 (midPosX + 5.) portPos3
                                        topRight.X portPos3
                                    )

                                ] viewPortLinesStaticComponent2) []
                    | _ -> nothing
                ]

            let lineLength = 15.

            let viewPortLinesType1 (compType:ComponentType)=
                let generateLines (portMap: Map<PortId, Port>) : ReactElement list =
                    portMap
                    |> Map.toList
                    |> List.map(fun (_, port:Port) ->
                        let absPos () = posAdd  topLeft port.PortPos

                        let dynamicContent =
                            match port.PortType with
                            | PortType.Input -> ((absPos()).X-lineLength), ((absPos()).X - 10.)
                            | PortType.Output -> ((absPos()).X+lineLength), ((absPos()).X + 10.)
                        let (PortNumber portNumber) =    
                            match port.PortNumber with
                            |Some a -> a
                            |None -> PortNumber -1
                        match portNumber with
                        |portCheck when portCheck >= 0 ->
                            g[] [
                                circle 
                                    (Seq.append [
                                        Cx (absPos()).X
                                        Cy (absPos()).Y
                                        R 3.
                                    ] (viewPortLinesStaticComponent port))[]
                                let (PortWidth wid) = port.Width
                                if selectedBool = true && (wid > 0) then
                                    text (Seq.append [
                                        X (snd dynamicContent)
                                        Y ((absPos()).Y - 20.)
                                        ] (viewPortBusIndicatorTextStaticComponent port)) [str <| string (port.Width)]
                                else nothing

                                match compType with 
                                | ComponentType.Not | ComponentType.Nand | ComponentType.Nor | ComponentType.Xnor ->
                                    match port.PortType with
                                    |PortType.Output -> 
                                        g [] [
                                            line 
                                                (Seq.append [
                                                    X1 ((absPos()).X - lineLength)
                                                    X2 ((fst dynamicContent) - lineLength);
                                                    Y2 (absPos()).Y
                                                    Y1 (absPos()).Y
                                                ] (viewPortLinesStaticComponent2))[]
                                            line
                                                (Seq.append [
                                                    X1 ((absPos()).X + 12. - lineLength)
                                                    X2 ((absPos()).X - lineLength)
                                                    Y2 ((absPos()).Y - 12.)
                                                    Y1 (absPos()).Y
                                                ] (viewPortLinesStaticComponent2))[]
                                        ]
                                    |_ -> nothing
                                | _ -> nothing
                            ]
                        |_ -> nothing
                    )

                (generateLines inputPorts) @ (generateLines outputPorts)

            //----------------------------viewPorts Functions---------------------------//
            let viewPortsInput : ReactElement list  = []
            let viewPortsOutput : ReactElement list = []                     
                
            let generatePorts (port:Port) (portLabel:string) : ReactElement =
                text (
                    Seq.append [
                        match port.PortType with
                        |PortType.Input -> 
                            X (topLeft.X + port.PortPos.X + 8.)
                            Y (topLeft.Y + port.PortPos.Y - 10.)
                        |PortType.Output ->
                            X (topLeft.X + port.PortPos.X - 8.)
                            Y (topLeft.Y + port.PortPos.Y - 10.)
                    ] (viewPortsStaticComponent port.PortType)
                ) [str<|portLabel]

            let viewPortsType1 (compType:ComponentType): ReactElement list = 
                let customCompHelper inputOutputLabelLst  inputOutputPorts =
                    inputOutputPorts
                    |> Map.toList
                        |> List.map (fun (_,port) ->
                            let portNumber =
                                match port.PortNumber with
                                | Some (PortNumber portNum) -> portNum
                                | None -> -1
                            if portNumber >= 0 then
                                let expandedPortLabel = 
                                    List.mapi (fun i (value, _) -> (i,value)) inputOutputLabelLst
                                let portLabel = 
                                    List.pick (fun elem ->
                                        match elem with
                                        |(x,label) when x = portNumber -> Some (processingString label 8)
                                        | _ -> None
                                    ) expandedPortLabel
                                generatePorts port portLabel
                            else nothing
                        )

                let inputList =
                    match compType with
                    |Custom customParams ->
                        customCompHelper customParams.InputLabels inputPorts
                    |_ ->
                        inputPorts
                        |> Map.toList
                        |> List.map (fun (_, port) ->
                            let portNumber = 
                                match port.PortNumber with 
                                | Some (PortNumber portNum) -> portNum
                                | None -> -1
                            match compType with
                            |Decode4 ->
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Sel"
                                |portNum when portNum = 1 -> generatePorts port "Data"
                                |_ -> nothing
                            |Mux2 ->
                                match portNumber with
                                |portNum when portNum < 2 -> generatePorts port (string portNumber)
                                |portNum when portNum = 2 -> generatePorts port "Sel"
                                |_ -> nothing
                            |Demux2 ->
                                match portNumber with
                                |portNum when portNum < 1 -> generatePorts port (string portNumber)
                                |portNum when portNum = 1 -> generatePorts port "Sel"
                                |_ -> nothing
                            |NbitsAdder _ ->
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Cin"
                                |portNum when portNum = 1 -> generatePorts port "A"
                                |portNum when portNum = 2 -> generatePorts port "B"
                                |_ -> nothing
                            |DFF ->
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "D"
                                |_ -> nothing
                            |DFFE ->
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "D"
                                |portNum when portNum = 1 -> generatePorts port "EN"
                                |_ -> nothing
                            |Register _ ->
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Data-In"
                                |_ -> nothing
                            |RegisterE _ ->
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Data-In"
                                |portNum when portNum = 1 -> generatePorts port "EN"
                                |_ -> nothing
                            |ROM _ | AsyncROM _ ->
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Addr"
                                |_ -> nothing
                            |RAM _ ->
                                match portNumber with
                                |portNum when portNum = 0 -> generatePorts port "Addr"
                                |portNum when portNum = 1 -> generatePorts port "Data-In"
                                |portNum when portNum = 2 -> generatePorts port "Write"
                                |_ -> nothing
                            |_ -> nothing
                        )
                let outputList =
                    match compType with
                    |Custom customParams ->
                        customCompHelper customParams.OutputLabels outputPorts
                    | _ -> 
                        outputPorts
                        |> Map.toList
                        |> List.map (fun (_, port)->
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
                        )
                inputList @ outputList

            //----------------------------viewPortLines Functions---------------------------//
            
            let viewPortLinesOutput =
                let port = snd (List.head (inputPorts |> Map.toList))
                [g [] 
                    [
                        circle 
                            (Seq.append [
                                Cx topLeft.X
                                Cy (0.5*(topLeft.Y+bottomLeft.Y))
                                R 3.
                            ] (viewPortLinesStaticComponent port))[]

                        if selectedBool = true then
                            text (Seq.append [
                                X (topLeft.X - lineLength)
                                Y ((0.5*(topLeft.Y+bottomLeft.Y)) - 20.)
                                ] (viewPortBusIndicatorTextStaticComponent port)) [str <| string (port.Width)]
                        else nothing
                    ]
                ]
            let viewPortLinesInput =
                let port = snd (List.head (outputPorts |> Map.toList))
                [g [] 
                    [
                        circle 
                            (Seq.append [
                                Cx topRight.X
                                Cy (0.5*(topLeft.Y+bottomLeft.Y))
                                R 3.
                            ] (viewPortLinesStaticComponent port))[]

                        if selectedBool = true then
                            text (Seq.append [
                                X (topRight.X + lineLength)
                                Y ((0.5*(topLeft.Y+bottomLeft.Y)) - 20.)
                                ] (viewPortBusIndicatorTextStaticComponent port)) [str <| string (port.Width)]
                        else nothing
                    ]
                ]

            //----------------------------Combine All View Functions---------------------------//
            let viewOverall (compType : ComponentType) = 
                match compType with 
                | ComponentType.Input _ -> viewBoxInput @ viewPortsInput @ viewPortLinesInput
                | ComponentType.Output _ -> viewBoxOutput @ viewPortsOutput @ viewPortLinesOutput
                | ComponentType.Constant (_,n) -> (viewBoxConstant n) @ viewPortsInput @ viewPortLinesInput
                | _ -> viewBoxType1 @ (viewPortLinesType1 compType) @ (viewPortsType1 compType)
                
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
    |> Map.map (fun _ ({Id = ComponentId id} as symbol) ->
        renderSymbol model
            {
                Symbol = symbol
                Dispatch = dispatch
                key = id
            }
    )
    |> Map.toList
    |> List.map snd
    |> ofList


//---------------Other interface functions--------------------//


/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth (wId: ConnectionId) (outputPortNumber: int) (inputPortWidths: int option list) : int option =
    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
let extractComponent (symModel: Model) (sId:ComponentId) : Component= 
    (getSymbolFromSymbolId symModel sId).Component

let extractComponents (symModel: Model) : Component list = 
    symModel
    |> Map.toList
    |> List.map (fun (_,sym) ->
        sym.Component
    )
