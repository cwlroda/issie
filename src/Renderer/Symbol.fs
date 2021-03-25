module Symbol
open Fable.React
open Fable.React.Props
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
        Shadow : bool
    }


type Model = Map<ComponentId, Symbol>

//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    | StartDragging of sId : ComponentId list * pagePos: XYPos      // Start Dragging Symbols
    | Dragging of sIdLst: ComponentId list * pagePos: XYPos         // Continue Dragging Symbols
    | EndDragging                                                   // Stop Dragging Symbols
    | AddSymbol of comp: Component                                  // Create a New Symbol
    | DeleteSymbols of sIdLst: ComponentId list                     // Delete All Symbols in List
    | UpdateSymbolModelWithComponent of Component                   // Issie interface
    | CreateInference of (PortId*PortId)                            // Updates the Widths of Symbols Connected by the Two PortIds that have Width Inferred Ports
    | DeleteInference of (PortId*PortId)                            // Resets the Widths of Symbols Connected by the Two PortIds that have Width Inferred Ports


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
        | point1,point2 when 
            (point1.X >= boundTopLeft.X) 
            && (point2.X <= boundBotRight.X) 
            && (point1.Y >= boundTopLeft.Y) 
            && (point2.Y <= boundBotRight.Y) -> true
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

let nearbyPorts (symModel: Model) (pos: XYPos) : Port list = 
    allPortsInModel symModel
    |> Map.filter
        (fun _ port ->
            portPos symModel port.PortId
            |> posDist pos <= 10.
        )
    |> Map.toList
    |> List.map snd
    |> List.sortBy
        (fun port ->
            portPos symModel port.PortId
            |> posDist pos
        )

let getTargetedPort (symModel: Model) (pos: XYPos) : PortId Option =
    match nearbyPorts symModel pos with
    | nearestPort::_ -> Some nearestPort.PortId
    | [] -> None

let getSpecificPort (symModel: Model) (pos: XYPos) (portType: PortType) : PortId Option =
    let portList =
        nearbyPorts symModel pos
        |> List.filter (fun p -> p.PortType = portType)
    
    match portList with
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

let componentBBox (comp: Component) : BBox =
    {
        Pos = {X = comp.X - gridSize; Y = comp.Y}
        Width = comp.W + (gridSize * 2.)
        Height = comp.H
    }

let symbolBBox (symModel: Model) (compId: ComponentId) : BBox =
    let foundSymbol = 
        Map.find compId symModel

    componentBBox foundSymbol.Component

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

let symbolsCollide (idLst : ComponentId list) (model : Model) : bool = 
    idLst
    |> List.collect (fun sId ->
        let symBBox = symbolBBox model sId
        getSymbolsInTargetArea model symBBox
        |> List.collect (fun matchedId -> if List.contains matchedId idLst then [] else [matchedId])
    )
    |> List.length
    |> (<) 0

let mulOfFive (input:float)  : float = 
    10. * float (int (input / 10.))
    
//-----------------------------Skeleton Model Type for symbols----------------//

//------------------------------Create Symbols---------------------//
let rng = System.Random 0
let rng2() = rng.Next(0,2)
let createSpecificComponent (position:XYPos) (compType:ComponentType) (labelName:string) : Component =
    let compId = ComponentId (uuid())
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
                | false -> float (portNumber + 1) * (compH/ (float (totalPorts + 1)))
            let offset = 
                match portType with 
                |PortType.Input -> 0.
                |PortType.Output -> compW        
            {
                PortId = PortId (uuid())
                PortNumber =  Some (PortNumber (portNumber))
                PortType = portType
                PortPos = (snapToGrid {X=offset; Y =  yPosCalc })
                HostId = compId
                Width = portWidth
            }
        | false ->
            {
                PortId = PortId (uuid())
                PortNumber =  None
                PortType = portType
                PortPos = {X=0.; Y=0. }
                HostId = compId
                Width = portWidth
            }

    let (inputPorts, outputPorts): (Map<PortId, Port> * Map<PortId, Port>) =
        let offset = 10.
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
                [portTemplate (true) (0) (PortType.Output) (PortWidth n) (false) (1)] 
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap
        |Output n ->
            let outputPortMap = 
                [portTemplate (false) (0) (PortType.Output) (PortWidth 0) (false) (0)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let inputPortMap = 
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
                    )
                    |> List.map (fun port -> (port.PortId, port))
                    |> Map.ofList

            inputPortMap,outputPortMap

        | And | Or | Xor ->
            let inputPortMap = 
                [0;1]
                |> List.map (fun portNumber ->
                    portTemplate (true) (portNumber) (PortType.Input) (PortWidth 1) (false) (2) 
                )
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                [portTemplate (true) (0) (PortType.Output) (PortWidth 1) (false) (1)] 
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap,outputPortMap

        | Nand | Nor | Xnor ->
            let inputPortMap = 
                [0;1]
                |> List.map (fun portNumber -> 
                    portTemplate (true) (portNumber) (PortType.Input) (PortWidth 1) (false) (2) 
                )
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                let temp = portTemplate (true) (0) (PortType.Output) (PortWidth 1) (false) (1) 
                [{temp with 
                    PortPos =
                        {temp.PortPos with
                            X = compW + offset
                        }
                }]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList
            inputPortMap, outputPortMap

        | Not ->
            let inputPortMap = 
                [portTemplate (true) (0) (PortType.Input) (PortWidth 1) (false) (1)]
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            let outputPortMap = 
                let temp = portTemplate (true) (0) (PortType.Output) (PortWidth 1) (false) (1) 
                [{temp with 
                    PortPos =
                        {temp.PortPos with
                            X = compW + offset
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
                |> List.map (fun portNumber -> portTemplate (true) (portNumber) (PortType.Output) (PortWidth 1) (true) (4))
                |> List.map (fun port -> (port.PortId, port))
                |> Map.ofList

            inputPortMap, outputPortMap

    {
        Id = compId
        Type = compType
        Label = labelName
        InputPorts = inputPorts
        OutputPorts = outputPorts
        X = compX
        Y = compY
        H = compH
        W = compW
    }

let updateCompoment (comp: Component) (p: XYPos) (label: string) : Component =
    { comp with
        Label = label
        X = p.X
        Y = p.Y
    }

let createDeepCopyOfComponent (comp: Component): Component * Map<PortId, PortId> =
    let compId = ComponentId (uuid())

    let createCopyOfPortMap portMap =
        let createDeepCopyOfPort (p: Port) (sId: ComponentId) =
            { p with
                HostId = sId
                PortId = PortId (uuid())
            }

        let (newPortList, conversionList) =
            portMap
            |> Map.toList
            |> List.map (fun (oldId, oldPort) -> 
                let newPort = createDeepCopyOfPort oldPort compId
                (newPort.PortId, newPort), (oldId, newPort.PortId)
            )
            |> List.unzip

        Map.ofList newPortList, conversionList

    let (inputPorts, inputPortConversion) = createCopyOfPortMap comp.InputPorts
    let (outputPorts, outputPortConversion) = createCopyOfPortMap comp.OutputPorts

    { comp with
        Id = compId
        InputPorts = inputPorts
        OutputPorts = outputPorts
    }, Map.ofList <| inputPortConversion @ outputPortConversion

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
let createCompType (compNo:int):ComponentType =
    let rng0 () = rng.Next (1,10)
    let memory () = {AddressWidth = rng0(); WordWidth = rng0(); Data=Map.empty}
    match (compNo) with
    | 1 -> Not
    | 2 -> And
    | 3 -> Or
    | 4 -> Xor
    | 5 -> Nand
    | 6 -> Nor
    | 7 -> Xnor
    | 8 -> Mux2
    | 9 -> NbitsAdder (rng0 ())
    | 10 -> DFF
    | 11 -> DFFE
    | 12 -> Register (rng0 ())
    | 13 -> RegisterE (rng0())
    | 14 -> AsyncROM (memory ())
    | 15 -> ROM (memory())
    | 16 -> RAM (memory())
    | 17 -> Decode4
    | 18 -> Input (rng0 ())
    | 19 -> Output (rng0 ())
    | 20 -> Demux2
    | 21 -> IOLabel
    | 22 -> MergeWires
    | 23 -> BusSelection (rng0(),rng0())
    | 24 -> 
        let cons = rng0()
        let wid = int ((log(float cons)/log(2.))+1.)
        Constant (wid, cons)
    | _-> SplitWire (rng.Next(1,9))

let createNewSymbol (index:int)  =
    let rngComponent () = rng.Next(1,27)
    let compType = 
        let customComp (custNo:int):CustomComponentType = 
            let labels (inputOutput:bool) (count:int) = 
                let inOrOut = 
                    match inputOutput with
                    |true -> "TestInput"
                    |false -> "TestOutput"


                [0..count-1]
                |> List.map (fun i -> ((string i + inOrOut), rng.Next(1,10)))

            {
                Name = "\"Our\" Custom Component"
                InputLabels = 
                    match custNo with
                    | 1 -> labels true 2
                    | 2 -> labels true 3
                    | 3 -> labels true 5
                    | 4 -> labels true 3
                    | _ -> labels true 10
                OutputLabels = 
                    match custNo with
                    | 1 -> labels false 3
                    | 2 -> labels false 2
                    | 3 -> labels false 2
                    | 4 -> labels false 6
                    | _ -> labels false 10
            }
        let componentType = 
            match index with
            | x when x < 46 ->
                let componentNo = 
                    match index with
                    | x when x < 26 -> x
                    | _ -> rngComponent()
                
                
                createCompType componentNo
            
            | x when x = 46 ->
                Custom (customComp 1)
            | x when x = 47 ->
                Custom (customComp 2)
            | x when x = 48 ->
                Custom (customComp 3)
            | x when x = 49 ->
                Custom (customComp 4)
            | x when x = 50 ->
                Custom (customComp 5)
            |_ -> And

        componentType


    let rng1 () = rng.Next(0,800)

    let positionX = (index % 5) * 250
    let positionY = (index / 5) * 200
    let comp = 
        createSpecificComponent (snapToGrid {X= float positionX;Y = float positionY }) compType (randomName() + (string(rng.Next (0,10))))
    {
        LastDragPos = {X=0. ; Y=0.}
        IsDragging = false
        Id = comp.Id
        Component = comp
        Shadow = false
    }

/// Dummy function for test. The real init would probably have no symbols.
let init () =
    [1..50]
    
    |> List.map (fun x -> createNewSymbol (x))
    |> List.map (fun sym -> (sym.Id, sym))
    |> Map.ofList
    , Cmd.none

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
                    | true -> 
                        let (PortWidth wid) = srcPort.Width
                        if wid >= 0 then srcPort.Width else PortWidth -1
                    | false -> PortWidth 0
            }
        let variedOutputPort = findPortFromNumAndType tgtSym (PortNumber 0) (PortType.Output)
        let variedOutputPortNew = 
            {variedOutputPort with
                Width = 
                    match addOrDelete with
                    | true -> 
                        if tgtPortNew.Width = (PortWidth -1) then PortWidth 0 
                        else subtractPortWidth tgtPortNew.Width (PortWidth n)
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
                    | true -> 
                        let (PortWidth wid) = srcPort.Width
                        if wid >= 0 then srcPort.Width else PortWidth -1
                    | false -> PortWidth 0
            }
        let variedOutputPort = findPortFromNumAndType tgtSym (PortNumber 0) (PortType.Output)
        let variedOutputPortNew = 
            {variedOutputPort with
                Width = 
                    match addOrDelete with
                    | true -> 
                        if tgtPortNew.Width = (PortWidth -1) then PortWidth 0 
                        else tgtPortNew.Width
                    | _ -> PortWidth 0
            }
        let tgtCompNew = 
            {tgtSym.Component with
                InputPorts = Map.add tgtPort.PortId tgtPortNew tgtSym.Component.InputPorts
                OutputPorts = Map.add variedOutputPort.PortId variedOutputPortNew tgtSym.Component.OutputPorts
            }
        updateSymbolModelWithComponent symModel tgtCompNew

    | BusSelection (outWid,outSelWid) ->
        let (PortWidth srcWid) = srcPort.Width
        let tgtPortNew = 
            {tgtPort with
                Width = 
                    match addOrDelete with
                    | true -> 
                        if srcWid >= outWid + outSelWid then srcPort.Width
                        else PortWidth (outWid + outSelWid)
                    | false -> PortWidth (outWid + outSelWid)
            }
        let tgtCompNew = 
            {tgtSym.Component with
                InputPorts = Map.add tgtPort.PortId tgtPortNew tgtSym.Component.InputPorts
            }
        updateSymbolModelWithComponent symModel tgtCompNew

    | MergeWires ->
        let tgtPortNew = 
            {tgtPort with
                Width = 
                    match addOrDelete with 
                    | true -> 
                        let (PortWidth wid) = srcPort.Width
                        if wid >= 0 then srcPort.Width else PortWidth -1
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
                    | true -> 
                        if ((tgtPortNew.Width = (PortWidth -1)) || (otherInputPort.Width = (PortWidth -1)))  then PortWidth 0 
                        else (addPortWidth tgtPortNew.Width otherInputPort.Width)
                    | _ -> 
                        let (PortWidth port1Width) = tgtPortNew.Width
                        let (PortWidth port2Width) = otherInputPort.Width

                        match port1Width with
                            | wid when wid < port2Width -> PortWidth port2Width
                            | _ -> PortWidth port1Width 
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
    | AddSymbol comp ->
        let sym = 
            {
                LastDragPos = {X=0. ; Y=0.}
                IsDragging = false
                Id = comp.Id
                Component = comp
                Shadow = false
            }
        
        Map.add comp.Id sym model, Cmd.none

    | DeleteSymbols sIdLst ->
        let sIdSet = Set.ofList sIdLst
        Map.filter (fun _ sym -> not <| Set.contains sym.Id sIdSet) model
        , Cmd.none
    
    | UpdateSymbolModelWithComponent comp ->
        updateSymbolModelWithComponent model comp, Cmd.none

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
        let newModel = 
            model
            |> Map.map(fun _ sym ->
                if List.tryFind (fun sId -> sId = sym.Id) sIdLst <> None then
                    let diff = posDiff pagePos sym.LastDragPos
                    {sym with
                        Component ={sym.Component with X = sym.Component.X + diff.X; Y = sym.Component.Y + diff.Y}
                        LastDragPos = pagePos
                    }
                else
                    sym
            )

        let collision = (symbolsCollide sIdLst newModel)        
        newModel
        |> Map.map (fun _ sym ->
            if collision then 
                if List.tryFind (fun sId -> sId = sym.Id) sIdLst <> None then
                    {sym with Shadow = true}
                else
                    {sym with Shadow = false}
            else 
                {sym with Shadow = false}
        ),

        Cmd.none
    
    | EndDragging ->
        model
        |> Map.map (fun _ sym ->
            if sym.IsDragging then
                { sym with IsDragging = false }
            else sym
        ), Cmd.none

    | CreateInference (pid1,pid2) ->
        widthInference model pid1 pid2 true, Cmd.none

    | DeleteInference (pid1,pid2) ->
        widthInference model pid1 pid2 false, Cmd.none

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderSymbolProps =
    {
        Symbol : Symbol // name works for the demo!
        Selected: bool
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
        MousePos: XYPos
        PortTypeToNotHighlight: PortType option
    }

/// View for one symbol with caching for efficient execution when input does not change
let private renderSymbol =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let opacity = if props.Symbol.Shadow then "20%" else "100%"

            let fillColor =
                if props.Selected then
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

            let portHover (port: Port) =
                match props.PortTypeToNotHighlight with
                | Some pType when pType = port.PortType -> false
                | _ -> 
                    let portHighlightRange = 50.
                    let portPos = posAdd port.PortPos topLeft
                    let diff = posDiff portPos props.MousePos
                    let distSquared = diff.X * diff.X + diff.Y * diff.Y

                    distSquared <= portHighlightRange * portHighlightRange

            let inputPorts = props.Symbol.Component.InputPorts
            let outputPorts = props.Symbol.Component.OutputPorts
            
            let componentType = props.Symbol.Component.Type
            let selectedBool = props.Selected

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
                                Opacity opacity
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
                        FontSize "13px"
                        Fill txtColor
                        FontStyle "italic"
                        Opacity opacity
                    ]
                }
                
            let viewBoxInternalStaticLabelStyle: IProp seq = 
                seq {Style [
                            UserSelect UserSelectOptions.None
                            TextAnchor "middle"
                            FontSize "20px"
                            Opacity opacity
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
                            X (bottomLeft.X + 5.)
                            Y (bottomLeft.Y - 14.)
                        ] viewboxExternalStaticLabelStyle) [str <| outputString]
                |_ -> 
                    text 
                        (Seq.append [
                            X (bottomRight.X - 5.)
                            Y (bottomRight.Y - 14.)
                        ] viewboxExternalStaticLabelStyle) [str <| outputString]

            let viewBoxClock (bottomLeft:XYPos): ReactElement =
                g [] 
                    [
                        polygon 
                            (Seq.append [
                                Points (sprintf "%f %f, %f %f, %f %f" bottomLeft.X (bottomLeft.Y-5.) (bottomLeft.X+10.) (bottomLeft.Y-10.) (bottomLeft.X) (bottomLeft.Y-15.))
                                SVGAttr.Fill "black"
                                SVGAttr.Stroke "black"
                                SVGAttr.StrokeWidth 2
                                SVGAttr.Opacity opacity
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
                        Opacity opacity
                        UserSelect UserSelectOptions.None
                    ]
                }

            let viewPortLinesStaticComponent (x:Port) : IProp seq = 
                seq {
                    if portHover x then
                        SVGAttr.Fill "red"
                        SVGAttr.Stroke "red"
                        SVGAttr.StrokeWidth 8
                        SVGAttr.Opacity opacity
                    else
                        SVGAttr.Fill fillColor
                        SVGAttr.Opacity opacity
                        SVGAttr.Stroke outlineColor
                        SVGAttr.StrokeWidth 6
                }
            
            let viewPortLinesStaticComponent2 : IProp seq = 
                seq {
                        SVGAttr.Fill fillColor
                        SVGAttr.Opacity opacity
                        SVGAttr.Stroke outlineColor
                        SVGAttr.StrokeWidth 3
                }

            let viewPortBusIndicatorLinesStaticComponent (x:Port) : IProp seq =
                seq {
                    if portHover x then
                        SVGAttr.Fill "red"
                        SVGAttr.Stroke "red"
                        SVGAttr.StrokeWidth 4
                        SVGAttr.Opacity opacity
                    else
                        SVGAttr.Fill fillColor
                        SVGAttr.Opacity opacity
                        SVGAttr.Stroke outlineColor
                        SVGAttr.StrokeWidth 3
            }

            let viewPortBusIndicatorTextStaticComponent (x:Port) : IProp seq =
                seq {
                Style [
                        
                        TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                        DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                        FontSize "12px"
                        FontWeight "Bold"
                        Fill "Blue" // demo font color
                        Opacity opacity
                        UserSelect UserSelectOptions.None
                    ]
            }

            //----------------------------viewBox Functions---------------------------//
            let viewBoxOutput =
                [
                    polygon
                        (Seq.append [
                            Points (sprintf "%f %f, %f %f, %f %f , %f %f, %f %f" topLeft.X  (0.5*(topLeft.Y+bottomLeft.Y)) (topLeft.X+10.) topLeft.Y topRight.X topRight.Y bottomRight.X bottomRight.Y (bottomLeft.X+10.) bottomLeft.Y )
                            SVGAttr.Fill fillColor
                            SVGAttr.Opacity opacity
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
                            Points (sprintf "%f %f, %f %f, %f %f , %f %f, %f %f" topLeft.X  topLeft.Y (topRight.X-10.) topRight.Y topRight.X (0.5*(topRight.Y+bottomRight.Y)) (bottomRight.X-10.) bottomRight.Y bottomLeft.X bottomLeft.Y)
                            SVGAttr.Fill fillColor
                            SVGAttr.Opacity opacity
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
                            Points (sprintf "%f %f, %f %f, %f %f , %f %f, %f %f" topLeft.X  topLeft.Y (topRight.X-10.) topRight.Y topRight.X (0.5*(topRight.Y+bottomRight.Y)) (bottomRight.X-10.) bottomRight.Y bottomLeft.X bottomLeft.Y)
                            SVGAttr.Fill fillColor 
                            SVGAttr.Opacity opacity
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
                            Rx 5.
                            Ry 5.
                            SVGAttr.Width width
                            SVGAttr.Height height
                            SVGAttr.Fill fillColor
                            SVGAttr.Opacity opacity
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
                                    D (sprintf
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

            let lineLength = 10.

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
                                        R 2.5
                                    ] (viewPortLinesStaticComponent port ))[]
                                let (PortWidth wid) = port.Width
                                if selectedBool = true && (wid > 0) then
                                    text (Seq.append [
                                        X (snd dynamicContent)
                                        Y ((absPos()).Y - 20.)
                                        ] (viewPortBusIndicatorTextStaticComponent port)) [str <| string (port.Width)]
                                else nothing

                                match compType with 
                                | Not | Nand | Nor | Xnor ->
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

let view (model : Model) (selectedSymbols: CommonTypes.ComponentId list option) (mousePos: XYPos) (portTypeToNotHighlight: PortType option) (dispatch : Msg -> unit) = 
    let selectedSet =
        match selectedSymbols with
        | Some sIdLst -> Set.ofList sIdLst
        | None -> Set.empty

    let (selectedSyms, unselectedSyms) =
        model
        |> Map.toList
        |> List.map snd
        |> List.partition (fun sym -> Set.contains sym.Id selectedSet)

    let renderView symbols selected : ReactElement list =
        symbols
        |> List.map (fun symbol ->
            let (ComponentId sId) = symbol.Id

            renderSymbol
                {
                    Symbol = symbol
                    Dispatch = dispatch
                    Selected = selected
                    key = sId
                    MousePos = mousePos
                    PortTypeToNotHighlight = portTypeToNotHighlight
                }
        )

    (renderView unselectedSyms false @ renderView selectedSyms true)
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


// Our team has created a type PortId to represent ports. 
// Issie may not recognise this, hence it might be useful for Issie if we translate 
// PortId into (ComponentId, PortNumber, PortType) Instead

let translatePortToIssieFormat (symModel: Model) (portId: PortId): (ComponentId*PortNumber*PortType) =
    let foundPort = 
        portId
        |> findPort symModel
    match foundPort.PortNumber with
    | Some x ->
        foundPort.HostId,x,foundPort.PortType
    | None -> failwithf "won't happen"

