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




/// <summary> Contains the component and all its relevant attributes. </summary>
/// <typeparam name="LastDragPos"> ```XYPos``` indicating the previous dragging location of the component.</typeparam>
/// <typeparam name="IsDragging"> ```bool``` that is ```true``` if the symbol is currently dragging and ```false``` otherwise. </typeparam>
/// <typeparam name="Id"> ```ComponentId``` indicating the unique ID for the symbol shared with Issie Component type. </typeparam>
/// <typeparam name="Component"> ```Component``` indicating the underlying component in the symbol. </typeparam>
/// <typeparam name="Shadow"> ```bool``` that is ```true``` if the symbol is currently in the preview state and ```false``` otherwise.</typeparam>
/// <typeparam name="Colliding"> ```bool``` that is ```true``` if the symbol is currently colliding with another symbol and ```false``` otherwise. </typeparam>
type Symbol =
    {
        /// <summary> ```XYPos``` indicating the previous dragging location of the component.</summary>
        LastDragPos : XYPos
        /// <summary> ```bool``` that is ```true``` if the symbol is currently dragging and ```false``` otherwise. </summary>
        IsDragging : bool
        /// <summary> ```ComponentId``` indicating the unique ID for the symbol shared with Issie Component type. </summary>
        Id : ComponentId
        /// <summary> ```Component``` indicating the underlying component in the symbol. </summary>
        Component : Component
        /// <summary> ```bool``` that is ```true``` if the symbol is currently in the preview state and ```false``` otherwise.</summary>
        Shadow : bool
        /// <summary> ```bool``` that is ```true``` if the symbol is currently colliding with another symbol and ```false``` otherwise. </summary>
        Colliding : bool
    }

/// <summary> ```Map``` containing all ```Symbol``` as ```Value``` and their respective ```ComponentId``` as ```Key``` </summary>\
type Model = Map<ComponentId, Symbol>

//----------------------------Message Type-----------------------------------//

/// <summary> Messages to update symbol model </summary>
/// <typeparam name="StartDragging"> Start dragging symbols at a position </typeparam>
/// <typeparam name="Dragging"> Continue dragging symbols, updates their positions and checks if the mouse is down  </typeparam>
/// <typeparam name="EndDragging"> Stop dragging symbols </typeparam>
/// <typeparam name="AddSymbol"> Create a new symbol of ```Component``` </typeparam>
/// <typeparam name="DeleteDymbols"> Deletes the given list of symbols </typeparam>
/// <typeparam name="UpdateSymbolModelWithComponent"> Issie interface </typeparam>
/// <typeparam name="CreateInference"> Updates the widths of symbols connected by two ```PortId``` that have undefined ```PortWidth```  </typeparam>
/// <typeparam name="StartDragging"> Resets the widths of symbols connected by two ```PortId``` that have undefined ```PortWidth``` </typeparam>
type Msg =
    /// <summary> Start dragging symbols </summary>
    | StartDragging of sId : ComponentId list * pagePos: XYPos
    /// <summary> Continue dragging symbols, updates their positions and checks if the mouse is down  </summary>
    | Dragging of sIdLst: ComponentId list * pagePos: XYPos * mouseisDown: bool     
    /// <summary> Stop dragging symbols </summary>
    | EndDragging                                        
    /// <summary> Create a new symbol of ```Component``` </summary>                       
    | AddSymbol of comp: Component                          
    /// <summary> Deletes the given list of symbols</summary>                          
    | DeleteSymbols of sIdLst: ComponentId list             
    ///<summary> Issie interface</summary>                       
    | UpdateSymbolModelWithComponent of Component      
    /// <summary> Updates the widths of symbols connected by two ```PortId``` that have undefined ```PortWidth```  </summary>                  
    | CreateInference of (PortId*PortId)                
    /// <summary> Resets the widths of symbols connected by two ```PortId``` that have undefined ```PortWidth``` </summary>                                    
    | DeleteInference of (PortId*PortId)                                           


//---------------------------------Functions ----------------------------------------//

/// <summary> Cuts the length of the input string to match the threshold.
/// Replaces any excess characters with ```..``` . </summary>
/// <param name="inputString"> The input string represented by ```string``` </param>
/// <param name="thresholdLength"> The threshold string length represneted by ```int``` </param>
/// <returns> The cut string represented by ```string``` </returns> 
let processingString (inputString) (thresholdLength): string = 
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

/// <summary> Combines all valid input and outpot ports of a symbol into a map </summary>
/// <param name="sym"> The symbol to extract the ports from </param>
/// <returns> Map of all valid ```Port``` in the symbol as ```Value``` and their respective ```PortIds``` as ```Key``` </returns>
let combinedPortsMap (sym: Symbol) : Map<PortId, Port> =
    let filledPortList (portMap: Map<PortId, Port>) : Map<PortId, Port> = 
        portMap
        |> Map.filter (fun _ port -> port.PortNumber <> None)

    Map.fold (fun acc k v -> Map.add k v acc) (filledPortList sym.Component.InputPorts) (filledPortList sym.Component.OutputPorts)

/// <summary> Finds the specific port given the symbol, its port number and its port type.</summary>
/// <param name="sym"> The Symbol to find that specific port </param>
/// <param name="portNum"> The specific port's port number </param>
/// <param name="portType"> The specific port's port type </param>
/// <returns> The specific port in the symbol as ```Port``` </returns>
let findPortFromNumAndType (sym: Symbol) (portNum: PortNumber) (portType: PortType): Port =
    combinedPortsMap sym
    |> Map.toList
    |> List.find ( fun (_,port) -> (port.PortNumber = Some portNum && port.PortType= portType))
    |> snd

/// <summary> Returns all valid port IDs that are in the given list of symbols. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="sIdLst"> The list of symbols to find all the ports from </param>
/// <returns> The list of valid port IDs represented by ```PortId list``` </returns> 
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

/// <summary> Returns all the symbols contained in the symbol model. </summary> 
/// <param name="symModel"> The current symbol model </param>
/// <returns> The component IDs of all the symbols contained in the symbol model represented by ```ComponentId list``` </returns>
let getAllSymbols (symModel: Model) : ComponentId list =
    symModel
    |> Map.toList
    |> List.map fst

/// <summary> Returns all valid ports contained in the symbol model. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <returns> Map of all valid ```Port``` as ```Value``` and their respective ```PortId``` as ```Key``` </returns>
let allPortsInModel (symModel: Model) : Map<PortId, Port> = 
    symModel
    |> Map.fold (fun acc _ elem -> 
        Map.fold (fun acc k v ->
            Map.add k v acc
        ) acc (combinedPortsMap elem)
    ) Map.empty

/// <summary> Given a port, find its parent symbol in the symbol model. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="port"> The port of the parent symbol </param>
/// <returns> The parent symbol represented by ```Symbol``` </returns>
let findSymbolFromPort (symModel: Model) (port: Port) : Symbol =
    symModel.[port.HostId]

/// <summary> Given a targeted position, return the option of the component ID of a symbol that is in the symbol model. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="pos"> The targeted position </param>
/// <returns> The option of the component ID of the symbol represented by ```Some ComponentId``` if the targeted position is in the symbol, and ```None``` otherwise </returns>
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

/// <summary> Given an bounding box area, return a list of component IDs in the symbol model that intersects with the bounding box. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="bbox"> The bounding box area </param>
/// <returns> The list of component IDs of the symbols that intersects with the bounding box, represented by ```ComponentId List``` </returns> 
let getSymbolsInTargetArea (symModel:Model) (bbox:BBox) : ComponentId List =
    symModel
    |> Map.filter
        (fun _ (sym:Symbol) ->
            let symBBox = pointsToBBox (posOf sym.Component.X sym.Component.Y) (posOf (sym.Component.X+sym.Component.W) (sym.Component.Y+sym.Component.H))
            overlaps symBBox bbox
        )
    |> Map.toList
    |> List.map fst

/// <summary> Given a port ID in a symbol model return the entire port. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="portId"> The port ID of the port </param>
/// <returns> The port represented by ```Port``` </returns>
let findPort (symModel: Model) (portId: PortId) : Port =
    (allPortsInModel symModel).[portId]

/// <summary> Given a port ID in a symbol model return its position. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="portId"> The port ID of the port </param>
/// <returns> The position of the port represented by ```XYPos``` </returns>
let portPos (symModel: Model) (portId: PortId) : XYPos = 
    let foundPort = findPort symModel portId
    let foundSymbol = findSymbolFromPort symModel foundPort
    {
        X = foundPort.PortPos.X + foundSymbol.Component.X
        Y = foundPort.PortPos.Y + foundSymbol.Component.Y
    }

/// <summary> Given a position, return all ports in the symbol model that are within the threshold distance of the position. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="pos"> The desired position </param>
/// <param name="threshold"> The threshold distance that has to be satisfied </param>
/// <returns> The list of ports in the symbol model within the threshold distance of the position, represented by ```Port list``` </returns>
let nearbyPorts (symModel: Model) (pos: XYPos) (threshold: float) : Port list = 
    allPortsInModel symModel
    |> Map.filter
        (fun _ port ->
            portPos symModel port.PortId
            |> posDist pos <= threshold
        )
    |> Map.toList
    |> List.map snd
    |> List.sortBy
        (fun port ->
            portPos symModel port.PortId
            |> posDist pos
        )

/// <summary> Given a position return the option of the port ID of a port in the symbol model within 10 pixels from the position. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="pos"> The desired position </param>
/// <returns> The port Id of the nearest port that is within 10 pixels from the desired position, represented by ```Some PortId```, and ```None``` otherwise </returns> 
let getTargetedPort (symModel: Model) (pos: XYPos) : PortId Option =
    match nearbyPorts symModel pos 10. with
    | nearestPort::_ -> Some nearestPort.PortId
    | [] -> None

/// <summary> Given a position and a reference port type, return option of the port ID of a port in the symbol model within 10 pixels from the port, and has port type that fits the reference. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="pos"> The desired position </param>
/// <param name= "referencePortType"> The reference port type </param>
/// <returns> The port Id of the nearest port that is within 10 pixels from the desired position and has the same port type as the reference, represented by ```Some PortId```, and ```None``` otherwise </returns>
let getSpecificPort (symModel: Model) (pos: XYPos) (referencePortType: PortType) : PortId Option =
    let portList =
        nearbyPorts symModel pos 10.
        |> List.filter (fun p -> p.PortType = referencePortType)
    
    match portList with
    | nearestPort::_ -> Some nearestPort.PortId
    | [] -> None

/// <summary> Given a symbol's ID in a symbol model, return the position of its top left corner. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="sId"> The component ID of the symbol </param>
/// <returns> The top left corner of the symbol, represented by ```XYPos``` </returns>
let symbolPos (symModel: Model) (sId: ComponentId) : XYPos = 
    Map.find sId symModel
    |> (fun sym -> {X=sym.Component.X;Y=sym.Component.Y})

/// <summary> Given a port id in a symbol model, return its port type. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="portId"> The desired port's port ID </param>>
/// <returns> The desired port's port type, represented by ```PortType``` </returns>
let portType (symModel: Model) (portId: PortId) : PortType = 
    let foundPort = findPort symModel portId
    foundPort.PortType

/// <summary> Given a port id in a symbol model, return its port width. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="portId"> The desired port's port ID </param>>
/// <returns> The desired port's port width, represented by ```int option``` </returns>
let portWidth (symModel: Model) (portId: PortId) : int option = 
    let foundPort = findPort symModel portId

    match foundPort.Width with
    | PortWidth x -> Some x

/// <summary> Given the desired symbol's ID in a symbol model, return the desired symbol. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="symId"> The desired symbol's component ID </param>
/// <returns> The desired symbol, represented by ```Symbol``` </returns>
let getSymbolFromSymbolId (symModel: Model) (symId: ComponentId) : Symbol = 
    Map.find symId symModel

/// <summary> Given a port's ID in a symbol model, return the ID of its parent symbol. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="portId"> The port's ID to find it's parent symbol </param>
/// <returns> The parent symbol's component ID, represented by ```ComponentID``` </returns> 
let getHostId (symModel: Model) (portId: PortId) : ComponentId = 
    (findPort symModel portId).HostId

/// <summary> Given a symbol's ID in a symbol model, return its component type. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="compId"> The desired symbol's ID </param>
/// <returns> The desired symbol's component type, represented by ```Component Type``` </returns>
let symbolType (symModel: Model) (compId: ComponentId) : ComponentType = 
    (Map.find compId symModel).Component.Type

/// <summary> Given a symbol's ID in a symbol model, return its component label. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="compId"> The desired symbol's ID </param>
/// <returns> The desired symbol's component label, represented by ```string``` </returns>
let symbolLabel (symModel: Model) (compId: ComponentId) : string = 
    (Map.find compId symModel).Component.Label

/// <summary> Given a component and some additional width offset, calculate its bounding box. </summary>
/// <param name="allowableWidth"> The additional width offset </param>
/// <param name="comp"> The input component </param>
/// <returns> The bounding box of the input component, represented by ```BBox``` </returns> 
let componentBBox (allowableWidth : int) (comp: Component): BBox =
    {
        Pos = posAddX {X = comp.X; Y = comp.Y} -gridSize
        Width = comp.W + (gridSize * (2.+ float (allowableWidth)))
        Height = comp.H
    }

/// <summary> Given a symbol's ID in a symbol model, calculate the symbol's bounding box. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="compId"> The symbol's ID </param>
/// <returns> The symbol's bounding box, represented by ```BBox``` </returns>
let symbolBBox (symModel: Model) (compId: ComponentId) : BBox =
    let foundSymbol = 
        Map.find compId symModel
    match foundSymbol.Component.Type with
    | Not | Nand | Nor | Xnor -> componentBBox 1 foundSymbol.Component
    | _ -> componentBBox 0 foundSymbol.Component

/// <summary> Subtracts two port widths. </summary>
/// <param name="pw1"> The first port width </param>
/// <param name="pw2"> The second port width </param>
/// <returns> The resultant port width, corresponding to ```pw1 - pw2```, represented by ```PortWidth``` </returns>
let subtractPortWidth (pw1:PortWidth) (pw2:PortWidth) :PortWidth = 
    let (PortWidth w1) = pw1
    let (PortWidth w2) = pw2
    PortWidth (w1-w2)

/// <summary> Adds two port widths. </summary>
/// <param name="pw1"> The first port width </param>
/// <param name="pw2"> The second port width </param>
/// <returns> The resultant port width, corresponding to ```pw1 + pw2```, represented by ```PortWidth``` </returns>
let addPortWidth (pw1:PortWidth) (pw2:PortWidth): PortWidth =
    let (PortWidth w1) = pw1
    let (PortWidth w2) = pw2
    PortWidth (w1+w2)

/// <summary> Given a list of symbol IDs in a symbol model, check if they are overlapping with any other symbols in the symbol model. </summary>
/// <param name="idLst"> List of symbol IDs </param>
/// <param name="symModel"> The current symbol model </param>
/// <returns> ```true``` if any symbol in the list overlaps with any other symbol in the symbol model, ```false``` otherwise </returns>
let symbolsCollide (idLst : ComponentId list) (symModel : Model) : bool = 
    idLst
    |> List.collect (fun sId ->
        let symBBox = symbolBBox symModel sId
        getSymbolsInTargetArea symModel symBBox
        |> List.collect (fun matchedId -> if List.contains matchedId idLst then [] else [matchedId])
    )
    |> List.length
    |> (<) 0

/// <summary> Given a component, a new position and a new label, update that component with those new information. </summary>  
/// <param name="comp"> The desired component </param>  
/// <param name="p"> The new position </param>
/// <param name="label"> The new label </param>
/// <returns> The updated component, represented by ```Component``` </returns>
let updateCompoment (comp: Component) (p: XYPos) (label: string) : Component =
    { comp with
        Label = label
        X = p.X
        Y = p.Y
    }
/// <summary> Given a position, component type and label, create a new component with those information. </summary>
/// <param name="position"> The position of the new component </param>
/// <param name="compType"> The component type of the new component </param>
/// <param name="labelName"> The label of the new component </param>
/// <returns> The new component, represented by ```Component``` </returns>
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

/// <summary> Creates a deep copy of the component, used for copying and pasting </summary>
/// <param name="comp"> The desired component </param>
/// <returns> The deep copy of the component, represented by ```Component * Map&lt;PortId, PortId&gt;``` </returns>
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

/// <summary> Updates a component in the current symbol model. If it does not exist then insert it into the symbol model. </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="comp"> The component to be updated </param>
/// <returns> The updated symbol model, represented by ```Model``` </returns>
let updateSymbolModelWithComponent (symModel: Model) (comp: Component) : Model =
    symModel
    |> Map.add comp.Id {symModel.[comp.Id] with Component = comp}





let rng = System.Random 0
let rng2() = rng.Next(0,2)



/// <summary> Generates a random name (For Demo) </summary>
/// <returns> A randomly generated name, represented by ```string``` </returns>  
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

/// <summary> Generates a component type based on the index (For Demo) </summary>
/// <param name="compNo"> The input index </param>
/// <returns> The component type, represented by ```ComponentType``` </returns>
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

/// <summary> Creates a new symbol based on the index (For Demo) </summary>
/// <param name="index"> The input index </param>
/// <returns> The generated symbol </returns>
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
        Colliding = false
    }

/// <summary> Dummy function for the demo. The real init would probably have no symbols. </summary>
let init () =
    [1..50]
    
    |> List.map (fun x -> createNewSymbol (x))
    |> List.map (fun sym -> (sym.Id, sym))
    |> Map.ofList
    , Cmd.none


///<summary> Calculates/Resets the width inference of symbols in the symbol model joined by two ports </summary>
///<param name="symModel"> The current symbol model </param>
///<param name="pid1"> The first port </param>
///<param name="pid2"> The second port </param>
///<param name="addOrDelete"> Boolean to decide whether to calculate or reset the width inference </param>
///<returns> The symbol model containing the updated symbols, represented by ```Model``` </returns> 
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
    
/// <summary> Update function which displays symbols. </summary>
/// <param name="msg"> The message from sent from sheets </param>
/// <param name="model"> The current symbol model </param>
/// <returns> The updated symbol model, together with additional commands, represented by ```Model * Cmd &lt;'a&gt;``` </returns>
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol comp ->
        let sym = 
            {
                LastDragPos = {X=0. ; Y=0.}
                IsDragging = false
                Id = comp.Id
                Component = comp
                Shadow = true
                Colliding = false
            }
        
        let newModel = Map.add comp.Id sym model

        let sym =
            if (symbolsCollide [comp.Id] newModel) then
                {sym with Shadow = true; Colliding = true}
            else sym

        newModel
        |> Map.change comp.Id (fun _ -> Some sym)
            
        , Cmd.none

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

    | Dragging (sIdLst, pagePos, mouseIsDown) -> 
        let newModel = 
            model
            |> Map.map(fun _ sym ->
                if List.tryFind (fun sId -> sId = sym.Id) sIdLst <> None then
                    let diff = posDiff pagePos sym.LastDragPos
                    {sym with
                        Component ={sym.Component with X = sym.Component.X + diff.X; Y = sym.Component.Y + diff.Y}
                        LastDragPos = pagePos
                        Shadow = not mouseIsDown
                    }
                else
                    sym
            )

        let collision = (symbolsCollide sIdLst newModel)        
        newModel
        |> Map.map (fun _ sym ->
            if collision then 
                if List.tryFind (fun sId -> sId = sym.Id) sIdLst <> None then
                    {sym with Colliding = true; Shadow = true}
                else
                    {sym with Colliding = false}
            else 
                {sym with Colliding = false}
        ),

        Cmd.none
    
    | EndDragging ->
        model
        |> Map.map (fun _ sym ->
            if sym.IsDragging then
                { sym with IsDragging = false; Shadow = false }
            else sym
        ), Cmd.none

    | CreateInference (pid1,pid2) ->
        widthInference model pid1 pid2 true, Cmd.none

    | DeleteInference (pid1,pid2) ->
        widthInference model pid1 pid2 false, Cmd.none

//----------------------------View Function for Symbols----------------------------//

/// <summary> Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol </summary>
type private RenderSymbolProps =
    {
        Symbol : Symbol // name works for the demo!
        Selected: bool
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
        MousePos: XYPos
        PortTypeToNotHighlight: PortType option
    }

/// <summary> View for one symbol with caching for efficient execution when input does not change </summary>
let private renderSymbol =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let opacity = if props.Symbol.Shadow then "20%" else "100%"

            let fillColor =
                match props.Selected, props.Symbol.Colliding with
                | (_, true) -> "#ff0000"
                | (true, _) -> "#00d1b2"
                | _ -> "#d3d3d3"
                
                    
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

/// <summary> View function for symbol layer of SVG. </summary>
/// <param name="model"> The current symbol model </param>
/// <param name="selectedSymbols"> An option of selected symbols </param>
/// <param name="mousePos"> The given mous position </param>
/// <param name="portTypeToNotHighlight"> Indicates which ports are not supposed to be highlighted </param>
/// <param name="bbox"> The screen's bounding box </param>
/// <param name="dispatch"> Mesages that are dispached here </param>
/// <returns> A ```ReactElement``` to be rendered </returns>
let view (model : Model) (selectedSymbols: CommonTypes.ComponentId list option) (mousePos: XYPos) (portTypeToNotHighlight: PortType option) (bbox: BBox) (dispatch : Msg -> unit) = 
    let selectedSet =
        match selectedSymbols with
        | Some sIdLst -> Set.ofList sIdLst
        | None -> Set.empty

    let visibleSyms = getSymbolsInTargetArea model bbox

    let (selectedSyms, unselectedSyms) =
        model
        |> Map.filter (fun _ sym -> List.contains sym.Id visibleSyms)
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



//----------------------Interface to Issie-----------------------------//
/// <summary> Extracts the component from the symbol model using the component's ID </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="sId"> The desired component's ID </param>
/// <returns> The desired component, represented by ```Component``` </returns>
let extractComponent (symModel: Model) (sId:ComponentId) : Component= 
    (getSymbolFromSymbolId symModel sId).Component

/// <summary> Extracts all components from the symbol model</summary>
/// <param name="symModel"> The current symbol model </param>
/// <returns> A list of all components, represented by ```Component list``` </returns>
let extractComponents (symModel: Model) : Component list = 
    symModel
    |> Map.toList
    |> List.map (fun (_,sym) ->
        sym.Component
    )


// Our team has created a type PortId to represent ports. 
// Issie may not recognise this, hence it might be useful for Issie if we translate 
// PortId into (ComponentId, PortNumber, PortType) Instead
/// <summary> Translates ```PortID``` into a tuple of ```ComponentId * PortNumber * PortType``` for Issie convenience </summary>
/// <param name="symModel"> The current symbol model </param>
/// <param name="portId"> The desired port's ID </param>
/// <returns> The translated port information, represented by ```ComponentId * PortNumber * PortType``` </returns>
let translatePortToIssieFormat (symModel: Model) (portId: PortId): (ComponentId * PortNumber * PortType) =
    let foundPort = 
        portId
        |> findPort symModel
    match foundPort.PortNumber with
    | Some x ->
        foundPort.HostId,x,foundPort.PortType
    | None -> failwithf "won't happen"



