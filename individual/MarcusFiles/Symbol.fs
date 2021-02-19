module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers

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
        Id : CommonTypes.ComponentId
        Component : CommonTypes.Component
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
    | StartDragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    | Dragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | EndDragging of sId : CommonTypes.ComponentId
    | AddSymbol of comp:CommonTypes.Component*pos:XYPos // used by demo code to add a circle
    | DeleteSymbol of sId:CommonTypes.ComponentId 
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface
    | SetSelected of topLeft:XYPos * bottomRight:XYPos // 
    | MouseOverPort of port : CommonTypes.Port
    | MouseOutPort of port : CommonTypes.Port

//---------------------------------helper types and functions----------------//



let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let withinSelectedBoundary (compTopLeft:XYPos) (compBotRight:XYPos) (boundTopLeft:XYPos) (boundBotRight:XYPos) :bool =
    match compTopLeft,compBotRight with
        | point1,point2 when (point1.X >= boundTopLeft.X) && (point2.X <= boundBotRight.X) && (point1.Y >= boundTopLeft.Y) && (point2.Y <= boundBotRight.Y) -> true
        | _ -> false


// let getPortPosition (port:CommonTypes.Port) (model:Model) : XYPos =
//     let parentSymbol = 
//         List.filter (fun x->x.Id = port.HostId) model
//         |>List.head
//     match port.PortType with
//     |CommonTypes.PortType.Input -> 
    
//-----------------------------Skeleton Model Type for symbols----------------//




//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let rng = System.Random 0
let createNewSymbol () (comp:CommonTypes.Component)  =
    
    {
        LastDragPos = {X=0. ; Y=0.} // initial value can always be this
        IsDragging = false // initial value can always be this
        Id = CommonTypes.ComponentId (Helpers.uuid()) // create a unique id for this symbol
        Component = comp
        Selected = 
            match rng.Next(0,2) with
            | 0 -> false
            | 1 -> true
            | _ -> failwithf "not implemented"
    }

let testComponent () :CommonTypes.Component =
        
        let compId = CommonTypes.ComponentId (Helpers.uuid())
        let rng1 () = rng.Next(0,10)
        let inputPorts :CommonTypes.Port list = List.map (fun x-> {PortId=CommonTypes.PortId (Helpers.uuid()); PortNumber = Some (CommonTypes.PortNumber x); PortType = CommonTypes.PortType.Input;HostId = compId;Hover = CommonTypes.PortHover false}) [0..rng1()]
        let outputPorts:CommonTypes.Port list = List.map (fun x-> {PortId=CommonTypes.PortId (Helpers.uuid()); PortNumber = Some (CommonTypes.PortNumber x); PortType = CommonTypes.PortType.Output;HostId = compId;Hover = CommonTypes.PortHover false}) [0..rng1()]
        // let inputPorts =  inputPortsFn ()
        // let outputPorts = outputPortsFn ()
        // let inputPorts:CommonTypes.Port list = [{PortId=CommonTypes.PortId (Helpers.uuid()); PortNumber = Some (CommonTypes.PortNumber 0); PortType = CommonTypes.PortType.Input;HostId = compId}; {PortId= CommonTypes.PortId (Helpers.uuid()); PortNumber = Some (CommonTypes.PortNumber 1); PortType = CommonTypes.PortType.Input;HostId = compId}]
        // let outputPorts:CommonTypes.Port list = [{PortId=CommonTypes.PortId (Helpers.uuid()); PortNumber = Some (CommonTypes.PortNumber 0); PortType = CommonTypes.PortType.Output;HostId = compId}]
        {
            Id = compId
            Type = CommonTypes.ComponentType.Or
            Label = "Hello"
            InputPorts = inputPorts
            OutputPorts = outputPorts
            X = 20.
            Y = 30.
            H = 25. +  float (max (List.length inputPorts) (List.length outputPorts)) * 15. + 5.
            W = 50.
        }
/// Dummy function for test. The real init would probably have no symbols.
let init () =
    
    let createTestList (testComponent :CommonTypes.Component) (xIn,yIn) =
    
        {testComponent with X = testComponent.X + xIn; Y=testComponent.Y+yIn}

    // List.allPairs [100.;200.;300.] [40.; 150.;300.]
    // |> List.map (createTestList testComponent)
    // |> List.map (createNewSymbol (rng.Next(2)))
    //Factorised to the below expression
    
    List.map ((createTestList (testComponent ())) >> createNewSymbol ()) (List.allPairs [100.;200.;300.] [40.; 150.;300.])
    // List.allPairs [12..14] [3]
    // |> List.map (fun (x,y) -> {X = float (x*64+30); Y=float (y*64+30)})
    // |> List.map (createNewSymbol testComponent) 
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

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (comp,pos)-> 
        (createNewSymbol ()  comp) :: model, Cmd.none
    | DeleteSymbol sId -> 
        List.filter (fun sym -> sym.Id <> sId) model, Cmd.none
    | SetSelected (topLeft, topRight) ->
        (setSelectedFunction (topLeft, topRight) model), Cmd.none
    | StartDragging (sId, pagePos) ->
        let sIdSymbol:Symbol = 
            (List.filter (fun x -> x.Id = sId) model)
            |>List.head
        let startDrag =
            List.map(fun sym ->
                    if sym.Selected then
                        {sym with 
                            LastDragPos = pagePos
                            IsDragging = true
                        }
                    else sym
                )
        (match sIdSymbol with
        |x when not x.Selected ->  
                setSelectedFunction ({X=x.Component.X; Y=x.Component.Y}, {X=x.Component.X + x.Component.W; Y = x.Component.Y + x.Component.H}) model
                    |> startDrag
        |_-> 
            model
                |> startDrag

        ), Cmd.none
        

    | Dragging (rank, pagePos) ->
        model 
        |> List.map (fun sym ->
            if sym.Selected then
                let diff = posDiff pagePos sym.LastDragPos
                { sym with
                    // Pos = posAdd sym.Pos diff
                    Component = {sym.Component with X = sym.Component.X + diff.X ; Y = sym.Component.Y + diff.Y}
                    LastDragPos = pagePos
                }
                
            else
                sym
            ), Cmd.none

    | EndDragging sId ->
        model 
        |> List.map (fun sym ->
            if sym.Selected then
                {sym with 
                    IsDragging = false
                }
            else
                sym
            ), Cmd.none
    | MouseOutPort port ->
        let portType = port.PortType
        model
        |> List.map (fun sym ->
            if sym.Component.Id = port.HostId then
                match portType with
                | CommonTypes.PortType.Input ->
                    {sym with
                        Component = 
                            {sym.Component with 
                                InputPorts =
                                    sym.Component.InputPorts
                                        |>List.map (fun checkPort ->
                                            {checkPort with
                                                Hover = CommonTypes.PortHover false
                                            }
                                        )
                                              
                            }
                    }
                |_ ->
                    {sym with
                        Component = 
                            {sym.Component with 
                                OutputPorts =
                                    sym.Component.OutputPorts
                                        |>List.map (fun checkPort ->
                                            {checkPort with
                                                Hover = CommonTypes.PortHover false
                                            }
                                        )       
                            }
                    }
                    

            else
                sym
            ),Cmd.none
    | MouseOverPort port -> 
        let portType = port.PortType
        
        model
        |> List.map (fun sym ->
            if sym.Component.Id = port.HostId then
                match portType with
                | CommonTypes.PortType.Input ->
                    {sym with
                        Component = 
                            {sym.Component with 
                                InputPorts =
                                    sym.Component.InputPorts
                                        |>List.map (fun checkPort ->
                                            if checkPort = port then
                                                {checkPort with
                                                    Hover = CommonTypes.PortHover true
                                                }
                                            else 
                                                {checkPort with
                                                    Hover = CommonTypes.PortHover false
                                                }
                                        )       
                            }
                    }
                |_ ->
                    {sym with
                        Component = 
                            {sym.Component with 
                                OutputPorts =
                                    sym.Component.OutputPorts
                                        |>List.map (fun checkPort ->
                                            if checkPort = port then
                                                {checkPort with
                                                    Hover = CommonTypes.PortHover true
                                                }
                                            else 
                                                {checkPort with
                                                    Hover = CommonTypes.PortHover false
                                                }
                                        )       
                            }
                    }
                    

            else
                sym        
        
        ), Cmd.none
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags
    | _ -> failwithf "Not implemented"

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
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Symbol.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )

            let color =
                if props.Symbol.IsDragging then
                    "green"
                else
                    "grey"
            let topLeft:XYPos = {X=props.Symbol.Component.X;Y=props.Symbol.Component.Y}
            let topRight:XYPos = {X=props.Symbol.Component.X+props.Symbol.Component.W; Y=props.Symbol.Component.Y}
            let bottomRight:XYPos = {X=props.Symbol.Component.X+props.Symbol.Component.W; Y=props.Symbol.Component.Y+props.Symbol.Component.H}
            let bottomLeft:XYPos = {X=props.Symbol.Component.X; Y=props.Symbol.Component.Y+props.Symbol.Component.H}
            
            let componentName =
                match props.Symbol.Component.Type with
                |CommonTypes.ComponentType.And | CommonTypes.ComponentType.Nand  -> "&"
                |CommonTypes.ComponentType.Not-> "1"
                |CommonTypes.ComponentType.Or | CommonTypes.ComponentType.Nor-> "≥"
                |CommonTypes.ComponentType.Xor | CommonTypes.ComponentType.Xnor -> "=1"
                |_ -> failwithf "Not implemented yet"
            
            let invertedOutput = 
                match props.Symbol.Component.Type with
                |CommonTypes.ComponentType.Nor| CommonTypes.ComponentType.Nand | CommonTypes.ComponentType.Not | CommonTypes.ComponentType.Xnor -> true
                |_ -> false
            
            let inputPorts = props.Symbol.Component.InputPorts
            let outputPorts = props.Symbol.Component.OutputPorts
            
            let viewPorts = 
                let generateLines (portList:CommonTypes.Port list) = 
                    List.map(fun (x:CommonTypes.Port) ->
                    // let separation = (props.Symbol.Component.H - 30.)/float (List.length nList)
                    let dynamicContent = 
                        match x.PortType with
                        | CommonTypes.PortType.Input -> (topLeft.X + 2., "left")
                        | CommonTypes.PortType.Output -> (topRight.X - 7., "right")
                    let (CommonTypes.PortNumber portNumber) =    
                        match x.PortNumber with
                        |Some a -> a
                        |None -> CommonTypes.PortNumber 0

                    text[
                        OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDragging props.Symbol.Id
                            |> props.Dispatch
                        )
                        OnMouseDown (fun ev -> 
                            // See note above re coords wrong if zoom <> 1.0
                            // StartDragging (props.Symbol.Id, posOf ev.pageX ev.pageY)
                            let multipleSelection = List.fold (fun acc elem -> if elem.Selected then acc+1 else acc) 0 model
                            if multipleSelection <= 1 then
                                SetSelected (topLeft, bottomRight)
                                |> props.Dispatch
                            StartDragging (props.Symbol.Id, posOf ev.pageX ev.pageY)
                            |> props.Dispatch
                            document.addEventListener("mousemove", handleMouseMove.current)
                        )
                        X (fst dynamicContent)
                        Y (topRight.Y + 25. + 5. + ((float (portNumber)) * 15.))
                        Style [
                            
                            TextAnchor (snd dynamicContent) // left/right/middle: horizontal algnment vs (X,Y)
                            DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                            FontSize "10px"
                            FontWeight "Bold"
                            Fill "Blue" // demo font color
                            UserSelect UserSelectOptions.None
                        ]
                    ] [str <| sprintf "%i" (portNumber)]) portList
                (generateLines inputPorts) @ (generateLines outputPorts) 
            let viewPortLines = 
                
                let generateLines (portList:CommonTypes.Port list) =
                    List.map(fun (x:CommonTypes.Port) ->
                    let lineLength = 5.
                    let dynamicContent =
                        match x.PortType with
                        | CommonTypes.PortType.Input -> topLeft,topLeft.X-lineLength
                        | CommonTypes.PortType.Output -> topRight,topRight.X + lineLength
                    let (CommonTypes.PortNumber portNumber) =    
                        match x.PortNumber with
                        |Some a -> a
                        |None -> CommonTypes.PortNumber 0
                    line [
                        OnMouseOver (fun ev -> 
                            MouseOverPort x
                            |> props.Dispatch
                        )
                        OnMouseOut (fun ev -> 
                            MouseOutPort x
                            |> props.Dispatch
                        )
                        X1 (fst dynamicContent).X
                        X2 ((snd dynamicContent))
                        Y2 ((fst dynamicContent).Y + 35.+ float portNumber*15.)
                        Y1 ((fst dynamicContent).Y + 35.+ float portNumber*15.)
                        
                        match x.Hover with
                        |CommonTypes.PortHover false ->
                            SVGAttr.Fill color
                            SVGAttr.Stroke color
                            SVGAttr.StrokeWidth 4
                        |_ -> 
                            SVGAttr.Fill "red"
                            SVGAttr.Stroke "red"
                            SVGAttr.StrokeWidth 8
                    ] []) portList
                (generateLines inputPorts) @ (generateLines outputPorts)
            
                
            let viewBox =
                [polygon
                    [ 
                        OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDragging props.Symbol.Id
                            |> props.Dispatch
                        )
                        OnMouseDown (fun ev -> 
                            // See note above re coords wrong if zoom <> 1.0
                            // StartDragging (props.Symbol.Id, posOf ev.pageX ev.pageY)
                            let multipleSelection = List.fold (fun acc elem -> if elem.Selected then acc+1 else acc) 0 model
                            if multipleSelection <= 1 then
                                SetSelected (topLeft, bottomRight)
                                |> props.Dispatch
                            StartDragging (props.Symbol.Id, posOf ev.pageX ev.pageY)
                            |> props.Dispatch
                            document.addEventListener("mousemove", handleMouseMove.current)
                        )
                        // OnMouseOver ()
                        // OnClick (fun ev ->
                        //     SetSelected (topLeft, bottomRight)
                        //     |>props.Dispatch

                        // )
                        SVGAttr.Points (sprintf "%f %f, %f %f, %f %f , %f %f" topLeft.X  topLeft.Y topRight.X topRight.Y bottomRight.X bottomRight.Y bottomLeft.X bottomLeft.Y)
                        // Cx props.Symbol.Pos.X
                        // Cy props.Circle.Pos.Y
                        // R 20.
                        SVGAttr.Fill color
                        SVGAttr.Stroke color
                        SVGAttr.StrokeWidth 2
                    ] [];
                
                text [
                        OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDragging props.Symbol.Id
                            |> props.Dispatch
                        )
                        OnMouseDown (fun ev -> 
                            // See note above re coords wrong if zoom <> 1.0
                            // StartDragging (props.Symbol.Id, posOf ev.pageX ev.pageY)
                            let multipleSelection = List.fold (fun acc elem -> if elem.Selected then acc+1 else acc) 0 model
                            if multipleSelection <= 1 then
                                SetSelected (topLeft, bottomRight)
                                |> props.Dispatch
                            StartDragging (props.Symbol.Id, posOf ev.pageX ev.pageY)
                            |> props.Dispatch
                            document.addEventListener("mousemove", handleMouseMove.current)
                        )
                        X ( topLeft.X + 0.5*(topRight.X - topLeft.X)); 
                        Y (topLeft.Y + 3.); 
                        Style [
                            TextAnchor "middle" 
                            UserSelect UserSelectOptions.None
                        ]
                    // ] [str <| componentName];
                    ] [str <| string props.Symbol.Selected];
                
                text [
                        OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDragging props.Symbol.Id
                            |> props.Dispatch
                        )
                        OnMouseDown (fun ev -> 
                            // See note above re coords wrong if zoom <> 1.0
                            // StartDragging (props.Symbol.Id, posOf ev.pageX ev.pageY)
                            let multipleSelection = List.fold (fun acc elem -> if elem.Selected then acc+1 else acc) 0 model
                            if multipleSelection <= 1 then
                                SetSelected (topLeft, bottomRight)
                                |> props.Dispatch
                            StartDragging (props.Symbol.Id, posOf ev.pageX ev.pageY)
                            |> props.Dispatch
                            document.addEventListener("mousemove", handleMouseMove.current)
                        )
                        X ( topLeft.X + 2.); 
                        Y (topLeft.Y + 15.); 
                        Style [
                            TextAnchor "left" 
                            UserSelect UserSelectOptions.None
                        ]
                    ] [str <| sprintf "IN"];
                
                text [
                        OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDragging props.Symbol.Id
                            |> props.Dispatch
                        )
                        OnMouseDown (fun ev -> 
                            // See note above re coords wrong if zoom <> 1.0
                            // StartDragging (props.Symbol.Id, posOf ev.pageX ev.pageY)
                            let multipleSelection = List.fold (fun acc elem -> if elem.Selected then acc+1 else acc) 0 model
                            if multipleSelection <= 1 then
                                SetSelected (topLeft, bottomRight)
                                |> props.Dispatch
                            StartDragging (props.Symbol.Id, posOf ev.pageX ev.pageY)
                            |> props.Dispatch
                            document.addEventListener("mousemove", handleMouseMove.current)
                        )
                        X ( topRight.X - 11.); 
                        Y (topLeft.Y + 15.); 
                        Style [
                            TextAnchor "middle" 
                            UserSelect UserSelectOptions.None
                        ]
                    ] [str <| sprintf "OUT"]
                ]
            
            //let viewOverall = viewBox @ (viewPorts output outputPortList) @ (viewPorts input inputPortList) @ (viewPortLines output outputPortList) @ (viewPortLines input inputPortList)
            let viewOverall = viewBox @ viewPorts @ viewPortLines
                // List.append (viewPorts "output" [0.. (List.length(props.Symbol.Component.OutputPorts)-1)]) (viewPorts "in" [0.. (List.length(props.Symbol.Component.InputPorts)-1)]) 
                // |> List.append viewBox
            g [Style [
                            // TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                            DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                            FontSize "10px"
                            FontWeight "Bold"
                            Fill "Blue" // demo font color
                        ]] viewOverall

                // [polygon
                //     [ 
                //         OnMouseUp (fun ev -> 
                //             document.removeEventListener("mousemove", handleMouseMove.current)
                //             EndDragging props.Symbol.Id
                //             |> props.Dispatch
                //         )
                //         OnMouseDown (fun ev -> 
                //             // See note above re coords wrong if zoom <> 1.0
                //             StartDragging (props.Symbol.Id, posOf ev.pageX ev.pageY)
                //             |> props.Dispatch
                //             document.addEventListener("mousemove", handleMouseMove.current)
                //         )
                //         SVGAttr.Points (sprintf "%f %f, %f %f, %f %f , %f %f" topLeft.X  topLeft.Y topRight.X topRight.Y bottomRight.X bottomRight.Y bottomLeft.X bottomLeft.Y)
                //         // Cx props.Symbol.Pos.X
                //         // Cy props.Circle.Pos.Y
                //         // R 20.
                //         SVGAttr.Fill color
                //         SVGAttr.Stroke color
                //         SVGAttr.StrokeWidth 2
                //     ] [];
                // text [X ( topLeft.X + 0.5*(topRight.X - topLeft.X)); 
                //         Y (props.Symbol.Component.Y + 10.); 
                //         Style [
                //             TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                //             DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                //             FontSize "10px"
                //             FontWeight "Bold"
                //             Fill "Blue" // demo font color
                //         ]
                //     ] [str <| componentName]
                // ]
                
            
        
    , "Component"
    , equalsButFunctions
    )

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = CommonTypes.ComponentId id} as symbol) ->
        renderSymbol model
            {
                Symbol = symbol
                Dispatch = dispatch
                Key = id
            }
    )
    |> ofList


//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> {X=sym.Component.X;Y=sym.Component.Y})

  
let portPos (symModel: Model) (portId: CommonTypes.PortId) (sId: CommonTypes.ComponentId) : XYPos = 
    let foundSymbol = List.find (fun sym -> sym.Id = sId) symModel
    let checkInput = Option.toList (List.tryFind (fun (x:CommonTypes.Port)-> x.PortId = portId) (foundSymbol.Component.InputPorts))
    let checkOutput = Option.toList (List.tryFind (fun (x:CommonTypes.Port)-> x.PortId = portId) (foundSymbol.Component.OutputPorts))
    let foundPort = List.head (checkInput @ checkOutput)
    let (CommonTypes.PortNumber portNumber) = 
        match foundPort.PortNumber with
        |Some a -> a
        |_ -> CommonTypes.PortNumber 0
    match (checkInput,checkOutput) with
    |([],[x]) -> {X=foundSymbol.Component.X+foundSymbol.Component.W;Y=foundSymbol.Component.Y+25.+ 5. + 15.* float portNumber}
    |([x],[]) -> {X=foundSymbol.Component.X;Y=foundSymbol.Component.Y+25.+ 5. + 15.* float portNumber}
    |_ -> failwithf "Cant happen"
    


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
