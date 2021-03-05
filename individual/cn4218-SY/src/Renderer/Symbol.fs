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
type BBox = {
    Pos : XYPos
    Width: float
    Height: float
}
type PortPos =
    {
        X: float
        Y: float
    }
type PortId = string

type PortWidth = int

type Label =
    {
        Label: string
        PPos: PortPos
        PortID: PortId
        PortType: CommonTypes.PortType
        PortWidth: PortWidth 
        highlightPort: bool

    }

type Labels = Label list 


type Symbol =  
    {         
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        Type: CommonTypes.ComponentType 
        H: float
        W: float
        Labels:Labels 
        Name:string
        Selected: bool
   
        
    }
type Model = Symbol list

type TempRecord =
    {
        Name: string
        Input: (string*float) list
        Output: (string*float) list
        Depth: (float*float)
    }

//----------------------------Message Type-----------------------------------//
type Msg =
    | MouseMsg of MouseT
    | StartDraggingSingle of sId : ComponentId * pagePos: XYPos
    | DraggingSingle of sId : ComponentId * pagePos: XYPos
    | EndDraggingSingle of sId : ComponentId
    | AddSymbol of Pos:XYPos * Comp:ComponentType
    | DeleteSymbolSingle of sId:ComponentId
    | UpdateSymbolModelWithComponent of Component //issie interface
    | DeleteSymbols of ComponentId list  
    | StartDragging of ComponentId list * XYPos
    | Dragging of ComponentId list * XYPos
    | EndDragging of ComponentId list  
    | SetSelected of ComponentId list
    | HighlightPorts of PortId list
    | UnhighlightPorts
    | DoNothing

//---------------------------------helper types and functions----------------//

let posDiff (a:XYPos) (b:XYPos):XYPos =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd (a:XYPos) (b:XYPos):XYPos =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf (x:float) (y:float):XYPos = {X=x;Y=y}

let exists id list = List.exists (fun m -> m = id) list

let coordArithmetic o (x,y) (x1,y1) = o x x1 , o y y1 

let addIt  = coordArithmetic (+) 

let toString (a:float *float) = sprintf "%A,%A" (fst a)  (snd a) 

let getXYTopL (bbox:BBox) = bbox.Pos.X,bbox.Pos.Y 

let getXYbottomR (bbox:BBox) = 
    (bbox.Pos.X + bbox.Width) , (bbox.Pos.Y + bbox.Height)

let compare c2 c1 cc2 cc1 = 
    c2 >= c1 && cc2 <= cc1

let posInBBox (bbox1:BBox) (bbox2:BBox) = //order of bbox's matter
    let (x1,y1) = getXYTopL bbox1
    let (x2,y2) = getXYTopL bbox2
    let (xx1,yy1) = getXYbottomR bbox1
    let (xx2,yy2) = getXYbottomR bbox2
    compare x2 x1 xx2 xx1 && compare y2 y1 yy2 yy1

let symBox (s:Symbol)= {Pos = s.Pos; Width= s.W ; Height = s.H}



//------------------------------Start of Interface Functions--------------------------------------------//

let getAllSymbols (symList: Model): ComponentId List = //works
    symList 
    |> List.map (fun x -> x.Id)


let getTargetedSymbol (symList: Model) (xypos: XYPos):ComponentId Option =  //works 
    let posBox = {Pos = xypos; Width= 0. ; Height = 0.}
    symList
    |> List.tryPick (fun sym -> if  posInBBox (symBox sym) posBox 
                                then Some sym.Id
                                else None )
    
let getSymbolsInTargetArea (symList:Model) (bound:BBox):ComponentId List  = //works 
    symList
    |> List.filter (fun sym -> posInBBox bound (symBox sym) ) 
    |>List.map (fun x -> x.Id)
   
//-----------------------------------For Demo------------------------------------//
let getTargetedPortDemo (symList:Model) (ppos:PortPos) = //works
    symList 
    |> List.map (fun sym ->
        sym.Labels
        |> List.map (fun l ->
            l.PortID
        )
    )
    |> List.concat
//---------------------------------------------------------------------------------------//

let getPortsOfSymbol (symList:Model) (id:ComponentId): PortId list = 
    let sym = symList |> List.find (fun x -> x.Id = id)
    let getPortList (labels:Labels) = 
        labels
        |> List.map (fun x -> x.PortID) 
    getPortList sym.Labels

let getTargetedPort (symList:Model) (ppos:PortPos) :PortId Option =  
    symList
    |> List.map (fun sym -> 
        sym.Labels
        |> List.map (fun l ->
            if l.PPos.X = ppos.X && l.PPos.Y = ppos.Y 
            then  [l.PortID]
            else []
        )
        |> List.concat
    )
    |> List.concat
    |> List.tryHead

let portPos (symList:Model) (pid:PortId):PortPos =  
    symList
    |> List.map (fun sym -> 
        sym.Labels
        |> List.map (fun label ->
            if label.PortID = pid
            then  [label.PPos]
            else []
        )
        |> List.concat
        
    )
    |> List.concat
    |> List.head
  
let portType (symList:Model) (pid:PortId): PortType = 
    symList
    |> List.map (fun sym -> 
        sym.Labels
        |> List.map (fun label ->
            if label.PortID = pid
            then  [label.PortType]
            else []
        )
        |> List.concat
    )
    |> List.concat
    |> List.head


let portWidth (symList:Model) (pid:PortId):PortWidth =  
    symList
    |> List.map (fun sym -> 
        sym.Labels
        |> List.map (fun label ->
            if label.PortID = pid
            then  [label.PortWidth]
            else []
        )
        |> List.concat
    )
    |> List.concat
    |> List.head


let symbolBBox (symList:Model) (id:ComponentId):BBox =
    let sym = symList |> List.find (fun x -> x.Id = id)
    symBox sym
   


let portsInRange (symList: Model) (ppos:PortPos) (range:float):PortId list = 
    let f c cP = c - cP
    let distance x y = sqrt ((f x ppos.X)** 2. + (f y ppos.Y)**2.)
    symList
    |> List.map (fun sym -> 
        sym.Labels
        |> List.map (fun label ->
            if   (distance label.PPos.X label.PPos.Y) <= range
            then  [label.PortID]
            else []
        )
        |> List.concat
    )
    |> List.concat
    

let getHostId (symList: Model) (pid:PortId):ComponentId =
    symList
    |> List.map (fun sym -> 
        sym.Labels
        |> List.map (fun label ->
            if label.PortID = pid
            then  [sym.Id]
            else []
        )
        |> List.concat
    )
    |> List.concat
    |> List.head

let symbolType (symList: Model ) (sId:ComponentId): ComponentType =
    let symbol =
        symList 
        |> List.filter (fun sym -> sym.Id = sId) 
        |>List.head
    symbol.Type



//------------------------------------Creating Symbol-------------------------------------------------

let names = [(And,"&");(Nand,"&");(Or,"≥1");
    (Nor,"≥1");(Xor,"=1");(Xnor,"=1")]


let nameMatch (comp:ComponentType)=
    names
    |> List.filter (fun x -> fst x = comp)
    |> List.exactlyOne
    |> snd

let template () =  
    {
        Pos = {X=0. ; Y=0.}
        LastDragPos = {X=0. ; Y=0.} 
        IsDragging = false 
        Id = CommonTypes.ComponentId (Helpers.uuid()) 
      //  Id = CommonTypes.ComponentId " "
        Type = CommonTypes.Not 
        H = 0. 
        W = 0. //change back to 50. later 
        Labels = [{Label = "";PPos= {X = 0.;Y = 0.} ; PortID = Helpers.uuid();
                   PortType = CommonTypes.PortType.Input;PortWidth = 1;highlightPort= false} ]
        Name = ""
        Selected = false 
    }



let ioLabels (f:PortType) (l:(string*float)list) (coords:XYPos) (h:float) (w:float)= 
    let z = if f = PortType.Input then coords.X else coords.X + w
    let pp = if f = PortType.Output then PortType.Output else PortType.Input
    let fstLabel() =List.item 0 (template()).Labels
    let n = List.length l
    [0..n-1]
    |> List.map (fun f -> 
        let (s,num)= List.item f l
        if s="EN" 
        then
            {fstLabel() with 
                Label = s ;
                PPos = {X = z + w*num; Y= coords.Y + h};
                
                PortType = pp;

            }
        else
            {fstLabel() with 
                Label = s ;
                PPos = {X = z; Y= coords.Y + h*num};
                
                PortType = pp;
                             
            }
        ) 

let combineList ilist olist xy d = 
    let iList = ioLabels (PortType.Input) (ilist) (xy) (fst d) (snd d) //first is height and second is width 
    let oList= ioLabels (PortType.Output) (olist) (xy) (fst d) (snd d)
    iList @ oList



let componentBuild xypos cType compname i o depth =
    {template() with 
        Pos = xypos;
        Type = cType;
        H = fst depth
        W = snd depth
        Labels = combineList i o xypos depth ;
        Name = compname;
    }

         

let newComponentMatch (compType:ComponentType) =
        match compType with 
        |Input _ ->  {Name="";Input=[]; Output=[("",0.5)] ;Depth=(30.,50.)}        
        |IOLabel -> {Name=""; Input=[("",0.)]; Output=[("",0.)]; Depth=(40.,60.)} 
        |Not _ -> {Name="1"; Input=[("",0.5)]; Output=[("",0.5)];Depth=(50.,50.)} 
        |BusSelection _->  {Name=""; Input=[("",0.5)]; Output=[];Depth=(30.,50.)} 
        |Constant _ -> {Name=""; Input=[]; Output=[("",0.5)];Depth=(20.,50.)} 
        |Output _ -> {Name=""; Input=[("",0.)]; Output=[];Depth=(30.,50.)}     
        |And |Or |Xor |Nand |Nor |Xnor -> {Name= nameMatch(compType); 
            Input=[("",0.25);("",0.75)]; Output=[("",0.5)];Depth=(50.,50.)} 
        |MergeWires -> {Name="";Input=[("",0.02);("",1.)];Output=[("",0.5)];Depth=(50.,50.)}
        |Decode4 -> {Name="decode"; Input=[("Sel",0.33);("Data",0.67)];
             Output=[("0",0.2);("1",0.4);("2",0.6);("3",0.8)];Depth=(150.,100.)} 
        |Mux2  -> {Name=""; Input=[("0",0.25);("1",0.75)];Output=[("",0.5)];Depth=(70., 40.)}  
        |Demux2 -> {Name=""; Input=[("",0.2)]; Output=[("0",0.01);("1",0.5)];Depth=(70., 40.)}  
        |NbitsAdder busW -> {Name="adder(" + string (busW-1) + ":0)"; 
            Input=[("Cin",0.25);("A",0.5);("B",0.75)]; Output=[("Sum",0.33);("Cout",0.67)];Depth=(150., 120.)}                                                                    
        |Custom _ -> {Name=""; Input=[("",0.5)]; Output=[];Depth=(30.,50.)} 
        |SplitWire _-> {Name="";Input= [("",0.5)];Output=[("",0.02);("",1.)];Depth=(50.,50.)} 
        |DFF  ->   {Name="DFF"; Input=[("D",0.5);(">clk",1.)]; Output=[("Q",0.5)];Depth=(80.,100.)}                   
        |DFFE -> {Name="DFFE"; Input=[("D",0.5);(">clk",1.);("EN",0.5)];
            Output=[("Q",0.5)];Depth=(80.,100.)} 
        |Register busW -> {Name="REG"+string busW; Input=[("data-in",0.5);(">clk",1.)]; 
            Output=[("data-out",0.5)];Depth=(110.,150.)}
        |RegisterE busW -> {Name="REG"+string busW; Input=[("data-in",0.5);(">clk",1.);("EN",0.5)];
             Output=[("data-out",0.5)];Depth=(110.,150.)} 
        |AsyncROM _ -> {Name="AsyncROM"; Input=[("addr",0.5)]; Output=[("data",0.5)];Depth=(110.,150.)} 

        |ROM _ -> {Name="ROM"; Input=[("addr",0.5);(">clk",1.)]; Output=[("data",0.5)];Depth=(110.,150.)}  
        |RAM _ -> {Name="RAM"; Input=[("addr",0.25);("data-in",0.5);("write",0.75);(">clk",1.)]; 
            Output=[("data-out",0.5)];Depth=(110.,150.)} 
    





let createNewSymbol (pos:XYPos) (comp:CommonTypes.ComponentType): Symbol=
    let {Name=name; Input=input; Output=output ;Depth=hw} = newComponentMatch (comp)
    let newSymbol () = componentBuild (pos) (comp) (name) (input) (output) (hw)
    let x,y = (newSymbol()).Pos.X , (newSymbol()).Pos.Y
    let naming = ["A";"B";"Sum";"data-in";"data-out";""]
    let updateLabels (w:int)= 
        let newL =
            (newSymbol()).Labels
            |> List.map ( fun s -> 
                {s with 
                   PortWidth = w 
                }                          
                )
        {newSymbol() with 
            Labels = newL
        }

    let forMuxDemux (ppos:PortPos)=
        {newSymbol() with 
            Labels =  {Label = "";PPos= ppos ; 
                       PortID = Helpers.uuid();
                       PortType = CommonTypes.PortType.Input;
                       PortWidth = 1;
                       highlightPort= false } ::(newSymbol().Labels)
        } 

    match (newSymbol()).Type with 
    |Input busW |Output  busW |NbitsAdder busW 
    |Register busW |RegisterE busW -> 
                                    let newLabels = 
                                        (newSymbol()).Labels
                                        |> List.map ( fun s -> 
                                            if exists s.Label naming
                                            then
                                                {s with 
                                                   PortWidth = busW 
                                                }
                                            else s
                                    )
                                    {newSymbol() with 
                                        Labels = newLabels
                                    }
    |Mux2 ->  forMuxDemux {X = x + 20.;Y = y + 60. }                 
                                
    |Demux2 -> forMuxDemux {X = x + 20.;Y = y + 40. }

    |Constant (width,value) -> updateLabels width

    |BusSelection (width,lSBit) -> updateLabels width
                                
    |AsyncROM mem|ROM mem|RAM mem -> 
                        let newLabels = 
                            (newSymbol()).Labels
                            |> List.map ( fun s -> 
                                match s.Label with
                                | "data" | "data-in"|"data-out" -> {s with PortWidth = mem.WordWidth }
                                | "addr" -> {s with PortWidth = mem.AddressWidth }
                                |_ -> s
                        )
                        {newSymbol() with 
                            Labels = newLabels
                        }                                                    
    | _ -> newSymbol()
   

//---------------------------------For Demo------------------------------------------------------//
let init () =
    let memory = {AddressWidth= 10;WordWidth= 10;Data= Map.empty}
   
    [ 
        createNewSymbol {X = 50.; Y=100.} Xor ;  
        createNewSymbol {X = 150.; Y=100.} And; 
        createNewSymbol {X = 250.; Y=100.} Mux2;  
        createNewSymbol {X = 350.; Y=120.} Demux2; 
        createNewSymbol {X = 450.; Y=130.} IOLabel; 
        createNewSymbol {X = 550.; Y=120.} (Constant (5,6)); 
        createNewSymbol {X = 650.; Y=120.} (Input 5); 
        createNewSymbol {X = 750.; Y=140.} (Output 10); 
        createNewSymbol {X = 50.; Y=200.} DFF; 
        createNewSymbol {X = 200.; Y=200.} DFFE; 
        createNewSymbol {X = 350.; Y=200.} (RegisterE 10);    
        createNewSymbol {X = 550.; Y=200.} (RegisterE 5); 
        createNewSymbol {X = 50.; Y=350.} (CommonTypes.ROM memory);
        createNewSymbol {X = 250.; Y=350.} (CommonTypes.RAM memory); 
        createNewSymbol {X = 450.; Y=350.} (CommonTypes.AsyncROM memory); 
        createNewSymbol {X = 650.; Y=350.} (NbitsAdder 7); 
        createNewSymbol {X = 820.; Y=350.} Decode4 
       
    ], Cmd.none
    

//----------------------------Update Function for Symbols----------------------------//

let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (pos ,comp)-> 
        (createNewSymbol pos comp):: model, Cmd.none
    | DeleteSymbolSingle sId -> 
        List.filter (fun sym -> sym.Id <> sId) model, Cmd.none
    | StartDraggingSingle (sId, pagePos) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                { sym with
                    LastDragPos = pagePos
                    IsDragging = true
                }
        )
        , Cmd.none

    | DraggingSingle (rank, pagePos) ->
        model
        |> List.map (fun sym ->
            if rank <> sym.Id then
                sym
            else
                let diff = posDiff pagePos sym.LastDragPos
                let newLabels = 
                    sym.Labels
                    |> List.map ( fun s -> 
                                    { s with 
                                        PPos =  {X =  s.PPos.X + (posAdd sym.Pos diff).X - sym.Pos.X ; //to make sure the ports and label move with the symbols 
                                                 Y = s.PPos.Y + (posAdd sym.Pos diff).Y - sym.Pos.Y  }
                                            

                                    }
                                )
                { sym with
                    Pos = posAdd sym.Pos diff
                    Labels = newLabels

                    LastDragPos = pagePos
                }
        )
        , Cmd.none

    | EndDraggingSingle sId ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then 
                sym
            else
                { sym with
                    IsDragging = false 
                }
        )
        , Cmd.none
//------------------------------Messages for group interface------------------------------------------------
    |DeleteSymbols sIdList ->
        List.filter (fun sym -> not(exists sym.Id sIdList)) model, Cmd.none

    |StartDragging (sIdList, pagePos) -> 
        model
        |> List.map (fun sym ->
            if not (exists sym.Id sIdList) then 
                sym
            else
                { sym with
                    LastDragPos = pagePos
                    IsDragging = true
                }
        )
        ,Cmd.none
    
    |Dragging (sIdList, pagePos) ->
        model
        |> List.map (fun sym ->
            if not (exists sym.Id sIdList) then
                sym
            else
                let diff = posDiff pagePos sym.LastDragPos
   
                let newLabels = 
                    sym.Labels
                    |> List.map ( fun s -> 
                                    { s with 
                                        PPos =  {X =  s.PPos.X + (posAdd sym.Pos diff).X - sym.Pos.X ; 
                                                 Y = s.PPos.Y + (posAdd sym.Pos diff).Y - sym.Pos.Y  }
                                           

                                    }
                                )
                { sym with
                    Pos = posAdd sym.Pos diff
                    Labels = newLabels

                    LastDragPos = pagePos
                }
        )
        , Cmd.none


    |EndDragging sIdList->
        model
        |> List.map (fun sym ->
            if not (exists sym.Id sIdList)then 
                sym
            else
                { sym with
                    IsDragging = false 
                }
        )
        , Cmd.none

    

    |SetSelected sIdList ->
        model
        |>List.map (fun  sym ->
            if exists sym.Id sIdList
            then {sym with Selected = true}
            else {sym with Selected = false}
        )
        ,Cmd.none

    |HighlightPorts pIdList ->
        model 
        |> List.map (fun sym ->
            let newLabels =
                sym.Labels
                |>List.map (fun label ->
                        if exists label.PortID pIdList
                        then {label with highlightPort = true}
                        else {label with highlightPort = false}
                )
            {sym with Labels = newLabels}
        )
        ,Cmd.none

    |UnhighlightPorts ->
        model 
        |> List.map (fun sym ->
            let newLabels =
                sym.Labels
                |>List.map (fun label ->
                        {label with highlightPort = false}
                        
                )
            {sym with Labels = newLabels}
        )
        ,Cmd.none

    | MouseMsg _ -> model, Cmd.none 

    | DoNothing ->  model, Cmd.none //This is just here for the Demo 
    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

type private RenderSymbolProps = 
    {
        Symbol : Symbol 
        Dispatch : Dispatch<Msg>
        key: string 
    }




let private textbox =
    fun (prop:RenderSymbolProps) ->
        let n = List.length (prop.Symbol.Labels) 
        
        let textlist =   
            [0..n-1]
            |> List.map (fun i -> 
                List.item i prop.Symbol.Labels 
                |> (fun x ->
                    text[  
                        X   (if x.PortType = PortType.Output then x.PPos.X  - 5. 
                            elif x.Label = ">clk" || x.Label = "EN" then x.PPos.X  
                            else  x.PPos.X + 5.)    
                        Y   (if x.Label = ">clk" ||x.Label = "EN"  
                            then x.PPos.Y - 5. 
                            else x.PPos.Y )
                        Style[ 
                                TextAnchor ( if x.PortType = CommonTypes.PortType.Input then "start" else "end" )
                                DominantBaseline "middle"
                                FontSize "15px"
                                Fill "Black"
                                UserSelect UserSelectOptions.None
                        ]
                    ][str x.Label ] 
                    )
            )
        
        let textName =
            text[   X ( prop.Symbol.Pos.X + prop.Symbol.W/ 2.)
                    Y (prop.Symbol.Pos.Y + 20.)
                    Style[          
                                    TextAnchor "middle"
                                    DominantBaseline "middle"
                                    FontSize "20px"
                                    FontWeight "Bold" 
                                    Fill "Black"
                                    UserSelect UserSelectOptions.None
                            ]
            ][str prop.Symbol.Name]
        textName::textlist  

let private renderSymbol =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->

            let symb = props.Symbol 
            let h = symb.H
            let w = symb.W 
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    DraggingSingle(symb.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )
            
            let color =
                if symb.IsDragging then
                    "lightblue"
                elif symb.Selected then 
                    "lightgreen"
                else
                    "lightgrey"
            
            
            let portCircle (coords:PortPos) (portColor:string):ReactElement = 
                circle[
                        Cx coords.X
                        Cy coords.Y
                        R 5.
                        SVGAttr.Fill  portColor
                        SVGAttr.Stroke portColor
                        SVGAttr.StrokeWidth 1
                      ][] 

            let rectangle (coords:XYPos)  (h:float) (w:float):ReactElement =
                rect[  
                    X coords.X
                    Y coords.Y
                    SVGAttr.Width w
                    SVGAttr.Height h
                    SVGAttr.Fill color
                    SVGAttr.Stroke color
                    SVGAttr.StrokeWidth 1
                    ][] 

            let polyline (points:string) =
                polyline[
                        SVGAttr.Points  points
                        SVGAttr.Fill "none" 
                        SVGAttr.Stroke "black"
                        SVGAttr.StrokeWidth 1
                        ][]

           
            let getXY =
                let p = symb.Pos
                p.X,p.Y
            
            let newList (l:(float * float ) list ) =
                l
                |> List.map (fun el ->
                    addIt getXY el
                    )

            let polyComponents (plist:(float * float ) list )  = 
                getXY::(newList plist)
                |> List.map toString
                |> String.concat " "   

            let forGates (xy:XYPos) (hh:float) (ww:float) =
                let points =
                    newList [(ww,hh/2.);(ww+30.,hh/2.);(ww+20.,hh/2.);(ww,hh/2.- 10.)]
                    |> List.map toString
                    |> String.concat " " 
                let poly = [polyline points]
                (rectangle xy hh ww)::poly
                |> ofList

            let polygon (pointList: (float * float) list) =
                polygon[
                        SVGAttr.Points   (polyComponents pointList)
                        SVGAttr.Fill color
                        SVGAttr.Stroke color
                        SVGAttr.StrokeWidth 1
                        ][]           
            let wirePolyline (pointList: (float * float) list)= 
                polyline (polyComponents pointList)

            
            let createPortCircle (symbol:Symbol) =
                symbol.Labels
                |> List.map (fun label ->
                    if label.highlightPort 
                    then portCircle label.PPos "green"
                    else portCircle label.PPos "none"
                )   
                |> ofList
                       

//-----------------------------Where the Functions above are called and the SVG ReactElements are Rendered----------------------------                
             
            g
                [ 
                    OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDraggingSingle props.Symbol.Id
                            |> props.Dispatch
                        )
                    OnMouseDown (fun ev -> 
                        // See note above re coords wrong if zoom <> 1.0
                        StartDraggingSingle (props.Symbol.Id, posOf ev.pageX ev.pageY)
                        |> props.Dispatch
                        document.addEventListener("mousemove", handleMouseMove.current)
                    )

                ]


                [   match  props.Symbol.Type with 
                    |And |Or |Xor |Decode4 |DFF |DFFE |Register _  |RegisterE _ 
                    |AsyncROM _| ROM _|RAM _|NbitsAdder _ |Custom _  -> rectangle symb.Pos h w
                    |Nand|Nor|Not|Xnor  -> forGates symb.Pos h w
                    |Input _ -> polygon [(30.,0.);(w,15.);(30.,h);(0.,h)]//[(-30.,0.);(-w,15.);(-30.,h);(0.,h)]//[(30.,0.);(w,15.);(30.,h);(0.,h)]
                    |Output _  -> polygon [(20.,-15.);(w,-15.);(w,15.);(20.,15.)]
                    |IOLabel -> polygon [(20.,-20.);(h,-20.);(w,0.);(h,20.);(20.,20.)]
                    |Constant _ -> polygon [(20.,h/2.);(w,h/2.);(20.,h/2.);(0.,h)]
                    |Mux2 -> polygon [(w,20.);(w,50.);(0.,h)]
                    |BusSelection _ -> polygon []
                    |Demux2 ->  polygon [(w,-20.);(w,50.);(0.,30.)]//[(w,-20.);(w,50.);(0.,30.)] //[(0.,h);(-w,50.);(-w,20.)] // 
                    |MergeWires -> wirePolyline []
                    |SplitWire _ -> wirePolyline []
                      
                    createPortCircle symb //creates the circles round ports 
                    
                    textbox props |> ofList // noticed that the error comes from textbox 
                    
                ]
          
    , "Symbol"
    , equalsButFunctions
    )      
            
                
                
              

           
                    
                    
/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = CommonTypes.ComponentId id} as Symbol) -> 
        renderSymbol 
            {
                Symbol = Symbol
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList


//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)

let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"

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

let extractComponents (symModel: Model) : CommonTypes.Component list =  failwithf "Not implemented"

