
# Order of Demo
keypress CtrlZ:
  Adds a symbol by sending a message to Symbol.fs
  
keypress CtrlD:
  Deletes that Symbol by sending message to Symbol.fs

keypress CtrlS:
  Select symbols within specific bounding box and all selected symbols will be highlighted 
  
Keypress CtrlShiftS:
  De-selects symbols 
  

Keypress CtrlC:
  Dehighlights 

Keypress CtrlP:
  This tests out ports related interface functions one by one and prints the information in the console to show that the functions are working
  - Gets id of all ports (with dummy function put in place only for the demo)
  - Gets single ID from the list
  - Gets the ID of the target port in a given xy position 
  - Gets port position with a given ID
  - States the type of port for specific ID
  - States the width of port for given ID
  - Using a range and an x y position, it tells you all the ports in that range 
  - Gets the component host ID associated with a particular port 
  - Gets the type of the component associated with ID 
  - Gets the bounding box of a symbol using an ID
  

Keypress CtrlShiftS

Keypress CtrlShiftS
 
 








# Intro to project 
My Symbol.fs file is divided into the following sections:
## Types (line 15):
  The main types added are the record type Label,the type TempRecord that will be used in a record deconstruction function (line 341)and 
  the changes made to Symbol to include the name of the component, the type,and a Label list that contains all the information grouped 
  per component port. In addition, new messages were added and it will be used by sheet to communicate with symbol.The DoNothing message was added for 
  demo purposes only.

## Helper functions (line 88): 
  These are some of the functions used throughout the entire project for calculating bounding boxes, differences, arithmetic etc.

## Interface (line 125): 
  All interface functions have been implemented and these are explanations for some of them.

   - getTargetedSymbol (symList: Model) (xypos: XYPos):ComponentId Option
      Takes in an XY position and is able to determine if that position lands on a specific symbol and returns the id of that 
      symbol. It uses external function like posInBox that determines if the position is within the BBox and symBox that creates a bounding 
      box from a given symbol.

   - getSymbolsInTargetArea (symList:Model) (bound:BBox):ComponentId List
       Takes in a bounding box,searches through the Symbol list and gets the Id's of all Symbols in within the BBox.

   - symbolBBox (symList:Model) (id:ComponentId):BBox
        Using the component Id of a Symbol, it gets the corresponding Symbol and returns its bounding box. 

   - portsInRange (symList: Model) (ppos:PortPos) (range:float):PortId list
        It takes the range as a radius between the point ppos and another. It then calculates the distances between ppos and other ports
        and if the distance between them is less than range, the port is considered within range and added to the list. 

   - getHostId (symList: Model) (pid:PortId):ComponentId
      This checks through Symbols to see which one contains a port with the specific portId and returns the component Id of that symbol.
      
 ## Create Symbol (line 378):
    With the componentMatch function I am able to deconstruct records into name,input,output etc and use their information to build the 
    Label (port info) for each port and the component Build function is used to build the entire record for each component. Further modifications
    are made to more specific components such as the demux and mux that have select ports in a specific position, the components who have specific busWidths
    for specific ports etc.
  
  
  ## init function for demo
  
  ## Update function that handles messages 
  
  ## RenderSymbol function:
    At the moment 22 components have been rendered and the others will be completed during the group stage.
    This function renders rectangles, polygons,polylines and circles (turn green when a message is sent to highlight ports).

  ## Changes to sheet.fs:
    I added a few key commands to sheet to test the new messages and the interface functions work. This is mostly for the demo. 

## Interface Functions 

getAllSymbols : Model -> ComponentId list
getTargetedSymbol: Model -> XYPos -> ComponentId option
getSymbolsInTargetArea: Model -> BBox -> ComponentId list
getTargetedPort: Model -> XYPos -> PortId option
getPortsOfSymbol: Model -> ComponentId -> PortId list
portPos: Model -> PortId -> XYPos
portType: Model -> PortId -> PortType
portWidth: Model -> PortId -> PortWidth
symbolBBox: Model -> ComponentId -> BBox
portsInRange: Model -> XYPos -> (range : float) -> PortId list
getHostId: Model -> PortId -> ComponentId
symbolType: Model -> ComponentId -> ComponentType


## Messages added

| DeleteSymbols of ComponentId list  
| StartDragging of ComponentId list * XYPos
| Dragging of ComponentId list * XYPos
| EndDragging of ComponentId list  
| SetSelected of ComponentId list
| HighlightPorts of PortId list
| UnhighlightPorts
| DoNothing

