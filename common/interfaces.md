# Interfaces

## New types
* BBox
	- containsPoint: BBox -> XYPos -> bool
	- distanceFromPoint: BBox -> XYPos -> float
	- overlaps: BBox -> BBox -> bool
* PortId

### Symbol

##### Interfaces
* getTargetedSymbol (symModel: Model) (pos: XYPos) : CompenentId  Option
* getSymbolsInTargetArea (symModel: Model) (bbox: BBox) : ComponentId List
* getTargetedPort (symModel: Model) (pos: XYPos) : PortId Option
* portPos (symModel: Model) (portId: PortId)  : XYPos
* portType (symModel: Model) (portId: PortId) : PortType
* portWidth (symModel: Model) (portId: PorttId) : PortWidth

##### Messages

* AddSymbol of (comp:CommonTypes.ComponentId, pos:XYPos) 
* StartDraging  (sIdLst:CommonTypes.ComponentId list) (pagePos:XYPos)
* Dragging (sIdLst:CommonTypes.ComponentId list) (pagePos:XYPos)
* EndDragging 
* SetSelected (sIdLst: CommonTypes.ComponentId list)
* HighlightPort (pId: PortId)
* UnhighlightPorts

### BusWire

##### Interfaces
* getTargetedWire (wModel:Model) (pos: XYPOs) :  CommonTypes.ConnectedId Option	
* getWireInTargetArea (wModel: Model) (bbox: BBox): CommonTypes.ConnectionId list

##### Messages
* AddWire (portA: PortId) * (portB: PortId)
* DeleteWire (wireId: CommonTypes.ConnectionId)
* SetSelected (wireId: CommonTypes.ConnectionId)
* UnselectAll
* StartDrag (wireId: CommonTypes.ConnectionId) (pos: XYPos)
* Dragging (wireId: CommonTypes.ConnectionId) (pos: XYPos)
* EndDrag
