# Interfaces

## New types
* BBox
	- containsPoint: BBox -> XYPos -> bool
	- distanceFromPoint: BBox -> XYPos -> float
	- overlaps: BBox -> BBox -> bool
* PortId

### Symbol

##### Interfaces
* getAllSymbols : Model -> ComponentId List
* getTargetedSymbol: Model -> XYPos -> ComponentId Option
* getSymbolsInTargetArea: Model -> BBox -> ComponentId List
* getTargetedPort: Model -> XYPos -> PortId Option
* portPos: symModel -> PortId -> XYPos
* portType: Model -> PortId -> PortType
* portWidth: Model -> PortId -> PortWidth

##### Messages

* AddSymbol of (comp: CommonTypes.ComponentType, pos: XYPos) 
* StartDraging of (sIdLst: CommonTypes.ComponentId list) (pagePos: XYPos)
* Dragging of (sIdLst: CommonTypes.ComponentId list) (pagePos: XYPos)
* EndDragging 
* SetSelected of (sIdLst: CommonTypes.ComponentId list)
* HighlightPort of (pId: PortId)
* UnhighlightPorts

### BusWire

##### Interfaces
* getTargetedWire: Model -> XYPos -> CommonTypes.ConnectionId Option	

##### Messages
* AddWire of (portA: PortId) * (portB: PortId)
* DeleteWire of (wireId: CommonTypes.ConnectionId)
* SetSelected of (wireId: CommonTypes.ConnectionId)
* UnselectAll
* StartDrag of (wireId: CommonTypes.ConnectionId) (pos: XYPos)
* Dragging of (wireId: CommonTypes.ConnectionId) (pos: XYPos)
* EndDrag
