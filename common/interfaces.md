# Interfaces

## New types
* BBox
	```
	type BBox = {
		Pos : XYPos
		Width: float
		Height: float
	}
	```
	- containsPoint: BBox -> XYPos -> bool
	- distanceFromPoint: BBox -> XYPos -> float
	- overlaps: BBox -> BBox -> bool
	- makeBBox: (pos : XYPos) (width : float) (height : float)
* PortId of string

### Symbol

##### Interfaces
* getAllSymbols : Model -> ComponentId List
* getTargetedSymbol: Model -> XYPos -> ComponentId Option
* getSymbolsInTargetArea: Model -> BBox -> ComponentId List
* getTargetedPort: Model -> XYPos -> PortId Option
* getPortsOfSymbol: Model -> ComponentId -> PortId list
* portPos: Model -> PortId -> XYPos
* portType: Model -> PortId -> PortType
* portWidth: Model -> PortId -> PortWidth
* symbolBBox: Model -> ComponentId -> BBox
* portsInRange: Model -> XYPos -> (range : float) -> PortId list

##### Messages

* AddSymbol of ComponentType * XYPos 
* DeleteSymbols of ComponentId list
* StartDraging of ComponentId list * XYPos
* Dragging of ComponentId list * XYPos
* EndDragging 
* SetSelected of ComponentId list
* HighlightPorts of PortId list
* UnhighlightPorts

### BusWire

##### Interfaces
* getTargetedWire: Model -> XYPos -> ConnectionId Option	

##### Messages
* AddWire of PortId * PortId
* DeleteWire of ConnectionId
* SetSelected of ConnectionId
* UnselectAll
* StartDrag of ConnectionId * XYPos
* Dragging of ConnectionId * XYPos
* EndDrag
