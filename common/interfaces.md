# Interfaces

## New types
* BBox
	```
	type BBox = {
		Pos: XYPos
		Width: float
		Height: float
	}
	```
	- containsPoint: BBox -> XYPos -> bool
	- distanceFromPoint: BBox -> XYPos -> float
	- overlaps: BBox -> BBox -> bool
	- makeBBox: (pos: XYPos) -> (width: float) -> (height: float) -> BBox
* Error
 	```
	type Error = {
		Msg: string
		Pos: XYPos
	}
	```
* PortId of string

## Symbol

#### Interfaces
##### Interface funtions provided by Symbol:

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
* getHostId: Model -> PortId -> ComponenetId
* symbolType: Model -> ComponentId -> ComponentType

##### Interface funtions used by Symbol:
-

##### Messages
##### Handle by Symbol:
* AddSymbol of ComponentType * XYPos 
* DeleteSymbols of ComponentId list
* StartDraging of ComponentId list * XYPos
* Dragging of ComponentId list * XYPos
* EndDragging 
* SetSelected of ComponentId list
* HighlightPorts of PortId list
* UnhighlightPorts

### BusWire

#### Interfaces
#### Interface funtions provided by BusWire:
* getTargetedWire: Model -> XYPos -> ConnectionId Option
* getErrors: Model -> Error Option

#### Interface functions used by BusWire:
* getPortsOfSymbol: Model -> ComponentId -> PortId list
* portPos: Model -> PortId -> XYPos
* portType: Model -> PortId -> PortType
* portWidth: Model -> PortId -> PortWidth
* symbolBBox: Model -> ComponentId -> BBox
* getHostId: Model -> PortId -> ComponenetId
* portsInRange: Model -> XYPos -> (range : float) -> PortId list
	- Potentially used to manually reroute wire from one port to another (Not implemented at indivdual stage)


#### Messages
##### Handle by BusWire:
* AddWire of PortId * PortId
* DeleteWire of ConnectionId
* SetSelected of ConnectionId
* UnselectAll
* StartDrag of ConnectionId * XYPos
* Dragging of ConnectionId * XYPos
* EndDrag
* Symbol.EndDrag
##### Sent to Symbol:
Passes all messages from Sheet to Symbol.

### Sheet

#### Interfaces
##### Interface funtions provided by Sheet:
-

##### Interface funtions used by Sheet:
###### From Symbol
* getAllSymbols : Model -> ComponentId List
* getTargetedSymbol: Model -> XYPos -> ComponentId Option
* getSymbolsInTargetArea: Model -> BBox -> ComponentId List
* getTargetedPort: Model -> XYPos -> PortId Option
* getPortsOfSymbol: Model -> ComponentId -> PortId list
* portsInRange: Model -> XYPos -> (range : float) -> PortId list
* getHostId: Model -> PortId -> ComponenetId
###### From BuwsWire
* getTargetedWire: Model -> XYPos -> ConnectionId Option
* getErrors: Model -> Error Option

#### Messages
##### Message used by Sheet:
###### Sent to BuwsWire:
* AddWire of PortId * PortId
* DeleteWire of ConnectionId
* SetSelected of ConnectionId
* UnselectAll
* StartDrag of ConnectionId * XYPos
* Dragging of ConnectionId * XYPos
* EndDrag
###### Sent to Symbol:
* AddSymbol of ComponentType * XYPos 
* DeleteSymbols of ComponentId list
* StartDraging of ComponentId list * XYPos
* Dragging of ComponentId list * XYPos
* EndDragging 
* SetSelected of ComponentId list
* HighlightPorts of PortId list
* UnhighlightPorts


