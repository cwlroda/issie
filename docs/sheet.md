# Sheet interface

## Interfaces

#### Interface funtions provided by Sheet:
-

#### Interface funtions used by Sheet

##### From Symbol
* getAllSymbols : Model -> ComponentId List
* getTargetedSymbol: Model -> XYPos -> ComponentId Option
* getSymbolsInTargetArea: Model -> BBox -> ComponentId List
* getTargetedPort: Model -> XYPos -> PortId Option
* getPortsOfSymbol: Model -> ComponentId -> PortId list
* portsInRange: Model -> XYPos -> (range : float) -> PortId list
* getHostId: Model -> PortId -> ComponentId
* portPos: Model -> PortId -> XYPos
* symbolBBox: Model -> ComponentId -> BBox
* symbolType: Model -> ComponentId -> ComponentType

##### From BusWire
* getTargetedWire: Model -> XYPos -> ConnectionId Option
* getErrors: Model -> Error list

## Messages
#### Messages provided by Sheet:
-

#### Messages used by Sheet

##### Sent to BusWire
* AddWire of PortId * PortId
* DeleteWire of ConnectionId
* SetSelected of ConnectionId
* UnselectAll
* StartDrag of ConnectionId * XYPos
* Dragging of ConnectionId * XYPos
* EndDrag

##### Sent to Symbol
* AddSymbol of ComponentType * XYPos 
* DeleteSymbols of ComponentId list
* StartDraging of ComponentId list * XYPos
* Dragging of ComponentId list * XYPos
* EndDragging 
* SetSelected of ComponentId list
* HighlightPorts of PortId list
* UnhighlightPorts


