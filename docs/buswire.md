# BusWire interfaces

## Interfaces

#### Interface funtions provided by BusWire
* getTargetedWire: Model -> XYPos -> ConnectionId option
* getErrors: Model -> Error list

#### Interface functions used by BusWire (from Symbol)
* getPortsOfSymbol: Model -> ComponentId -> PortId list
* portPos: Model -> PortId -> XYPos
* portType: Model -> PortId -> PortType
* portWidth: Model -> PortId -> PortWidth
* symbolBBox: Model -> ComponentId -> BBox
* getHostId: Model -> PortId -> ComponentId
* portsInRange: Model -> XYPos -> (range : float) -> PortId list


## Messages

##### Handled by BusWire
* AddWire of PortId * PortId
* DeleteWire of ConnectionId
* SetSelected of ConnectionId
* UnselectAll
* StartDrag of ConnectionId * XYPos
* Dragging of ConnectionId * XYPos
* EndDrag
* Symbol.EndDrag
* ToggleLabels

#### Messages used by BusWire

##### Sent to Symbol
Passes all messages from Sheet to Symbol.
