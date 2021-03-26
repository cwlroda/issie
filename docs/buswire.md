# BusWire interfaces

## Interfaces

#### Interface funtions provided by BusWire
* getTargetedWire: Model -> XYPos -> ConnectionId option
* getErrors: Model -> Error list
* findWire: Model -> WireId -> wire
* getAllPidEnds: Model -> PortId -> PortId list
* getWiresOfSymbols: Model -> Symbol.Model -> ComponentId list -> Map < ConnectionId, Wire>
* getConnectedWires: Model -> Symbol.Model -> ComponentId list -> Map <ConnectionId,Wire>
#### Interface functions used by BusWire (from Symbol)
* getPortsFromSymbols: Symbol.Model -> ComponentId list -> PortId list CHECK
* findPort: Symbol.Model -> Symbol.PortId -> Port
* findSymbolFromPort: Symbol.Model -> Port -> Symbol
* portPos:Symbol.Model -> PortId -> XYPos
* portType: Symbol.Model -> PortId -> PortType
* portWidth: Symbol.Model -> PortId -> PortWidth
* symbolBBox: Symbol.Model -> ComponentId -> BBox
* getHostId: Symbol.Model -> PortId -> ComponentId
* getAllSymbols: Symbol.Model -> ComponentId list
* getSymbolFromSymbolId: Symbol.Model -> ComponentId -> Symbol
* getTargetedPort: Symbol.Model -> XYPOs -> PortId Option
* allPortsInModel: Symbol.Model -> Map < PortId, Port>



## Messages

##### Handled by BusWire
* DeleteSymbols of CommonTypes.ComponentId list
* DraggingSymbols of CommonTypes.ComponentId list * BBox    
*  AddWire of (PortId * PortId)                              
* DeleteWire of ConnectionId                                
* StartDrag of wId: ConnectionId * pos: XYPos               
* Dragging of wId: ConnectionId * pos: XYPos                
* EndDrag                                                   
* RoutingUpdate of BBox                                     
* Debug


#### Messages used by BusWire

##### Sent to Symbol
Passes all messages from Sheet to Symbol.
