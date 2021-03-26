## Symbol interfaces

## Interfaces

#### Interface funtions provided by Symbol:
* processingString: (inputString: string) -> (thresholdLength:int) -> string
* combinedPortsMap: Symbol -> Map<PortId,Port>
* findPortFromNumAndType: Symbol -> PortNumber -> PortType -> Port
* getPortsFromSymbols: Model -> ComponentId list -> PortId list
* getAllSymbols: Model -> ComponentId list
* allPortsInModel: Model -> Map<PortId, Port>
* findSymbolFromPort: Model -> Port -> Symbol
* getTargetedSymbol: Model -> XYPos -> ComponentId option
* getSymbolsInTargetArea: Model -> BBox -> ComponentId list
* findPort: Model -> PortId -> Port
* portPos: Model -> PortId -> XYPos
* nearbyPorts: Model -> XYPos -> (threshold: float) -> Port list
* getTargetedPort: Model -> XYPos -> PortId option
* getSpecificPort: Model -> XYPos -> PortType -> PortId option
* symbolPos: Model -> ComponentId -> XYPos
* portType: Model -> PortId -> PortType
* portWidth: Model -> PortId -> int option
* getSymbolFromSymbolId -> Model -> ComponentId -> Symbol
* getHostId: Model -> PortId -> ComponentId
* symbolType: Model -> ComponentId -> ComponentType
* symbolLabel: Model -> ComponentId -> string
* componentBBox: (allowableWidth: int) -> Component -> BBox
* symbolBBox: Model -> ComponentId -> BBox
* subtractPortWidth: PortWidth -> PortWidth -> PortWidth
* addPortWidth: PortWidth -> PortWidth -> PortWidth
* symbolsCollide: ComponentId list -> Model -> bool
* updateComponent: Component -> XYPos -> (label: string) -> Component
* createSpecificComponent: XYPos -> ComponentType -> (labelName: string) -> Component
* createDeepCopyOfComponent: Component -> Component * Map<PortId,PortId>
* updateSymbolModelWithComponent: Model -> Component -> Model

#### Interface funtions used by Symbol:
-

## Messages

#### Handled by Symbol:
* AddSymbol of Component 
* DeleteSymbols of ComponentId list
* StartDraging of ComponentId list * XYPos
* Dragging of ComponentId list * XYPos * (mouseisDown: bool)
* EndDragging 
* UpdateSymbolModelWithComponent of Component
* CreateInference of (PortId*PortId)
* DeleteInference of (PortId*PortId)