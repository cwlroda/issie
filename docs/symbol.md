## Symbol interfaces

## Interfaces

#### Interface funtions provided by Symbol:

* getAllSymbols : Model -> ComponentId list
* getTargetedSymbol: Model -> XYPos -> ComponentId option
* getSymbolsInTargetArea: Model -> BBox -> ComponentId list
* getTargetedPort: Model -> XYPos -> PortId option
* getPortsOfSymbol: Model -> ComponentId -> PortId list
* portPos: Model -> PortId -> XYPos
* portType: Model -> PortId -> PortType
* portWidth: Model -> PortId -> PortWidth
* symbolBBox: Model -> ComponentId -> BBox
* portsInRange: Model -> XYPos -> (range : float) -> PortId list
* getHostId: Model -> PortId -> ComponentId
* symbolType: Model -> ComponentId -> ComponentType

#### Interface funtions used by Symbol:
-

## Messages

#### Handled by Symbol:
* AddSymbol of ComponentType * XYPos 
* DeleteSymbols of ComponentId list
* StartDraging of ComponentId list * XYPos
* Dragging of ComponentId list * XYPos
* EndDragging 
* SetSelected of ComponentId list
* HighlightPorts of PortId list
* UnhighlightPorts
