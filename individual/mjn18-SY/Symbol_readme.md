#Symbol Demonstration by Marcus Neo


##Components
- Not, And, Or, Xor, Nand, Nor, Xnor
- DFF, DFFE
- MUX, DEMUX, Decode4
- NbitsAdder
- Input, Output, Constant
- RAM, ROM, AsyncROM
- Register, RegisterE
- Wire Label (IOLabel)
- MergeWires, SplitWire
- BusSelection

##Interfaces

    withinSelectedBoundary: 
        compTopLeft:XYPos -> compBotRight:XYPos -> boundTopLeft:XYPos -> boundTopRight:XYPos -> bool

    combinedPortsList:
        Symbol -> Port list

    getPortsOfSymbol: 
        Model -> ComponentId -> PortId list

    getAllSymbols:
        Model -> ComponentId list

    allPortsInModel:
        Model -> Port list

    findSymbolFromPort:
        Model -> Port -> Symbol

    getTargetedSymbol:
        Model -> XYPos -> ComponentId Option

    getTargedSymbolsInTargetArea:
        Model -> BBox -> ComponentId list

    getTargetedPort:
        Model -> XYPos -> PortId Option

    symbolPos:
        Model -> ComponentId -> XYPos

    findPort:
        Model -> PortId -> Port

    portPos:
        Model -> PortId -> XYPos

    portType:
        Model -> PortId -> PortType

    portWidth:
        Model -> PortId -> PortWidth

    getSymbolFromSymbolId:
        Model -> ComponentId -> Symbol

##Messages
    StartDragging(Dummy)
    Dragging(Dummy)
    EndDragging(Dummy)
    DeleteSymbol
    AddSymbol
    SetSelected(Dummy)
    HighlightPorts(Dummy)
    UnhighlightPorts(Dummy)

