# Preface
All the files in this directory were made by me, without consulting other's code. This was done to make sure that I could understand how implementing the interfaces could be done, and thereby ensure that we did not agree on interfaces that were unnecessarily complicated or impossible, or that we agreed on interfaces that wouldn't allow for the functionality we required.

# Features

## Implemented
* Panning and zooming
* Single and multiple select of symbols
    * Click and area select
    * Ctrl for additive select
* Dragging symbols and wire
* Snap to grid and gridlines
* Error popup on erroneous wires
* Deletion of symbols and wire
* Copy and paste of symbols
* Undo and Redo of movement and deletion/creation

## Future features
* Preview of Symbol placing
    * Blocked on: Not sure how Sheet is supposed to know what symbol you want to place
* Scroll with mouse wheel
    * Blocked on: Can't receive scroll events on an unscrollable div
* Change key combinations from Alt+{A,C,V,Z} (and more) to Ctrl+{A,C,V,Z}
    * Blocked on: Electron hooks into a lot of Ctrl key combinations, so these are not available
* Display error mesages with wrapping and rectangle properly fit around the text
    * Blocked on: Can't get the size of an svg text element, and rectangles don't automatically fit to encompass text in svgs.
* Maybe save selection state for undo/redo
    * Blocked on: Not sure if that's what a user expects or not, can easily be changed.
* Convert some constants to Model parameters
	* Blocked on: We haven't yet learnt how to interface with Issie, so I've left this for the team phase

# Interfaces Used 

### Symbol

###### Interfaces
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

###### Messages
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
* getErrors: Model -> Error Option

###### Messages
* AddWire of PortId * PortId
* DeleteWire of ConnectionId
* SetSelected of ConnectionId
* UnselectAll
* StartDrag of ConnectionId * XYPos
* Dragging of ConnectionId * XYPos
* EndDrag

### Sheet
All messages in sheet are currently internal

Interfaces and messages to communicate with Issie will be left for the group phase.
