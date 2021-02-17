# Interfaces

## Common
* BBox
	- XYPos, Width, Height
	- containsPoint
		- Takes: XYPos
		- Returns: boolean
	- distanceFromPoint
		- Takes: XYPos
		- Returns: float

### Symbol
* GetTargetedSymbol (Msg)
	- Takes: XYPos
	- Returns: Symbol option

* GetPortPosition (Msg)
	- Takes: Port
	- Return: XYPos option

* CreateSymbolOfType (Msg)
	- Takes: Component

* Dragging (Msg)
	* StartDrag
		- Takes: XYPos
	* Dragging
		- Takes: XYPos
	* EndDrag
		- Takes: ()

* SetSelected (Msg)
	- Takes: Symbol boolean

* HighlightPorts (Msg)
	- Takes: PortType option

* UnselectAll (Msg)
	- Takes: ()

### BusWire
* GetTargetedWire
	- Takes: point
	- Returns: Wire option

* AddWire
	- Takes: Port Port
	- Returns: ()

* Dragging
	* StartDrag
		- Takes: XYPos
		- Returns: ()
	* Dragging
		- Takes: XYPos
		- Returns: ()
	* EndDrag
		- Takes: ()
		- Returns: ()

* SetSelected
	- Takes: Wire boolean
	- Returns: ()

* UnselectAll
	- Takes: ()
	- Returns: ()

### Sheet
* Issie stuff
