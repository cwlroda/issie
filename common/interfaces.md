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

After working on Symbol, I realised that Sheets only talks to Buswire, who then talks to Symbol. This kind of exclusive top down approach is used in the skeleton code and i believe it is meant to be extrapolated to work the same way for the entire project

Another thing i understood about messages is that it is supposed to update the symbol model (model = symbol list). Therefore some functions that do not update the symbol model (like the previously GetPortPosition) should not be messages, but should instead be helper functions. 
Therefore i will be amending the stuffs below, and providing you with my definitions for them.

 
* (Previously Discussed) GetTargetedSymbol (Msg)
	- Takes: XYPos
	- Returns: Symbol option
* (Marcus' Redefinition) getTargetedSymbol (symModel: Model) (selectionBoxTopLeft:XYPos) (selectionBoxBottomRight:XYPos) : ComponentId list
(As mentioned earlier, GetTargetedSymbol does not really change the model, all it does is just query and gets a response. so instead i have redefined it as a helper function).

In the skeleton code the opposite of getTargetedSymbol is also defined. 
* (Skeleton Code) symbolPos (symModel: Model) (sId: ComponentId) : XYPos
	
* (Previously Discussed) GetPortPosition (Msg)
	- Takes: Port
	- Return: XYPos option
* (Marcus' Redefinition) portPos (symModel: Model) (portId: PortId) (sId: ComponentId) : XYPos



(Similar to the previous, GetPortPosition is also just a query, so it is redefined as a helper function)
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
