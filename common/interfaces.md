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

Finally for messages, they are just parameters for the update function, and the update function always returns a tuple of (updatedModel:Model, Cmd.none). Not really sure whats Cmd do, but thats that and it kinda works

 
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

* (Previously Discussed) CreateSymbolOfType (Msg)
	- Takes: Component

* (Skeleton Code) AddSymbol of (comp:CommonTypes.ComponentId, pos:XYPos)
 (This has already been defined in Symbol.fs, but it was just about creating the circles. I have edited it to create some random working gates rather than circle which will be great to use as a placeholder.Currently This AddSymbol is not in use because I cant test it now since buswire has to call it, and probably sheets has to inform buswire to call it)

* Dragging (Msg)
(Yep, these are messages )
	* StartDraging of (sId:CommonTypes.ComponentId, pagePos:XYPos)
	(Sets true all "IsDragging" record label of symbols with "Selected" record label = true)
	* Dragging of (rank:CommonTypes.ComponentId, pagePos:XYPos)
	(Updates Position of symbol to follow the mouse. Applies to all symbols with "Selected" record label = true)
	(Currently I dont really think the parameter rank is needed, and have not used it, but ill just put it there for now)
	* EndDragging (sId:CommonTypes.ComponentId)
	(Sets false all "IsDragging" record label of symbols with "Selected" record label = true)
	(Same here, not sure if i will use sId)



* SetSelected of (topLeft:XYPos, bottomRight:XYPos)
(This is used in the event where sheets highlights an area in the application, and wishes to select all symbols in that area. Sheets then informs buswire of the topLeft and bottomRight positions of the intended area, who will then transmit it down to sheets. Sheets will set true the "Selected" Record of all symbols ENTIRELY covered by the area, and set false for the all other symbols.)

* (Previously Discussed) HighlightPorts (Msg)
	- Takes: PortType option
* (Marcus' reimplementation) Hovering and Unhovering Ports
	*MouseOverPort of (port:Port)
	(Sets true all Hover record of the port that is being hovered, and false for all other ports)
	*MouseOutPort of (port:Port)
	(Sets false all Hover record of all ports)

* UnselectAll
(Set false "Selected" record lablel of all symbols )


*UPDATES ON DISCRIMINATED UNIONS
Type Port has a new added record label "Hover : PortHover" where PortHover = |PortHover of bool
Type Component has record labels "X", "Y", "H", "W", as floats for future facilitations of changing grid denomination

*TRY IT YOURSELF! :)
I have added and changed a whole lot of stuff in CommonTypes.fs to facilate better type casting. 
If you want to try out this code of mine i have uploaded my 3 fs files on the git! 
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
