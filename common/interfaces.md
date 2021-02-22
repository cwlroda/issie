# Interfaces

## Common
* BBox
	- XYPos (top Left corner), Width, Height
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


* getTargetedSymbol (symModel: Model) (pos: XYPos) : CompenentId  Option

* getSymbolsInTargetArea (symModel: Model) (bbox: BBox) : ComponentId List

* getTargetedPort (symModel: Model) (pos: XYPos) : PortId Option

(As mentioned earlier, GetTargetedSymbol does not really change the model, all it does is just query and gets a response. so instead i have redefined it as a helper function).

In the skeleton code the opposite of getTargetedSymbol is also defined. 
* (Skeleton Code) symbolPos (symModel: Model) (sId: ComponentId) : XYPos
	

* portPos (symModel: Model) (portId: PortId)  : XYPos

(Similar to the previous, GetPortPosition is also just a query, so it is redefined as a helper function)


*  AddSymbol of (comp:CommonTypes.ComponentId, pos:XYPos) 
 (This has already been defined in Symbol.fs, but it was just about creating the circles. I have edited it to create some random working gates rather than circle which will be great to use as a placeholder.Currently This AddSymbol is not in use because I cant test it now since buswire has to call it, and probably sheets has to inform buswire to call it)

* Dragging (Msg)
(Yep, these are messages )
	* StartDraging  (sIdLst:CommonTypes.ComponentId list) (pagePos:XYPos)

	* Dragging (rank:CommonTypes.ComponentId list) (pagePos:XYPos)

	* EndDragging 

* SetSelected (sIdLst: CommonTypes.ComponentId list)
(This is used in the event where sheets highlights an area in the application, and wishes to select all symbols in that area. Sheets then informs buswire of the topLeft and bottomRight positions of the intended area, who will then transmit it down to sheets. Sheets will set true the "Selected" Record of all symbols ENTIRELY covered by the area, and set false for the all other symbols.)

* PermenentHighlightPort (pId: PortId)
	
* DehighlightPort - for after "end drag"








### BusWire
* GetTargetedWire (pos: XYPOs) : (wireId: CommonTypes.ConnectedId),  Option 
* GetWireInTargetArea (bbox: BBox): CommonTypes.ConnectionId list

* AddWire (srtPortId: PortId) (tgtPortId: PortId) 
* DeleteWire (wireId: CommonTypes.ConnectionId)

* SetSelected (wireId: CommonTypes.ConnectionId)


* UnselectAll


* Dragging
	* StartDrag (wireId: CommonTypes.ConnectionId) (pos: XYPos)
	
	* Dragging (wireId: CommonTypes.ConnectionId) (pos: XYPos)
		
	* EndDrag



### Sheet
* Issie stuff
