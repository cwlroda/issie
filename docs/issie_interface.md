# Issie Interfaces

## Interface functions 
### Function in Symbol
Symbol contains three functions to convert symbols and ports to fit with Issie. 

#### extractComponent -> Symbol.Model -> ComponentId -> Component
This function extracts the underlying component from the symbol wrapper, by using the component id to search for that specific component.

#### extractComponents -> Symbol.Model -> Component list
This function extracts all underlying components from the symbol model. 

#### translatePortTtoIssieFormat -> Symbol.Model -> PortId -> (ComponentId * PortNumber * PortType)
Considering that PortId is a type defined by our team, it was agreed to translate it into formats that are perhaps understood by Issie. This extracts the port from the symbol model using the port id, then translate it into a tuple of (ComponentId * PortNumber * PortType), which is readable by Issie. 

### Functions in Buswire
Buswire contians three functions specifcally designed to convert between buswires Wire type and Issie's connection type. Note that the ```Connection.Id``` which has type string is the string of the connection id which uniqly identifies the wires. 

#### extractWire Buswire.Model -> Symbol.Model -> ConnectionId -> Connection
Takes a Buswire.Model, Symbol.Model, ConnectionId, and extracts all the information which issie needs for its connection type and creates a Connection which has an Id which is the same as the wire id of the Wire commonent of interest.
#### extractSymbolWires Buswire.Model -> Symbol.Model -> ComponentId -> Connection list
Extracts all the wires connected to symbol with the given component id and returns a list of connections where each connection contains the information of interest from the matching wire.

#### extractAllWires Buswire.Model -> Symbol.Model -> Connection list
Returns a list of connections where each connection is the map of a wire from the Wire type to the connection type. 

### Functions in Sheet
Sheet contains the following Interface Messages which can be used by ISSIE for all top-level renderer functionality. The keyboard shortcuts were abstracted to have functional names, as ISSIE may only want to call certain messages when the Renderer portion of the UI is in focus, for example.

* `CreateObjects of CreateElements` - The `CreateElements` type contains a list of components and list of wires to be added to the renderer.
* `Copy` - To copy the selected objects to the renderer clipboard.
* `Paste` - Paste the renderer objects currently in the clipboard.
* `Undo` - Return to the previous state.
* `Redo` - Cancel the most recent undo.
* `DeleteSelected` - Delete the currently selected symbols and wires.
* `CancelAction` - Cancel the action currently occurring in the renderer, for example dragging an object, or placing a new symbol.
* `SelectAll` - Select all symbols.
* `Zoom of ZoomRequest` - Zoom the sheet. The ZoomRequest DU consists of In, Out or Reset.
* `ToggleWireDebug` - Toggle wire debugging - when debugging, the bounding boxes of wires are displayed.
* `MouseMsg of MouseT * Modifier` - Send a mouse message to the renderer. MouseT contains information about which mouse button is pressed, the position of the mouse and if the mouse was clicked up / down / moved, etc. Modifier describes if the Control key was being held during the action.