## Implemented Functionality

### Routing
Different forms of routing occurs when busWire recives:
- AddWire: Creates new wire and autoRoutes a connection
- Symbol.Drag: update the wire using auto routing
- Dragging: adjust the selected segement 
- EndDrag: update portconnection and if necessary routing if a portconnection segement has mvoed.updatewires to fit gird
\\

* autoRoute: Model -> portId -> portId -> XYPos List
    * Takes the wire and extracts the source port and target port. Gnerates a 3 segment or 5 segment wire depending on the ports relative position
* manualRouteAdj: Model -> wireId -> XYPos ->  Map <COnnectionId * wire>
    * Takes the Id for the selected wire Id and looks at the relative movement since last drag message and updates the Segments which was the closed to pos of the mouse. If the selected segement is connected to a port the segment can be moved from port. To allow for manual reconnection.
* UpdatePortConnection: Model -> ConnectionId -> Wire
    * Called by ensure connection. Used to check if the end of the wires are still connected to their port. If they are not, look to see if they should be reconnected (update the wire with new ports and autoroute it) or if "mid-air" autoRoute a connection between the orginal ports.
* ensureConnection: Model -> Map <COnnectionId * wire>
    * Called after an EndDrag message has been recived. Goes through the model in its current state and checks all wires ends are at ports. Calls UpdatePortConnection for each wire.

* fitToGrid: Model -> Map <COnnectionId * wire>
    * Used after a drag action has been finished to ensure that all verties sit on the grid.
### Add/Delete Wires
* addWire: Model -> portId -> portId -> Map <ConnectionId * wire>
    * Creates a new wire instance and sets the correct colours and size by validating the give portsIds. Adds the new wire to the wireMap in the model.
* deleteWire: Model -> ConnectionId -> Map <ConnectionId * wire>
    * Takes the wire id and removes the wire from the model
### Width annotation
* Keept as fields in the Wire type. THe model has to keep track "WireAnnotation: bool" if the annotations are on. View function has an if statment when it creates the annotation props which allows labels to be shown or hidden.


### Colours depending on action
* setSelectedColor: Model -> ConnectionId -> Map <ConnectionId * wire>
    * Updates the colour of the wire with the given id to be green to indicate that it is selected
* setUnselectedColor: Model -> ConnectionId -> Map <ConnectionId * wire>
    * Goes through the model and updates the color of all wires depending on their widths and if they are connections will create connection.

### Interface Functions
* getTargetedWire: Model -> XYPos -> CommonTypes.ConnectionId Option
    * Filters the Map of wires using isTarged Wire which takes a wire and creates a BoundingBox for each segments and then check if the point is in the bouding box. If the filtered map has more than one element the wire which is closed to the point is chosen. If there is not wire close to the given position None is returned.
* getErros: Model -> Error list
    * Goes through the model and creates a list with the existing error and the position of the associated Source Port



