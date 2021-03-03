## Implemented Functionality

### Routing
Different forms of routing occurs when busWire recives:
- AddWire: Creates new wire and autoRoutes a connection
- Symbol.Drag: update the wire using auto routing
- Dragging: adjust the selected segement 
- EndDrag: update portconnection and if necessary routing if a portconnection segement has mvoed.updatewires to fit gird
* addWire: Model -> portId -> portId -> Map <COnnectionId * wire>
    Creates a new wire instance and sets the correct colours and size by validating the give portsIds. Adds the new wire to the wireMap in the model.
* autoRoute: Model -> portId -> portId -> XYPos List
    Takes the wire and extracts the source port and target port. Gnerates a 3 segment or 5 segment wire depending on the ports relative position
*manualRouteAdj: Model -> wireId -> XYPos ->  Map <COnnectionId * wire>
    Takes 
##### 
### Width annotation
### Colours depending on action


