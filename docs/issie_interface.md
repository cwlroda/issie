# Issie Interfaces

## Interface functions 
### Function in Symbol
### Functions in Buswire
Buswire contians three functions specifcally designed to convert between buswires Wire type and Issie's connection type. Note that the ```Connection.Id``` which has type string is the string of the connection id which uniqly identifies the wires. 

#### extractWire Buswire.Model -> Symbol.Model -> ConnectionId -> Connection
Takes a Buswire.Model, Symbol.Model, ConnectionId, and extracts all the information which issie needs for its connection type and creates a Connection which has an Id which is the same as the wire id of the Wire commonent of interest.
#### extractSymbolWires Buswire.Model -> Symbol.Model -> ComponentId -> Connection list
Extracts all the wires connected to symbol with the given component id and returns a list of connections where each connection contains the information of interest from the matching wire.

#### extractAllWires Buswire.Model -> Symbol.Model -> Connection list
Returns a list of connections where each connection is the map of a wire from the Wire type to the connection type. 

### Functions in Sheet
