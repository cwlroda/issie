# Summary of Features

## Symbols
### 1. Design
1. All symbols are rectangular, with curved edges for aesthetic purposes.  
2. Non-selected symbols are grey.
3. Selected symbols are green, with their port widths displayed. 
4. Symbols are labelled internally, with a maximum string length of up to 10 characters depending on the component type. 
5. Symbol titles have a maximum string length of 15 characters. 
6. Symbol port labels have a maximum string length 8 characters.

### 2. Preview State
The preview state is an intermediate stage operation that enables the user to find out where they can place their symbols while dragging them.  This is visualised by the symbols being translucent.
1. Symbols enter preview state when they are
    - Newly created
    - Newly pasted
    - Overlapping another symbol while being dragged
2. When symbols in preview states are overlapping other symbols they will turn red, and the state cannot be exited unless forced.
3. To exit preview state, ensure that the symbols in preview state are not red, before performing ```left-click```.
4. ```esc``` force-exits the preview state.
    - Newly created/pasted symbols will be deleted
    - Symbols originally existing will be snapped back to their original positions

### 3. Creating a Symbol
1. ```alt+n``` creates a new random symbol (for Demo Purposes) in preview state.

### 4. Selection of Symbols
1. Symbols are automatically selected when they are newly created or pasted. 
2. To select a desired symbol, perform ```left-click``` with the cursor within the symbol.
2. To select multiple symbols individually, hold ```ctrl``` while performing ```left-click``` on the desired symbols.
3. To select an area of symbols, hold ```left-click``` in blank space and drag it to intersect the desired area of symbols.
4. To select multiple areas of symbols, hold ```ctrl``` and perform the previous step. 
5. ```alt+a``` selects all symbols.

### 5. Deleting Symbols
1. ```del``` deletes all selected symbols and their wires.

### 6. Dragging Symbols
1. To drag a symbol, hold ```left-click``` on a symbol and move the mouse. Doing this will also drag all other selected symbols.

### 7. Copying and Pasting Symbols
1. Works on all selected symbols. ```alt+c``` to copy and ```alt+v``` paste.
2. All pasted symbols are in preview mode. 
3. All wires connected between the copied symbols will be carried over to the pasted symbols. 

### 8. Port Width Inference
1. Symbols with ports that have no predefined widths (in this section we call them undefined ports) include
    - Bus Select (Input port 0)
    - Split Wire (Input port 0, Output port 0)
    - Merge Wires (Input port 0, Input port 1, Output port 0)
    - Wire Label (Input port 0, Output Port 0)
2. When a defined output port of another symbol is connected to an undefined input port
    - Width inferrence is performed and the input port is now defined.
    - Output ports of the same symbol that are affected by this input port will have their widths updated.
    - Width inference will be propagated to other undefined ports connected to this updated output port.
3. When a connection is removed from a previously undefined input port
    - Width inferrence is performed and the input port will revert back to being undefined.
    - Output ports of the same symbol that are affected by this input port will have their widths updated. 
    - Width inference will be propagated to other undefined ports connected to this updated output port. 
4. Protection against circular port-width definitions have been implemented. This prevents cases like 
    - Merge wires connecting to itself
    - Merge wires connecting to another merge wires and then connecting to itself


## Wires

### 1. Design
1. Single-bit wires are blue, while buses are purple. 
2. Buses have their width labelled, and are thicker than single-bit wires. 
3. Wires have their corners curved to be aesthetically pleasing. 
### 2. Preview Wire
1. The preview wire is an intermediate stage operation that enables the user to find out where they can connect their wires while dragging them.  This is visualised by a dotted line emerging from the source port.
2. To exit the preview state, release ```left-click```.
3. ```esc``` forcefully exits the preview state.

### 3. Creating a Wire 
1. To create a wire, hold ```left-click``` on a port and release it on another port. 
    - Before ```left-click``` is released, a preview wire is generated. 
    - Both ports must be of opposite polarity or the wire will not be created. 
    - Only ports that satisfy this criterion will be highlighted. 
2. ```ins``` creates a random wire between two ports of opposite polarity.

### 4. Deleting a Wire
1. To only delete a wire, click on the wire to select it, then press ```del```.

### 5. Auto-routing of Wires: 
1. Wires are auto-routed whenever
    - the positions of their parent symbols are being updated
    - a foreign symbol attempts to overlap with the wire
2. Wire auto-routing performs the shortest-path algorithm, which implements bi-directional recursion to avoid symbols in the horizontal or vertical direction based on the orientation of the wire segment.
3. If no optimal correct paths exists, then the algorithm tries its best to reduce collisions with symbols. 

### 6. Manual-routing of wires:
1. Wires can be manually routed by selecting the desired wire at its non-terminal segments, then dragging it to the desired position. 
2. Wires that have been manually routed will have their positions locked, and will no longer be auto-routed.
3. These positions can be unlocked by moving the parent symbols.

### 7. Changing Ports 
1. The source or target ports of a wire can be changed by selecting the desired wire at its terminal segments, then dragging it to the new desired port.
2. The previous and the new ports must be of the same polarity.

### 8. Wire Errors
Wires with errornous connections will be highlighed red. These include
1. Port width mismatch
2. No driver (when a port of currently undefined width is connected to another port)
3. Multiple wires connected to the same target port

### 9. Debugging Mode for Wires 
1. ```alt+shift+d``` enables the bounding boxes of wires to be seen. 


## Sheets
### 1. Design
1. The entire sheet is (practically) infinite.
2. The sheet is divided into grids of 10px by 10px. 
3. 100 of these grids are encapsulated by a larger grid of bigger thickness. 

### 2. Snap-to-Grid
1. All wires and symbols are snapped to the grids of the sheet.

### 3. Pan
1. To pan the sheet, hold ```mousewheel``` and drag accordingly.

### 4. Undo/Redo
1. ```alt+z``` performs the undo.
2. ```alt+shift+z``` performs the redo.

### 5. Zoom
1. ```ctrl+shift+=``` zooms in
2. ```ctrl+-``` zooms out
3. ```ctrl+=``` resets the zoom

### 6. Cancelling Operations
1. ```esc``` cancels all intermediate-stage operations