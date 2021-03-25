# Summary of Features



### 1. Auto-routing of wires: 
1. Performs shortest-path algorithm, which implements bi-directional recursion to avoid symbols in the horizontal or vertical direction based on the orientation of the wire segment.
Wires are auto-routed every time the positions of their parent symbols are being updated.

### 2. Manual-routing of wires:
1. Wires can be manually routed by selecting the desired wire at its non-terminal segments, then dragging it to the desired position. 
2. Wires that have been manually routed will have their positions locked, and will not be influenced by other moving symbols. 
3. These positions can be unlocked by moving the parent symbols.

### 3. Changing Ports 
1. The source or target ports of a wire can be changed by selecting the desired wire at its terminal segments, then dragging it to the new desired port.
2. The previous and the new ports must be of the same polarity.

### 4. Selection of Symbols
1. The desired symbol can be selected by ```left-click```-ing it.
2. To select multiple symbols individually, hold ```ctrl``` and click on the desired symbols.
3. To select an area of symbols, hold ```left-click``` and drag it to intersect the desired area of symbols.
4. To select multiple areas of symbols, hold ```ctrl``` and perform the previous step. 
5. ```alt+a``` selects all symbols.

### 5. Deleting Symbols
1. ```del``` deletes all selected symbols and their wires.

### 6. Creating a Symbol
1. ```alt+n``` creates a new random symbol (for Demo Purposes). The generated symbol will be in preview-state. It will be translucent and follow the cursor until ```left-click``` command is performed on an empty space of the sheet.

### 7. Dragging Symbols
1. To drag a symbol, hold ```left-click``` on a symbol and move the mouse. 
2. If several symbols are already selected, performing step 1 on will drag all of them.

### 8. Creating a Wire 
1. To create a wire, hold ```left-click``` on a port and release it on another port. Both ports must be of opposite polarity or the wire will not be created. Only ports that satisfy this criterion will be highlighted. 
2. ```ins``` creates a random wire between two ports of opposite polarity.

### 9. Deleting a Wire
1. To only delete a wire, click on the wire to select it, then press ```del```.

### 10. Debugging Mode for Wires 
1. ```alt+shift+d``` enables the bounding boxes of wires to be seen. 

### 11. Wire Errors
Wires with errornous connections will be highlighed red. These include
1. Port width mismatch
2. No driver (when a port of currently undefined width is connected to another port)
3. Multiple wires connected to the same target port

### 12. Copying and Pasting Symbols
1. Works on all selected symbols. ```alt+c``` to copy and ```alt+v``` to enter paste mode.
2. ```left-click``` on an empty space to exit paste mode. Symbols can only be pasted when they are not intersecting with other symbols. All existing wires in the original selection will be carried forward to the newly pasted symbols. 

### 13. Undo/Redo
1. ```alt+z``` performs the undo.
2. ```alt+shift+z``` performs the redo.

### 14. Zoom
1. ```ctrl+shift+=``` zooms in
2. ```ctrl+-``` zooms out
3. ```ctrl+=``` resets the zoom

### 15. Cancelling Operations
1. ```esc``` cancels all intermediate-stage operations

### 16. Symbol Collision Detection
1. When a symbol is colliding with another symbol, it will turn translucent. 

### 17. Snap-to-Grid
1. All wires and symbols are snapped to the grids of the sheet.

### 18. Pan
1. The sheet is infinite. To pan the sheet, hold ```mousewheel``` and drag accordingly.

### 19. Width Inference
1. Updates currently undefined port-width to become defined if it were to be connected to a defined source port.
2. Removes the port-width of width-inferred symbols should the connecting wire be deleted or relocated. 
3. Protects against circular port-width definitions. 

### 20. Wires
1. Single-bit wires are blue, while buses are purple. 
2. Buses have their width labelled, and are thicker than single-bit wires. 
3. Wires have their corners curved to be aesthetically pleasing. 

### 21. Symbols
1. All symbols are rectangular, with curved edges for aesthetic purposes.  
2. Non-selected symbols are grey.
3. Selected symbols are greyish-green, with their port widths displayed. 
4. Symbols are labelled internally, with a maximum string length of up to 10 characters depending on the component type. 
5. Symbol titles have a maximum string length of 15 characters. 
6. Symbol port labels have a maximum string length 8 characters. 




