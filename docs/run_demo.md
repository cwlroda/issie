# Running the Demo

## Getting the Program to Start

#### First time running
1. In the main directory, open up the command prompt
2. Run the following code
    - Windows: ```build```
    - Linux: ```build.sh```
3. If the program fails to run, go to the original skeleton code, retrieve the file ```build.fsx```, and paste it into the root directory, then repeat step 2 again.

#### Subsequent times
1. In the main directory, open up the command prompt
2. Run the following code
    - ```npm run dev```

## Demo Procedures
#### Symbols
##### 1. Rendering Different Symbol Components
As this is a demo, several symbols have already been randomly pre-generated. If some symbols are not shown on the screen, kindly hit ```ctrl-R``` to refresh the interface until the desired symbol is rendered.

##### 2. Selecting / Unselecting Symbols
- One Symbol
    - Symbols can be selected by clicking on them. They can likewise be unselected by clicking outside of the selected symbol. 
    
- Multiple Symbols in an Area
    - Should it be desired to select multiple symbols in an area, click and drag an area that intersects with the desired symbols. 

- Multiple Symbols by Individual Selection
    - By holding ```ctrl``` and clicking on new symbols, the new symbols will become selected while the already selected symbols will remain selected. 

##### 3. Moving Symbols
Symbols can be moved my clicking and dragging any symbol. 
Performing this action on any selected symbol will move all selected symbols.
Performing this on a non-selected symbol will unselect all selected symbols, then select and move the current symbol. 

##### 4. Creating Symbols
As this is only a demo, the only component that will be created is an ```and``` gate.
To create this symbol, ```right-click``` on any space in the user interface. 

##### 5. Deleting Symbols
By pressing ```del```, all selected symbols will be deleted. 

#### Wires

##### 1.Creating Wires
Wires can be created by connecting an input port to an output port.
To make such a connection, ```left-click``` on any ports, and drag it to any other opposite-type ports.

##### 2. Selecting Wires
Wires can be selected by clicking on any segment of the wire. 

##### 3. Deleting Wires
By pressing ```del```, all selected wires will be deleted (Works in conjunction with symbol deletion).

##### 4. Auto-routing Wires
Wires are auto-routed everytime the positions of their parent symbols are being updated. They prioritise the shortest path that does not intersect with any symbols. Should all paths be intersecting a symbol, the best possible intersecting path will be selected. 

##### 5. Manual-routing Wires
Wires can be manual-routed by selecting the desired wire at its non-terminal segments, then dragging it to the desired position. 

##### 6. Changing Ports
The source/target ports of wires can be changed by selecting the desired wire at its terminal segments, then lining the end of the wire segment with the new port.

##### 7. Copying / Pasting Wires
Wires are only copied if both parent symbols are being copied. Otherwise only the symbol is copied and not the wire.
Pasting wires will mean that the newly pasted wire is connected to the newly pasted parent symbols. 

#### Sheet
##### 1. Snap-to-Grid
All wires and symbols are snapped to the grids of the sheet. 
##### 2. Pan
The sheet is infinite. To pan the sheet, hold the ```mousewheel``` button and drag accordingly.
##### 3. Zoom
 - To zoom in, press ```ctrl shift = ```
 - To zoom out, press ```ctrl -```

 ### Added features
 ##### 1. Width inference
 a. Connect a component without width inference to one with width inference, then delete the connection.
 The width of the symbol that has width inference will be updated accordingly. 
 b. Connect a component without width inference to a merge (ConnectionA), and conenct the output of that merge to another merge (ConnectionB), and the output of the last merge connects to the empty input port of the first merge (ConnectionC). An error should be expected. Delete the ConnectionC and the error should be resolved. Delete ConnectionA and a new error should arise.

 
