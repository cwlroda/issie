# Ben Stobbs - Sheet - Individual Work
## Features
### Symbols
* **Creation** - symbols can be created with `Ctrl+N` or the menu shortcut. They are placed in the position of the last mouse-down event.
* **Selection**
    * **Clicking** - a single symbol can be selected by clicking on it. Click away in empty space to deselect.
    * **Shift Clicking** - select multiple symbols by holding shift and clicking. Clicking a selected symbol whilst holding shift will deselect it.
    * **Selection Box** - select multiple symbols by dragging a selection box over them.
    * **Select All** - Use `Alt+A` or the menu option to select all symbols.
    * **Copy and paste** - use `Alt+C` and `Alt+V` or the menu options to copy and paste the selected symbols. After pasting, the newly created symbols are selected.
* **Deletion** - the selected symbols can be deleted with `DEL` or the menu option.

### Wires
* **Creation** - A wire is created by dragging between two ports. Sheet ensures the port types are opposite and then sends the `AddWire` message. The source port and nearby ports of opposite type are highlighted as the preview wire is dragged around.
* **Selection** - A wire is selected by clicking on or near it.
* **Deletion** - Pressing `DEL` or using the menu item deletes a selected wire.
* **Errors** - On each wire event, Sheet queries BusWire for any errors. These are then displayed in the UI and the error location can be highlighted for convenience.

### Dragging
* **Wires**
    * Some simple wire dragging is implemented to demonstrate that the relevent `StartDrag`, `Dragging` and `EndDrag` messages are sent.
* **Symbols**
    * **Snap to grid toggle** - by default, snap-to-grid is enabled. This can be toggled with `Ctrl+G` or the menu item. When snap-to-grid is re-enabled, the symbols snap to their nearest grid position.
    * **Show grid toggle** - showing the grid can be toggled with `Ctrl+F` or the menu item. This control is independent of the actual snap-to-grid behaviour.
    * **Change grid size** - the grid size can be adjusted with `Ctrl+Q` and `Ctrl+W`. The symbols are snapped onto the new grid.

* **Zoom and scrolling**
    * **SVG zoom** - use `Ctrl+=` and `Ctrl+-` to zoom in and out. The zoom is a SVG transformation rather than a web zoom, which allows other elements of the UI to be independent of this zoom.
    * **Scrolling** - the canvas `div` scrolls independently of the rest of the page. Similarly to above, this allows for other static elements on screen.