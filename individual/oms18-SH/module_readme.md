# Features

## Implemented
* Panning and zooming
* Single and multiple select of symbols
    * Click and area select
    * Ctrl for additive select
* Dragging symbols and wire
* Snap to grid and gridlines
* Error popup on erroneous wires
* Deletion of symbols and wire
* Copy and paste of symbols
* Undo and Redo of movement and deletion/creation

## Future features
* Preview of Symbol placing
    * Blocked on: Not sure how Sheet is supposed to know what symbol you want to place
* Scroll with mouse wheel
    * Blocked on: Can't receive scroll events on an unscrollable div
* Change key combinations from Alt+{A,C,V,Z} (and more) to Ctrl+{A,C,V,Z}
    * Blocked on: Electron hooks into a lot of Ctrl key combinations, so these are not available
* Display error mesages with wrapping and rectangle properly fit around the text
    * Blocked on: Can't get the size of an svg text element, and rectangles don't automatically fit to encompass text in svgs.
* Maybe save selection state for undo/redo
    * Blocked on: Not sure if that's what a user expects or not, can easily be changed.
