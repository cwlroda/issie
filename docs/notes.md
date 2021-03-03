# Notes

## Grid
The grid is 5px by 5px
This is implemented by having each symbol dimension be a multiple of 5px and each port be placed on a grid-coordinate.
The wires will also be made to only lie along grid lines.

## Errors
Wires can be used to connect between two ports of different widths, and therefore there needs to be error handling for incorrectly connected wires, and importantly the width of a port can change after a wire has been placed.

Symbol on the other hand can be validated in their creation menus. We've decided that having invalid symbols on the sheet is bad design and can be handled higher in the stack, and therefore there is no need to don't handle erros from invalid symbols.

