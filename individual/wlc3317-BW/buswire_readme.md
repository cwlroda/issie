# Progress Check

## Features Implemented

1. Add wire
2. Delete wire
3. Select/deselect (highlight) wire
4. Basic autorouting
5. Manual adjustment of wire segments
6. Change wire color
7. Change wire width
8. Port width inference
9. Created wire bounding boxes

## Interfaces

1. Symbol (borrowed dummy code from Marcus)
 - getTargetedSymbol:
        Model -> XYPos -> ComponentId Option
 - getTargetedPort:
        Model -> XYPos -> PortId Option
 - portPos:
        Model -> PortId -> XYPos
 - symbolBBox:
        Model -> ComponentId -> BBox
2. Sheet (changed key bindings)
3. Renderer (changed key bindings)


## TO-DO

1. Advanced autorouting (with collision detection)
