# New types
### BBox
	```
	type BBox = {
		Pos: XYPos
		Width: float
		Height: float
	}
	```
	- containsPoint: BBox -> XYPos -> bool
	- distanceFromPoint: BBox -> XYPos -> float
	- overlaps: BBox -> BBox -> bool
	- makeBBox: (pos: XYPos) -> (width: float) -> (height: float) -> BBox

### Error
 	```
	type Error = {
		Msg: string
		Pos: XYPos
	}
	```
### PortId
	```
	[<Erase>]
	type PortId = | PortId of string
	```
```
