module BBox

open Helpers

type BBox = {
    Pos: XYPos
    Width: float
    Height: float
}

let toBBox x y w h: BBox =
    {
        Pos = (posOf x y)
        Width = w
        Height = h
    }

let topLeft (bb : BBox) = bb.Pos
let topRight (bb : BBox) = posOf (bb.Pos.X + bb.Width) bb.Pos.Y
let bottomLeft (bb : BBox) = posOf bb.Pos.X (bb.Pos.Y + bb.Height)
let bottomRight (bb : BBox) = posOf (bb.Pos.X + bb.Width) (bb.Pos.Y + bb.Height)

let pointsToBBox (p1: XYPos) (p2: XYPos) =
    let minX = min p1.X p2.X
    let maxX = max p1.X p2.X
    let minY = min p1.Y p2.Y
    let maxY = max p1.Y p2.Y

    toBBox minX minY (maxX - minX) (maxY - minY)


let containsPoint (p: XYPos) (bb: BBox) =
    p.X >= bb.Pos.X
    && p.X < (bb.Pos.X + bb.Width)
    && p.Y >= bb.Pos.Y
    && p.Y < (bb.Pos.Y + bb.Height)

let overlaps (b1: BBox) (b2: BBox): bool =
    let isAbove b1 b2 = (b1.Pos.Y + b1.Height) < b2.Pos.Y
    let isLeftOf b1 b2 = (b1.Pos.X + b1.Width) < b2.Pos.X

    not (
        isAbove b1 b2
        || isAbove b2 b1
        || isLeftOf b1 b2
        || isLeftOf b2 b1
    )
