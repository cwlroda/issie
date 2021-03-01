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

type RelativePosition =
    | UP
    | LEFT
    | INSIDE
    | RIGHT
    | DOWN

let distanceFromPoint (p: XYPos) (bb: BBox) =
    let verticalPos =
        if p.Y < bb.Pos.Y then
            UP
        else if p.Y < (bb.Pos.Y + bb.Height) then
            INSIDE
        else
            DOWN

    let horizontalPos =
        if p.X < bb.Pos.X then
            LEFT
        else if p.X < (bb.Pos.X + bb.Width) then
            INSIDE
        else
            RIGHT

    let anchor =
        match (verticalPos, horizontalPos) with
        | (UP, LEFT) -> bb.Pos
        | (UP, RIGHT) -> posAdd bb.Pos (posOf bb.Width 0.)
        | (DOWN, LEFT) -> posAdd bb.Pos (posOf 0. bb.Height)
        | (DOWN, RIGHT) -> posAdd bb.Pos (posOf bb.Width bb.Height)
        | (UP, INSIDE) -> posOf p.X bb.Pos.Y
        | (DOWN, INSIDE) -> posOf p.X (bb.Pos.Y + bb.Height)
        | (INSIDE, LEFT) -> posOf bb.Pos.X p.Y
        | (INSIDE, RIGHT) -> posOf (bb.Pos.X + bb.Width) p.Y
        | (INSIDE, INSIDE) -> p
        | (_, _) -> failwithf "This relative position is impossible"

    posDist p anchor
