module BBox

open Helpers

let posOf x y = { X = x; Y = y }

let posDiff a b = { X = a.X - b.X; Y = a.Y - b.Y }

let posAdd a b = { X = a.X + b.X; Y = a.Y + b.Y }

let posDist a b =
    let diff = posDiff a b

    sqrt <| diff.X * diff.X + diff.Y * diff.Y

type Bounds = { Width: float; Height: float }

let boundsOf w h = { Width = w; Height = h }

type BBox = { Pos: XYPos; Bounds: Bounds }

let toBBox x y w h: BBox =
    { Pos = (posOf x y)
      Bounds = (boundsOf w h) }

let containsPoint (p: XYPos) (bb: BBox) =
    p.X >= bb.Pos.X
    && p.X < (bb.Pos.X + bb.Bounds.Width)
    && p.Y >= bb.Pos.Y
    && p.Y < (bb.Pos.Y + bb.Bounds.Height)

let overlaps (b1: BBox) (b2: BBox): bool =
    let isAbove b1 b2 = (b1.Pos.X + b1.Bounds.Width) < b2.Pos.X
    let isLeftOf b1 b2 = (b1.Pos.X + b1.Bounds.Width) < b2.Pos.X

    (isAbove b1 b2 || isAbove b2 b1)
    && (isLeftOf b1 b2 || isLeftOf b2 b1)

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
        else if p.Y < (bb.Pos.Y + bb.Bounds.Height) then
            INSIDE
        else
            DOWN

    let horizontalPos =
        if p.X < bb.Pos.X then
            LEFT
        else if p.X < (bb.Pos.X + bb.Bounds.Width) then
            INSIDE
        else
            RIGHT

    let anchor =
        match (verticalPos, horizontalPos) with
        | (UP, LEFT) -> bb.Pos
        | (UP, RIGHT) -> posAdd bb.Pos (posOf bb.Bounds.Width 0.)
        | (DOWN, LEFT) -> posAdd bb.Pos (posOf 0. bb.Bounds.Height)
        | (DOWN, RIGHT) -> posAdd bb.Pos (posOf bb.Bounds.Width bb.Bounds.Height)
        | (UP, INSIDE) -> posOf p.X bb.Pos.Y
        | (DOWN, INSIDE) -> posOf p.X (bb.Pos.Y + bb.Bounds.Height)
        | (INSIDE, LEFT) -> posOf bb.Pos.X p.Y
        | (INSIDE, RIGHT) -> posOf (bb.Pos.X + bb.Bounds.Width) p.Y
        | (INSIDE, INSIDE) -> p
        | (_, _) -> failwithf "This relative position is impossible"

    posDist p anchor
