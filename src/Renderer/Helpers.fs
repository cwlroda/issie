module Helpers
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Electron
open Fable.React

//-------------------------------------------------------------------------//
//------------------------------Types--------------------------------------//
//-------------------------------------------------------------------------//

/// position on SVG canvas
type XYPos =
    {
        X : float
        Y : float
    }

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posAddX a b =
    {X=a.X+b; Y=a.Y}

let posAddY a b =
    {X=a.X; Y=a.Y+b}

let posAddXY a b =
    {X=a.X+b; Y=a.Y+b}

let posOf x y = {X=x;Y=y}

let posLength p =
    sqrt <| p.X * p.X + p.Y * p.Y

let posDist a b =
    posLength <| posDiff a b

let midPt (aPos: XYPos) (bPos: XYPos): XYPos =
    let diff = posDiff bPos aPos
    posAdd aPos (posOf (diff.X / 2.) (diff.Y / 2.))

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
    let isAbove b1 b2 = (b1.Pos.Y + b1.Height) <= b2.Pos.Y
    let isLeftOf b1 b2 = (b1.Pos.X + b1.Width) <= b2.Pos.X

    not (
        isAbove b1 b2
        || isAbove b2 b1
        || isLeftOf b1 b2
        || isLeftOf b2 b1
    )

let gridSize = 10.

// snaps wire segments to grid (variable grid size)
let snapToGrid (pos: XYPos) : XYPos =
    let xOffset = pos.X % gridSize
    let yOffset = pos.Y % gridSize

    {
        X = if xOffset < gridSize - xOffset then pos.X - xOffset else pos.X + gridSize - xOffset
        Y = if yOffset < gridSize - yOffset then pos.Y - yOffset else pos.Y + gridSize - yOffset
    }

type MouseOp = 
    /// button up
    | Up
    /// button down
    | Down
    /// Move with button up
    | Move 
    /// Move with button Down
    | Drag
    /// When mouse leaves program area
    | Leave

type MouseButton =
    | Left
    | Right
    | Middle
    | Unknown

type MouseT = {
    Button: MouseButton
    Pos: XYPos
    Op: MouseOp}

//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//

/// return a v4 (random) universally unique identifier (UUID)
let uuid():string = import "v4" "uuid"


//-----------------Code to record and print execution time statistics-------//

let timeNowInMicroS() = 
    System.DateTime.Now.Ticks
    |> (fun t -> t /10L)

type Stats = {
    Min: float
    Max: float
    Av: float
    Num: float
    }

/// add time t to st
let addTimeToStats (t:float) (st:Stats) =
    {
        Min = min st.Min t
        Max = max st.Max t
        Av = (st.Av*st.Num + t)/(st.Num+1.)
        Num = st.Num + 1.
    }

/// execution time stats indexed by name in recordExecutionStats
let mutable executionStats = Map<string,Stats> []

/// Run (f arg) recording its time in executionStats under name.
/// NB - this will run f multiple times if needed to estimate average speed more accurately.
/// If an execution time of 5ms for this function is too long reduce timeLimit.
/// The multiple time execution will not work, and will give lower than real results, if
/// f is memoised. In that case set timeLimit to 0. for only one execution.
let recordExecutionTimeStats (name: string) (f: 'a -> 'b) (arg: 'a) : 'b =
    let timeLimit = 0. // time in ms to execute f for.
    let t1 = timeNowInMicroS()
    let execTime() = float (timeNowInMicroS() - t1) / 1000.
    let res = f arg // do f
    let mutable iterations = 1
    while execTime() < timeLimit do // do f multiple times if it is fast to get more accurate speed statistics
        iterations <- iterations + 1
        f arg |> ignore // do f again
    let t = execTime() / float iterations
    executionStats <-
        Map.tryFind name executionStats
        |> Option.map (addTimeToStats t)
        |> Option.defaultValue {Min=t;Max=t;Av=t;Num=1.}  
        |> (fun st -> Map.add name st executionStats)
    res

/// print
let printStats() =
    executionStats
    |> Map.toList
    |> List.iter (fun (name,st) -> 
        printfn "%s time: min=%.3fms max=%.3fms av=%.3fms samples:%d" name st.Min st.Max st.Av (int st.Num))
    executionStats <- Map [] // reset stats

//--------------------------------Constants----------------------------------//

/// these determine the size of the canvas relative to the objects on it.
let canvasUnscaledDimensions : XYPos = 
    {X = 1000. ; Y = 1000.}




    

