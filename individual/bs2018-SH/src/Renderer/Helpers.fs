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

type MouseOp = 
    /// button up
    | Up
    /// button down
    | Down
    /// Move with button up
    | Move 
    /// Move with button Down
    | Drag

type MouseT = {
    Pos: XYPos
    Op: MouseOp}

type BBox = {
    XYPos : XYPos
    Width: float
    Height: float
}

//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//
let posOf x y = {X=x;Y=y}

let bboxFromDiagonals (c1 : XYPos) (c2 : XYPos) : BBox =
    let x =  if c1.X < c2.X then c1.X else c2.X
    let y = if c1.Y < c2.Y then c1.Y else c2.Y
    let h = if c1.Y - c2.Y > 0. then c1.Y - c2.Y else c2.Y - c1.Y
    let w = if c1.X - c2.X > 0. then c1.X - c2.X else c2.X - c1.X
    {
        XYPos = posOf x y
        Height = h
        Width = w
    }

let extendBBoxForError (bb : BBox) (e : float) : BBox =
    {
        XYPos = posOf (bb.XYPos.X - e) (bb.XYPos.Y - e)
        Height = bb.Height + (e * 2.)
        Width = bb.Width + (e * 2.)
    }

let containsPoint (pos : XYPos) (bb : BBox) : bool =
    pos.X >= bb.XYPos.X
    && pos.X <= (bb.XYPos.X + bb.Width)
    && pos.Y >= bb.XYPos.Y
    && pos.Y <= (bb.XYPos.Y + bb.Height)

let corners (bb : BBox) : XYPos list =
    [
        bb.XYPos
        posOf (bb.XYPos.X + bb.Width) bb.XYPos.Y
        posOf (bb.XYPos.X + bb.Width) (bb.XYPos.Y + bb.Height)
        posOf bb.XYPos.X (bb.XYPos.Y + bb.Height)
    ]

let overlaps (bb1 : BBox) (bb2 : BBox) : bool =
    (
        corners bb1
        |> List.sumBy (fun corner -> if containsPoint corner bb2 then 1 else 0)
    ) > 0

let polygonPointsString (point:XYPos) (diagonalPoint:XYPos) =
    let polygonPoints = [
        (point.X, point.Y)
        (point.X, diagonalPoint.Y)
        (diagonalPoint.X, diagonalPoint.Y)
        (diagonalPoint.X, point.Y)
    ]
    let pointToStr (x, y) =
        String.concat "" [(x.ToString());",";(y.ToString())]

    polygonPoints
    |> List.map pointToStr
    |> String.concat " "


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




    

