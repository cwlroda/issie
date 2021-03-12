﻿module Helpers
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

type BBox =
    { Pos: XYPos //Top left corner of box
      W: float
      H: float }

//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//

/// return a v4 (random) universally unique identifier (UUID)
let uuid():string = import "v4" "uuid"


/// Useful helper functions
let posOf x y = { X = x; Y = y }

/// Takes a set of coordinates and returns a new set of cooridantes adjusted according to the adjPos floats given
/// each in the X and Y respectively
let posDiff a b = { X = a.X - b.X; Y = a.Y - b.Y }

let posAdd a b = { X = a.X + b.X; Y = a.Y + b.Y }

let midPt (aPos: XYPos) (bPos: XYPos): XYPos =
    let diff = posDiff bPos aPos
    posAdd aPos (posOf (diff.X / 2.) (diff.Y / 2.))


let makeBBox (pos: XYPos) (width: float) (height: float) =
    {Pos = pos; W =  width; H = height}


///Takes a point and a BBox and checks if the point are within the bounderies of the box
/// Returns true if the points is within the bounderires of the BBox. Otherwise returns false
let ptInBB (pt: XYPos) (bb: BBox): bool =
    let bRCorner = posAdd bb.Pos { X = bb.W; Y = -bb.H }
    match pt.X, pt.Y with
    | x, _ when (x < bb.Pos.X) || (x > bRCorner.X) ->
        false

    | _, y when (y > bb.Pos.Y) || (y < bRCorner.Y) ->
        false
    | _ -> true



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




    
