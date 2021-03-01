// match wire type cases based on port positions
// pathfind by incrementing in intervals of 5px
// check if next increment will hit bounding box of symbol
// make a turn that will decrease distance to output port
// returns wire with updated segments

let autoRouting (wModel: Model) (wId: ConnectionId) : Wire =
    // checks if wire segment intersects symbol
    let offsetSymbol (sym: Symbol.Symbol) (axis: Direction) (offset: float) : float =
        let leftSym = sym.Component.X
        let rightSym = sym.Component.X + sym.Component.W
        let topSym = sym.Component.Y
        let botSym = sym.Component.Y + sym.Component.H

        match axis with
        | Horizontal -> 
            match (topSym - 10. <= offset), (offset <= botSym + 10.) with
            | true, true -> botSym - offset + 10.
            | _, _ -> 0.
        | Vertical ->
            match (leftSym - 10. <= offset), (offset <= rightSym + 10.) with
            | true, true -> rightSym - offset + 10.
            | _, _ -> 0.
    
    // calculate total offset to bypass all symbols
    let calcOffset (offset: float) (axis: Direction) : float = 
        wModel.Symbol
        |> List.fold (fun acc sym -> acc + (offsetSymbol sym axis acc)) offset
        
    // offsets wire segment if obstacle is found
    let bypassObstacles (wire: Wire) : WireSegment list =
        let srcPos = Symbol.portPos wModel.Symbol wire.SrcPort
        let tgtPos = Symbol.portPos wModel.Symbol wire.TargetPort

        let res =
            wire.Segments
            |> List.fold (fun (acc: (WireSegment list * float)) s -> 
                match s.StartPos, s.EndPos with
                | x, _ when x = srcPos -> ((fst acc) @ [s], 0.)
                | _, x when x = tgtPos ->
                    match s.Direction with 
                    | Horizontal ->
                        let seg = {
                            s with StartPos = {
                                X = s.StartPos.X - snd acc
                                Y = s.StartPos.Y
                                }
                            }
                        
                        ((fst acc) @ [seg], 0.)
                        
                    | Vertical ->
                        let seg = {
                            s with StartPos = {
                                X = s.StartPos.X
                                Y = s.StartPos.Y - snd acc
                                }
                            }

                        ((fst acc) @ [seg], 0.)
                | _, _ ->
                    match s.Direction with 
                    | Horizontal ->
                        let seg = {
                            s with StartPos = {
                                X = s.StartPos.X - snd acc
                                Y = s.StartPos.Y
                                }
                            }

                        let offset = calcOffset seg.StartPos.Y s.Direction
                        
                        ((fst acc) @ 
                            [{
                                seg with 
                                    StartPos = {
                                        X = seg.StartPos.X
                                        Y = seg.StartPos.Y + offset
                                        }
                                    EndPos = {
                                        X = seg.EndPos.X
                                        Y = seg.EndPos.Y + offset
                                        }
                            }], offset)
                        
                    | Vertical ->
                        let seg = {
                            s with StartPos = {
                                X = s.StartPos.X
                                Y = s.StartPos.Y - snd acc
                                }
                            }

                        let offset = calcOffset seg.StartPos.X s.Direction
                        
                        ((fst acc) @
                            [{
                                seg with 
                                    StartPos = {
                                        X = seg.StartPos.X + offset
                                        Y = seg.StartPos.Y
                                        }
                                    EndPos = {
                                        X = seg.EndPos.X + offset
                                        Y = seg.EndPos.Y
                                        }
                            }], offset)
                ) ([], 0.)

        (fst res)

        // need to update start pos of next wire somehow
    
    let wire = findWire wModel wId

    {wire with Segments = bypassObstacles wire}



let autoRoute (wModel: Model) (wId: ConnectionId) : Wire =
    // match wire type cases based on port positions
    // pathfind by incrementing in intervals of 5px
    // check if next increment will hit bounding box of symbol
    // make a turn that will decrease distance to output port
    // returns wire with updated segments

    let wire = findWire wModel wId
    let srcPos = Symbol.portPos wModel.Symbol wire.SrcPort
    let tgtPos = Symbol.portPos wModel.Symbol wire.TargetPort

    // let wireType =
    //     match (tgtPos.X - srcPos.X) with
    //     | x when x >= 0 ->
    //         match (tgtPos.Y - srcPos.Y) with
    //         | y when y > 0 ->
    //         | y when y = 0 -> 1
    //         | _
    //     | _ ->
    //         match (tgtPos.Y - srcPos.Y) with
    //         | y when y > 0
    //         | y when y = 0
    //         | _

    let acc = srcPos
    let dir = Horizontal

    let rec (route: WireSegment list) =
        let seg = []
        match acc, dir with
        | x, Horizontal when x = tgtPos ->
            route @ [x]
        | x, Vertical ->
            match
        | _ ->