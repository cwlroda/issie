let autoRouting (model: Model) (wId: CommonTypes.ConnectionId) (srcPos: XYPos) (tgtPos: XYPos) : WireSegment list =
    // calculate segment offsets
    // for each wire segment, figure out if wire segment cuts through any symbol and shift accordingly if there are obstacles
    // when updating wire segment
    //  1. change in start pos = change in end pos of previous segment
    //  2. change in end pos = change in start pos of next segment

    // checks if a symbol will block the path of a wire segment
    let intersectsSymbol (sym: Symbol.Symbol) (axis: string) (offset: float) =
        match axis with
        | "X" -> sym.Component.X < offset || offset < (sym.Component.X + sym.Component.W)
        | "Y" -> sym.Component.Y < offset || offset < (sym.Component.Y + sym.Component.H)
        | _ -> failwithf "Not implemented"
    
    // offsets wire segment if obstacle is found
    let bypassObstacles (axis: string) (offset: float) : float =
        match List.tryFind (fun sym -> intersectsSymbol sym axis offset) model.Symbol with
        | Some _ -> (offset + 10.)
        | None -> offset
    
    // creates wire segment
    let makeSegment x1 y1 x2 y2 =
        let startPos = {X = x1
                        Y = y1}
        let endPos = {X = x2
                      Y = y2}
        
        {
            Id = CommonTypes.WireSegId (uuid())
            StartPos = startPos
            EndPos = endPos
            Selected = false
            LastDragPos = {X = 0.
                           Y = 0.}
            IsDragging = false
            Color = CommonTypes.Grey
            HostId = wId
        }

    let offsetSrcX = 10.
    let offsetTgtX = -10.
    let offsetSrcY = 0.
    let offsetTgtY = 0.
    
    match (tgtX - srcX) with
    | x when x >= 20. -> 
        match (tgtY - srcY) with
        // case 2 & 3
        | y when y <> 0. ->
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeSegment srcX srcY (srcX + offsetSrcX) srcY
            let (s2: WireSegment) = makeSegment (srcX + offsetSrcX) srcY (srcX + offsetSrcX) (srcY + offsetSrcY)
            let (s3: WireSegment) = makeSegment (srcX + offsetSrcX) (srcY + offsetSrcY) tgtX tgtY
            s1::s2::s3::segList
        // case 1
        | y when y = 0. ->
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeSegment srcX srcY tgtX tgtY
            s1::segList
        | _ -> failwithf "Not implemented"
    | x when x < 20. ->
        match (tgtY - srcY) with
        // case 9
        | y when y < 0. ->
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeSegment srcX srcY (srcX + offsetSrcX) srcY
            let (s2: WireSegment) = makeSegment (srcX + offsetSrcX) srcY (srcX+offsetSrcX) (srcY+offsetSrcY)
            let (s3: WireSegment) = makeSegment (srcX + offsetSrcX) (srcY + offsetSrcY) (tgtX + offsetTgtX) (tgtY + offsetTgtY)
            let (s4: WireSegment) = makeSegment (tgtX + offsetTgtX) (tgtY + offsetTgtY) (tgtX + offsetTgtX) tgtY
            let (s5: WireSegment) = makeSegment (tgtX + offsetTgtX) tgtY tgtX tgtY
            s1::s2::s3::s4::s5::segList
        // case 8
        | y when y >= 0. ->
            let (segList: WireSegment List) = []
            let (s1: WireSegment) = makeSegment srcX srcY (srcX + offsetSrcX) srcY
            let (s2: WireSegment) = makeSegment (srcX + offsetSrcX) srcY (srcX + offsetSrcX) (srcY + offsetSrcY)
            let (s3: WireSegment) = makeSegment (srcX + offsetSrcX) (srcY + offsetSrcY) (tgtX + offsetTgtX) (tgtY + offsetTgtY)
            let (s4: WireSegment) = makeSegment (tgtX + offsetTgtX) (tgtY + offsetTgtY) (tgtX + offsetTgtX) tgtY
            let (s5: WireSegment) = makeSegment (tgtX + offsetTgtX) tgtY tgtX tgtY
            s1::s2::s3::s4::s5::segList
        | _ -> failwithf "Not implemented"
    | _ -> failwithf "Not implemented"