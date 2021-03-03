let findOffset (sym: Symbol.Symbol) (axis: Direction) (pos: XYPos) : float
    let symBBox = Symbol.symbolBBoxx wModel.Symbol sym.Id

    match axis with
    | Horizontal -> 
        match (symBBox.Corner.Y <= offset), (offset <= symBBox.Corner.Y + symBBox.H) with
        | true, true -> symBBox.Corner.Y + symBBox.H - offset + 10.
        | _, _ -> 0.
    | Vertical ->
        match (symBBox.Corner.X <= offset), (offset <= symBBox.Corner.X + symBBox.W) with
        | true, true -> symBBox.Corner.X + symBBox.W - offset + 10.
        | _, _ -> 0.

let aggOffset (offset: float) (axis: Direction) : float =
    wModel.Symbol
    |> List.fold (fun acc sym -> acc + (offsetSymbol sym axis acc)) offset

let autoRoute (wModel: Model) (wire: Wire): Map<WireSegId, WireSegments> =
    wire.Segments
    |> Map.map (fun _ v ->
        match v.Direction with
        | Horizontal ->
            let offset = calcOffset v.StartPos.Y v.Direction

            {
                v with
                    StartPos =
                        {
                            X = v.StartPos.X
                            Y = v.StartPos.Y + offset
                        }
                    EndPos =
                        {
                            X = v.StartPos.X
                            Y = v.StartPos.Y + offset
                        }
            }
        | Vertical ->
            let offset = calcOffset v.StartPos.X v.Direction

            {
                v with
                    StartPos =
                        {
                            X = v.StartPos.X + offset
                            Y = v.StartPos.Y
                        }
                    EndPos =
                        {
                            X = v.StartPos.X + offset
                            Y = v.StartPos.Y
                        }
            }
    )