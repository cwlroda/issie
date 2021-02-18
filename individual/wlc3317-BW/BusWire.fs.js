import { Union, Record } from "./.fable/fable-library.3.1.0/Types.js";
import { lambda_type, unit_type, string_type, union_type, list_type, record_type, bool_type, tuple_type, obj_type } from "./.fable/fable-library.3.1.0/Reflection.js";
import { uuid, XYPos, MouseT$reflection, XYPos$reflection } from "./Helpers.fs.js";
import { HighLightColor__Text, HighLightColor, HighLightColor$reflection } from "./CommonTypes.fs.js";
import { update as update_1, Msg as Msg_1, init as init_1, view as view_1, symbolPos, Msg$reflection as Msg$reflection_1, Symbol$$reflection } from "./Symbol.fs.js";
import { ofSeq, item, length, append, cons, ofArrayWithTail, empty, sortBy, head, fold, map, find } from "./.fable/fable-library.3.1.0/List.js";
import { randomNext, uncurry, comparePrimitives, compare, equals } from "./.fable/fable-library.3.1.0/Util.js";
import { toText, printf, toFail } from "./.fable/fable-library.3.1.0/String.js";
import { FunctionComponent_Of_Z5A158BBF } from "./.fable/Fable.React.7.0.1/Fable.React.FunctionComponent.fs.js";
import * as react from "react";
import { rangeNumber } from "./.fable/fable-library.3.1.0/Seq.js";
import { Cmd_map, Cmd_OfFunc_result, Cmd_none } from "./.fable/Fable.Elmish.3.1.0/cmd.fs.js";

export class WireSegment extends Record {
    constructor(Id, Vertices, LastDragPos, IsDragging, Color) {
        super();
        this.Id = Id;
        this.Vertices = Vertices;
        this.LastDragPos = LastDragPos;
        this.IsDragging = IsDragging;
        this.Color = Color;
    }
}

export function WireSegment$reflection() {
    return record_type("BusWire.WireSegment", [], WireSegment, () => [["Id", obj_type], ["Vertices", tuple_type(XYPos$reflection(), XYPos$reflection())], ["LastDragPos", XYPos$reflection()], ["IsDragging", bool_type], ["Color", HighLightColor$reflection()]]);
}

export class Wire extends Record {
    constructor(Id, SrcSymbol, TargetSymbol, Segments) {
        super();
        this.Id = Id;
        this.SrcSymbol = SrcSymbol;
        this.TargetSymbol = TargetSymbol;
        this.Segments = Segments;
    }
}

export function Wire$reflection() {
    return record_type("BusWire.Wire", [], Wire, () => [["Id", obj_type], ["SrcSymbol", obj_type], ["TargetSymbol", obj_type], ["Segments", list_type(WireSegment$reflection())]]);
}

export class Model extends Record {
    constructor(Symbol$, WX, Color) {
        super();
        this.Symbol = Symbol$;
        this.WX = WX;
        this.Color = Color;
    }
}

export function Model$reflection() {
    return record_type("BusWire.Model", [], Model, () => [["Symbol", list_type(Symbol$$reflection())], ["WX", list_type(Wire$reflection())], ["Color", HighLightColor$reflection()]]);
}

export class Msg extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Symbol", "AddWire", "SetColor", "MouseMsg", "StartDragging", "Dragging", "EndDragging"];
    }
}

export function Msg$reflection() {
    return union_type("BusWire.Msg", [], Msg, () => [[["Item", Msg$reflection_1()]], [["Item", tuple_type(obj_type, obj_type)]], [["Item", HighLightColor$reflection()]], [["Item", MouseT$reflection()]], [["wId", obj_type], ["sId", obj_type], ["pagePos", XYPos$reflection()]], [["wId", obj_type], ["sId", obj_type], ["pagePos", XYPos$reflection()]], [["wId", obj_type], ["sId", obj_type]]]);
}

export function posDiff(a, b) {
    return new XYPos(a.X - b.X, a.Y - b.Y);
}

export function posAdd(a, b) {
    return new XYPos(a.X + b.X, a.Y + b.Y);
}

export function posOf(x, y) {
    return new XYPos(x, y);
}

export function wire(wModel, wId) {
    return find((elem) => equals(elem.Id, wId), wModel.WX);
}

export class WireRenderProps extends Record {
    constructor(key, WireP, SrcP, TgtP, ColorP, StrokeWidthP, Dispatch) {
        super();
        this.key = key;
        this.WireP = WireP;
        this.SrcP = SrcP;
        this.TgtP = TgtP;
        this.ColorP = ColorP;
        this.StrokeWidthP = StrokeWidthP;
        this.Dispatch = Dispatch;
    }
}

export function WireRenderProps$reflection() {
    return record_type("BusWire.WireRenderProps", [], WireRenderProps, () => [["key", obj_type], ["WireP", Wire$reflection()], ["SrcP", XYPos$reflection()], ["TgtP", XYPos$reflection()], ["ColorP", string_type], ["StrokeWidthP", string_type], ["Dispatch", lambda_type(Msg$reflection(), unit_type)]]);
}

export function orthDist(pos1, pos2, pos3) {
    const shoelaceArea = Math.abs((((((pos1.X * pos2.Y) + (pos2.X * pos3.Y)) + (pos3.X * pos1.Y)) - (pos1.Y * pos2.X)) - (pos2.Y * pos3.X)) - (pos3.Y * pos1.X));
    const segmentLength = Math.sqrt(Math.pow(Math.abs(pos1.X - pos2.X), 2) + Math.pow(Math.abs(pos1.X - pos2.X), 2));
    return shoelaceArea / segmentLength;
}

export function wireToSelectOpt(wModel, pos) {
    let w_1, sId, d;
    const min = (tupledArg) => {
        const a = tupledArg[0];
        const aId = tupledArg[1];
        return (tupledArg_1) => {
            const b = tupledArg_1[0];
            const bId = tupledArg_1[1];
            return (compare(a, b) < 0) ? [a, aId] : [b, bId];
        };
    };
    const wireDist = map((w) => [fold((currMin, s) => min(currMin)([orthDist(s.Vertices[0], s.Vertices[1], pos), s.Id]), [orthDist(head(w.Segments).Vertices[0], head(w.Segments).Vertices[1], pos), head(w.Segments).Id], w.Segments), w], wModel.WX);
    const sortedDist = sortBy((tupledArg_2) => {
        const x = tupledArg_2[0];
        return x[0];
    }, wireDist, {
        Compare: comparePrimitives,
    });
    const matchValue = head(sortedDist);
    if (w_1 = matchValue[1], (sId = matchValue[0][1], (d = matchValue[0][0], d < 10))) {
        const w_2 = matchValue[1];
        const sId_1 = matchValue[0][1];
        const d_1 = matchValue[0][0];
        return [w_2.Id, sId_1];
    }
    else {
        return void 0;
    }
}

export function basicRouting(s1, s2) {
    let x, y, y_2, x_2, y_4, y_6;
    const srcX = s1.X;
    const srcY = s1.Y;
    const tgtX = s2.X;
    const tgtY = s2.Y;
    const midX = 0.5 * (srcX + tgtX);
    const midY = 0.5 * (srcY + tgtY);
    const makeSegment = (x1, y1, x2, y2) => (new WireSegment(uuid(), [new XYPos(x1, y1), new XYPos(x2, y2)], new XYPos(0, 0), false, new HighLightColor(5)));
    const matchValue = tgtX - srcX;
    if (x = matchValue, x >= 0) {
        const x_1 = matchValue;
        const matchValue_1 = tgtY - srcY;
        if (y = matchValue_1, y !== 0) {
            const y_1 = matchValue_1;
            const segList = empty();
            const s1_1 = makeSegment(srcX, srcY, midX, srcY);
            const s2_1 = makeSegment(midX, srcY, midX, tgtY);
            const s3 = makeSegment(midX, tgtY, tgtX, tgtY);
            return ofArrayWithTail([s1_1, s2_1, s3], segList);
        }
        else if (y_2 = matchValue_1, y_2 === 0) {
            const y_3 = matchValue_1;
            const segList_1 = empty();
            const s1_2 = makeSegment(srcX, srcY, midX, srcY);
            return cons(s1_2, segList_1);
        }
        else {
            return toFail(printf("Not implemented"));
        }
    }
    else if (x_2 = matchValue, x_2 < 0) {
        const x_3 = matchValue;
        const matchValue_2 = tgtY - srcY;
        if (y_4 = matchValue_2, y_4 < 0) {
            const y_5 = matchValue_2;
            const segList_2 = empty();
            const s1_3 = makeSegment(srcX, srcY, srcX + 60, srcY);
            const s2_2 = makeSegment(srcX + 60, srcY, srcX + 60, srcY - 60);
            const s3_1 = makeSegment(srcX + 60, srcY - 60, tgtX - 60, tgtY - 60);
            const s4 = makeSegment(tgtX - 60, tgtY - 60, tgtX - 60, tgtY);
            const s5 = makeSegment(tgtX - 60, tgtY, tgtX, tgtY);
            return ofArrayWithTail([s1_3, s2_2, s3_1, s4, s5], segList_2);
        }
        else if (y_6 = matchValue_2, y_6 >= 0) {
            const y_7 = matchValue_2;
            const segList_3 = empty();
            const s1_4 = makeSegment(srcX, srcY, srcX + 60, srcY);
            const s2_3 = makeSegment(srcX + 60, srcY, srcX + 60, srcY + 60);
            const s3_2 = makeSegment(srcX + 60, srcY + 60, tgtX - 60, tgtY + 60);
            const s4_1 = makeSegment(tgtX - 60, tgtY + 60, tgtX - 60, tgtY);
            const s5_1 = makeSegment(tgtX - 60, tgtY, tgtX, tgtY);
            return ofArrayWithTail([s1_4, s2_3, s3_2, s4_1, s5_1], segList_3);
        }
        else {
            return toFail(printf("Not implemented"));
        }
    }
    else {
        return toFail(printf("Not implemented"));
    }
}

export function updateWires(model) {
    const wireList = map((w) => {
        const segList = basicRouting(symbolPos(model.Symbol, w.SrcSymbol), symbolPos(model.Symbol, w.TargetSymbol));
        return new Wire(w.Id, w.SrcSymbol, w.TargetSymbol, segList);
    }, model.WX);
    return new Model(model.Symbol, wireList, model.Color);
}

export function autoRouting(model) {
    return toFail(printf("Not implemented"));
}

export function singleWireView(wModel) {
    return FunctionComponent_Of_Z5A158BBF((props) => {
        const wire_1 = props.WireP;
        const wireSegments = map((segment) => {
            const srcX = segment.Vertices[0].X;
            const srcY = segment.Vertices[0].Y;
            const tgtX = segment.Vertices[1].X;
            const tgtY = segment.Vertices[1].Y;
            return react.createElement("g", {}, react.createElement("polyline", {
                onMouseDown: (ev) => {
                    const mousePos = posOf(ev.pageX, ev.pageY);
                    const matchValue = wireToSelectOpt(wModel, mousePos);
                    if (matchValue == null) {
                        props.Dispatch(new Msg(2, new HighLightColor(0)));
                    }
                    else {
                        const x = matchValue;
                        props.Dispatch(new Msg(2, new HighLightColor(1)));
                    }
                },
                onMouseUp: (ev_1) => {
                    props.Dispatch(new Msg(2, new HighLightColor(1)));
                },
                points: toText(printf("%f %f, %f %f"))(srcX)(srcY)(tgtX)(tgtY),
                stroke: props.ColorP,
                fillOpacity: 0,
                strokeWidth: props.StrokeWidthP,
            }), react.createElement("text", {
                x: srcX + 40,
                y: srcY - 20,
                style: {
                    textAnchor: "middle",
                    dominantBaseline: "hanging",
                    fontSize: "18px",
                    fontWeight: "Bold",
                    fill: "Blue",
                },
            }, toText(printf("0..x"))));
        }, wire_1.Segments);
        return react.createElement("g", {}, ...append(empty(), wireSegments));
    }, void 0, uncurry(2, void 0), void 0, "singleWireView", "/home/cwlroda/projects/hlp21qs/hlp21-indiv-05-SH-wlc3317/src/Renderer/BusWire.fs", 200);
}

export function view(model, dispatch) {
    const wires = map((w) => {
        const props = new WireRenderProps(w.Id, w, symbolPos(model.Symbol, w.SrcSymbol), symbolPos(model.Symbol, w.TargetSymbol), HighLightColor__Text(model.Color), "2px", dispatch);
        return singleWireView(model)(props);
    }, updateWires(model).WX);
    const symbols = view_1(model.Symbol, (sMsg) => {
        dispatch(new Msg(0, sMsg));
    });
    return react.createElement("g", {}, react.createElement("g", {}, ...wires), symbols);
}

export function init(n, unitVar1) {
    const patternInput = init_1();
    const symbols = patternInput[0];
    const cmd = patternInput[1];
    const symIds = map((sym) => sym.Id, symbols);
    const rng = {};
    const makeRandomWire = () => {
        let r2, r1;
        const n_1 = length(symIds) | 0;
        let patternInput_1;
        const matchValue = [randomNext(0, n_1 - 1), randomNext(0, n_1 - 2)];
        if (r2 = (matchValue[1] | 0), (r1 = (matchValue[0] | 0), r1 === r2)) {
            const r2_1 = matchValue[1] | 0;
            const r1_1 = matchValue[0] | 0;
            patternInput_1 = [item(r1_1, symbols), item(n_1 - 1, symbols)];
        }
        else {
            const r2_2 = matchValue[1] | 0;
            const r1_2 = matchValue[0] | 0;
            patternInput_1 = [item(r1_2, symbols), item(r2_2, symbols)];
        }
        const s2 = patternInput_1[1];
        const s1 = patternInput_1[0];
        return new Wire(uuid(), s1.Id, s2.Id, basicRouting(posOf(s1.Component.X, s1.Component.Y), posOf(s2.Component.X, s2.Component.Y)));
    };
    return [new Model(symbols, map((i) => makeRandomWire(), ofSeq(rangeNumber(1, 1, n))), new HighLightColor(5)), Cmd_none()];
}

export function update(msg, model) {
    switch (msg.tag) {
        case 1: {
            return toFail(printf("Not implemented"));
        }
        case 2: {
            const c = msg.fields[0];
            return [new Model(model.Symbol, model.WX, c), Cmd_none()];
        }
        case 3: {
            const mMsg = msg.fields[0];
            return [model, Cmd_OfFunc_result(new Msg(0, new Msg_1(0, mMsg)))];
        }
        case 4: {
            const wId = msg.fields[0];
            const sId = msg.fields[1];
            const pagePos = msg.fields[2];
            const wireList = map((w) => {
                let x;
                if (x = wId, equals(x, w.Id)) {
                    const x_1 = wId;
                    const segList = map((s) => {
                        let y;
                        if (y = sId, equals(y, s.Id)) {
                            const y_1 = sId;
                            return new WireSegment(s.Id, s.Vertices, pagePos, true, s.Color);
                        }
                        else {
                            return s;
                        }
                    }, w.Segments);
                    return new Wire(w.Id, w.SrcSymbol, w.TargetSymbol, segList);
                }
                else {
                    return w;
                }
            }, model.WX);
            return [new Model(model.Symbol, wireList, model.Color), Cmd_none()];
        }
        case 5: {
            const wId_1 = msg.fields[0];
            const sId_1 = msg.fields[1];
            const pagePos_1 = msg.fields[2];
            const wireList_1 = map((w_1) => {
                let x_2;
                if (x_2 = wId_1, equals(x_2, w_1.Id)) {
                    const x_3 = wId_1;
                    const segList_1 = map((s_1) => {
                        let y_2;
                        if (y_2 = sId_1, equals(y_2, s_1.Id)) {
                            const y_3 = sId_1;
                            const diff = posDiff(pagePos_1, s_1.LastDragPos);
                            return new WireSegment(s_1.Id, [posAdd(s_1.Vertices[0], diff), posAdd(s_1.Vertices[1], diff)], pagePos_1, s_1.IsDragging, s_1.Color);
                        }
                        else {
                            return s_1;
                        }
                    }, w_1.Segments);
                    return new Wire(w_1.Id, w_1.SrcSymbol, w_1.TargetSymbol, segList_1);
                }
                else {
                    return w_1;
                }
            }, model.WX);
            return [new Model(model.Symbol, wireList_1, model.Color), Cmd_none()];
        }
        case 6: {
            const wId_2 = msg.fields[0];
            const sId_2 = msg.fields[1];
            const wireList_2 = map((w_2) => {
                let x_4;
                if (x_4 = wId_2, equals(x_4, w_2.Id)) {
                    const x_5 = wId_2;
                    const segList_2 = map((s_2) => {
                        let y_4;
                        if (y_4 = sId_2, equals(y_4, s_2.Id)) {
                            const y_5 = sId_2;
                            return new WireSegment(s_2.Id, s_2.Vertices, s_2.LastDragPos, false, s_2.Color);
                        }
                        else {
                            return s_2;
                        }
                    }, w_2.Segments);
                    return new Wire(w_2.Id, w_2.SrcSymbol, w_2.TargetSymbol, segList_2);
                }
                else {
                    return w_2;
                }
            }, model.WX);
            return [new Model(model.Symbol, wireList_2, model.Color), Cmd_none()];
        }
        default: {
            const sMsg = msg.fields[0];
            const patternInput = update_1(sMsg, model.Symbol);
            const sm = patternInput[0];
            const sCmd = patternInput[1];
            return [new Model(sm, model.WX, model.Color), Cmd_map((arg0) => (new Msg(0, arg0)), sCmd)];
        }
    }
}

export function extractWire(wModel, sId) {
    return toFail(printf("Not implemented"));
}

export function extractWires(wModel) {
    return toFail(printf("Not implemented"));
}

export function updateSymbolModelWithComponent(symModel, comp) {
    return toFail(printf("Not Implemented"));
}

