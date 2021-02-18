import { toString, Union, Record } from "./.fable/fable-library.3.1.0/Types.js";
import { uuid, XYPos, MouseT$reflection, XYPos$reflection } from "./Helpers.fs.js";
import { string_type, lambda_type, unit_type, union_type, record_type, obj_type, bool_type } from "./.fable/fable-library.3.1.0/Reflection.js";
import { Component as Component_3, ComponentType, Port, PortType, Port$reflection, Component$reflection } from "./CommonTypes.fs.js";
import { equals, comparePrimitives, max, randomNext } from "./.fable/fable-library.3.1.0/Util.js";
import { toText, printf, toFail } from "./.fable/fable-library.3.1.0/String.js";
import { find, append, fold, head, filter, cons, ofArray, length, ofSeq, map } from "./.fable/fable-library.3.1.0/List.js";
import { value, some } from "./.fable/fable-library.3.1.0/Option.js";
import { allPairs, rangeNumber } from "./.fable/fable-library.3.1.0/Seq.js";
import { Cmd_none } from "./.fable/Fable.Elmish.3.1.0/cmd.fs.js";
import { FunctionComponent_Of_Z5A158BBF } from "./.fable/Fable.React.7.0.1/Fable.React.FunctionComponent.fs.js";
import * as react from "react";
import { Helpers_equalsButFunctions } from "./.fable/Fable.React.7.0.1/Fable.React.Helpers.fs.js";

export class Symbol$ extends Record {
    constructor(LastDragPos, IsDragging, Id, Component, Selected) {
        super();
        this.LastDragPos = LastDragPos;
        this.IsDragging = IsDragging;
        this.Id = Id;
        this.Component = Component;
        this.Selected = Selected;
    }
}

export function Symbol$$reflection() {
    return record_type("Symbol.Symbol", [], Symbol$, () => [["LastDragPos", XYPos$reflection()], ["IsDragging", bool_type], ["Id", obj_type], ["Component", Component$reflection()], ["Selected", bool_type]]);
}

export class Msg extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["MouseMsg", "StartDragging", "Dragging", "EndDragging", "AddSymbol", "DeleteSymbol", "UpdateSymbolModelWithComponent", "SetSelected", "MouseOverPort"];
    }
}

export function Msg$reflection() {
    return union_type("Symbol.Msg", [], Msg, () => [[["Item", MouseT$reflection()]], [["sId", obj_type], ["pagePos", XYPos$reflection()]], [["sId", obj_type], ["pagePos", XYPos$reflection()]], [["sId", obj_type]], [["comp", Component$reflection()], ["pos", XYPos$reflection()]], [["sId", obj_type]], [["Item", Component$reflection()]], [["topLeft", XYPos$reflection()], ["bottomRight", XYPos$reflection()]], [["port", Port$reflection()]]]);
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

export function withinSelectedBoundary(compTopLeft, compBotRight, boundTopLeft, boundBotRight) {
    let point2, point1;
    const matchValue = [compTopLeft, compBotRight];
    if (point2 = matchValue[1], (point1 = matchValue[0], (((point1.X >= boundTopLeft.X) ? (point2.X <= boundBotRight.X) : false) ? (point1.Y >= boundTopLeft.Y) : false) ? (point2.Y <= boundBotRight.Y) : false)) {
        const point2_1 = matchValue[1];
        const point1_1 = matchValue[0];
        return true;
    }
    else {
        return false;
    }
}

export const rng = {};

export function createNewSymbol(comp) {
    let matchValue;
    return new Symbol$(new XYPos(0, 0), false, uuid(), comp, (matchValue = (randomNext(0, 2) | 0), (matchValue === 0) ? false : ((matchValue === 1) ? true : toFail(printf("not implemented")))));
}

export const testComponent = (() => {
    const compId = uuid();
    const inputPorts = map((x) => (new Port(uuid(), some(x), new PortType(0), compId, false)), ofSeq(rangeNumber(0, 1, randomNext(0, 10))));
    const outputPorts = map((x_1) => (new Port(uuid(), some(x_1), new PortType(1), compId, false)), ofSeq(rangeNumber(0, 1, randomNext(0, 10))));
    return new Component_3(compId, new ComponentType(7), "Hello", inputPorts, outputPorts, 20, 30, (25 + (max(comparePrimitives, length(inputPorts), length(outputPorts)) * 15)) + 5, 50);
})();

export function init() {
    const createTestList = (testComponent_1, tupledArg) => {
        const xIn = tupledArg[0];
        const yIn = tupledArg[1];
        return new Component_3(testComponent_1.Id, testComponent_1.Type, testComponent_1.Label, testComponent_1.InputPorts, testComponent_1.OutputPorts, testComponent_1.X + xIn, testComponent_1.Y + yIn, testComponent_1.H, testComponent_1.W);
    };
    return [map((arg) => createNewSymbol(createTestList(testComponent, arg)), ofSeq(allPairs(ofArray([100, 200, 300]), ofArray([40, 150, 300])))), Cmd_none()];
}

export function setSelectedFunction(topLeft, topRight, model) {
    return map((sym) => {
        if (withinSelectedBoundary(new XYPos(sym.Component.X, sym.Component.Y), new XYPos(sym.Component.X + sym.Component.W, sym.Component.Y + sym.Component.H), topLeft, topRight)) {
            return new Symbol$(sym.LastDragPos, sym.IsDragging, sym.Id, sym.Component, true);
        }
        else {
            return new Symbol$(sym.LastDragPos, sym.IsDragging, sym.Id, sym.Component, false);
        }
    }, model);
}

export function update(msg, model) {
    let x_1, x_2;
    switch (msg.tag) {
        case 4: {
            const pos = msg.fields[1];
            const comp = msg.fields[0];
            return [cons(createNewSymbol(comp), model), Cmd_none()];
        }
        case 5: {
            const sId = msg.fields[0];
            return [filter((sym) => (!equals(sym.Id, sId)), model), Cmd_none()];
        }
        case 7: {
            const topRight = msg.fields[1];
            const topLeft = msg.fields[0];
            return [setSelectedFunction(topLeft, topRight, model), Cmd_none()];
        }
        case 1: {
            const sId_1 = msg.fields[0];
            const pagePos = msg.fields[1];
            const sIdSymbol = head(filter((x) => equals(x.Id, sId_1), model));
            const startDrag = (list_1) => map((sym_1) => {
                if (sym_1.Selected) {
                    return new Symbol$(pagePos, true, sym_1.Id, sym_1.Component, sym_1.Selected);
                }
                else {
                    return sym_1;
                }
            }, list_1);
            return [(x_1 = sIdSymbol, !x_1.Selected) ? (x_2 = sIdSymbol, startDrag(setSelectedFunction(new XYPos(x_2.Component.X, x_2.Component.Y), new XYPos(x_2.Component.X + x_2.Component.W, x_2.Component.Y + x_2.Component.H), model))) : startDrag(model), Cmd_none()];
        }
        case 2: {
            const rank = msg.fields[0];
            const pagePos_1 = msg.fields[1];
            return [map((sym_2) => {
                let inputRecord;
                if (sym_2.Selected) {
                    const diff = posDiff(pagePos_1, sym_2.LastDragPos);
                    return new Symbol$(pagePos_1, sym_2.IsDragging, sym_2.Id, (inputRecord = sym_2.Component, new Component_3(inputRecord.Id, inputRecord.Type, inputRecord.Label, inputRecord.InputPorts, inputRecord.OutputPorts, sym_2.Component.X + diff.X, sym_2.Component.Y + diff.Y, inputRecord.H, inputRecord.W)), sym_2.Selected);
                }
                else {
                    return sym_2;
                }
            }, model), Cmd_none()];
        }
        case 3: {
            const sId_2 = msg.fields[0];
            return [map((sym_3) => {
                if (sym_3.Selected) {
                    return new Symbol$(sym_3.LastDragPos, false, sym_3.Id, sym_3.Component, sym_3.Selected);
                }
                else {
                    return sym_3;
                }
            }, model), Cmd_none()];
        }
        case 8: {
            const port = msg.fields[0];
            const portType = port.PortType;
            return [map((sym_4) => {
                let inputRecord_1, inputRecord_2;
                if (equals(sym_4.Component.Id, port.HostId)) {
                    if (portType.tag === 0) {
                        return new Symbol$(sym_4.LastDragPos, sym_4.IsDragging, sym_4.Id, (inputRecord_1 = sym_4.Component, new Component_3(inputRecord_1.Id, inputRecord_1.Type, inputRecord_1.Label, map((checkPort) => {
                            if (equals(checkPort, port)) {
                                return new Port(checkPort.PortId, checkPort.PortNumber, checkPort.PortType, checkPort.HostId, true);
                            }
                            else {
                                return checkPort;
                            }
                        }, sym_4.Component.InputPorts), inputRecord_1.OutputPorts, inputRecord_1.X, inputRecord_1.Y, inputRecord_1.H, inputRecord_1.W)), sym_4.Selected);
                    }
                    else {
                        return new Symbol$(sym_4.LastDragPos, sym_4.IsDragging, sym_4.Id, (inputRecord_2 = sym_4.Component, new Component_3(inputRecord_2.Id, inputRecord_2.Type, inputRecord_2.Label, inputRecord_2.InputPorts, map((checkPort_1) => {
                            if (equals(checkPort_1, port)) {
                                return new Port(checkPort_1.PortId, checkPort_1.PortNumber, checkPort_1.PortType, checkPort_1.HostId, true);
                            }
                            else {
                                return checkPort_1;
                            }
                        }, sym_4.Component.OutputPorts), inputRecord_2.X, inputRecord_2.Y, inputRecord_2.H, inputRecord_2.W)), sym_4.Selected);
                    }
                }
                else {
                    return sym_4;
                }
            }, model), Cmd_none()];
        }
        case 0: {
            return [model, Cmd_none()];
        }
        default: {
            return toFail(printf("Not implemented"));
        }
    }
}

class RenderSymbolProps extends Record {
    constructor(Symbol$, Dispatch, key) {
        super();
        this.Symbol = Symbol$;
        this.Dispatch = Dispatch;
        this.key = key;
    }
}

function RenderSymbolProps$reflection() {
    return record_type("Symbol.RenderSymbolProps", [], RenderSymbolProps, () => [["Symbol", Symbol$$reflection()], ["Dispatch", lambda_type(Msg$reflection(), unit_type)], ["key", string_type]]);
}

function renderSymbol(model) {
    return FunctionComponent_Of_Z5A158BBF((props) => {
        const handleMouseMove = react.useRef((ev) => {
            const ev_1 = ev;
            props.Dispatch(new Msg(2, props.Symbol.Id, posOf(ev_1.pageX, ev_1.pageY)));
        });
        const color = props.Symbol.IsDragging ? "green" : "grey";
        const topLeft = new XYPos(props.Symbol.Component.X, props.Symbol.Component.Y);
        const topRight = new XYPos(props.Symbol.Component.X + props.Symbol.Component.W, props.Symbol.Component.Y);
        const bottomRight = new XYPos(props.Symbol.Component.X + props.Symbol.Component.W, props.Symbol.Component.Y + props.Symbol.Component.H);
        const bottomLeft = new XYPos(props.Symbol.Component.X, props.Symbol.Component.Y + props.Symbol.Component.H);
        let componentName;
        const matchValue = props.Symbol.Component.Type;
        switch (matchValue.tag) {
            case 6:
            case 9: {
                componentName = "\u0026";
                break;
            }
            case 5: {
                componentName = "1";
                break;
            }
            case 7:
            case 10: {
                componentName = "≥";
                break;
            }
            case 8:
            case 11: {
                componentName = "=1";
                break;
            }
            default: {
                throw (new Error("The match cases were incomplete against type of \u0027ComponentType\u0027 at /home/cwlroda/projects/hlp21qs/hlp21-indiv-05-SH-wlc3317/src/Renderer/Symbol.fs"));
            }
        }
        let invertedOutput;
        const matchValue_1 = props.Symbol.Component.Type;
        switch (matchValue_1.tag) {
            case 10:
            case 9:
            case 5:
            case 11: {
                invertedOutput = true;
                break;
            }
            default: {
                invertedOutput = false;
            }
        }
        const inputPorts = props.Symbol.Component.InputPorts;
        const outputPorts = props.Symbol.Component.OutputPorts;
        let viewPorts;
        const generateLines = (portList) => map((x) => {
            const dynamicContent = (x.PortType.tag === 1) ? [topRight.X - 7, "right"] : [topLeft.X + 2, "left"];
            let portNumber;
            const matchValue_3 = x.PortNumber;
            if (matchValue_3 == null) {
                portNumber = 0;
            }
            else {
                const a = value(matchValue_3);
                portNumber = a;
            }
            return react.createElement("text", {
                onMouseUp: (ev_2) => {
                    document.removeEventListener("mousemove", handleMouseMove.current);
                    props.Dispatch(new Msg(3, props.Symbol.Id));
                },
                onMouseDown: (ev_3) => {
                    const multipleSelection = fold((acc, elem) => (elem.Selected ? (acc + 1) : acc), 0, model) | 0;
                    if (multipleSelection <= 1) {
                        props.Dispatch(new Msg(7, topLeft, bottomRight));
                    }
                    props.Dispatch(new Msg(1, props.Symbol.Id, posOf(ev_3.pageX, ev_3.pageY)));
                    document.addEventListener("mousemove", handleMouseMove.current);
                },
                x: dynamicContent[0],
                y: ((topRight.Y + 25) + 5) + (portNumber * 15),
                style: {
                    textAnchor: dynamicContent[1],
                    dominantBaseline: "hanging",
                    fontSize: "10px",
                    fontWeight: "Bold",
                    fill: "Blue",
                    userSelect: "none",
                },
            }, toText(printf("%i"))(portNumber));
        }, portList);
        viewPorts = append(generateLines(inputPorts), generateLines(outputPorts));
        let viewPortLines;
        const generateLines_1 = (portList_1) => map((x_1) => {
            const lineLength = 20;
            const dynamicContent_1 = (x_1.PortType.tag === 1) ? [topRight, topRight.X + lineLength] : [topLeft, topLeft.X - lineLength];
            let portNumber_1;
            const matchValue_5 = x_1.PortNumber;
            if (matchValue_5 == null) {
                portNumber_1 = 0;
            }
            else {
                const a_1 = value(matchValue_5);
                portNumber_1 = a_1;
            }
            return react.createElement("line", {
                onMouseOver: (ev_4) => {
                    props.Dispatch(new Msg(8, x_1));
                },
                x1: dynamicContent_1[0].X,
                x2: dynamicContent_1[1],
                y2: (dynamicContent_1[0].Y + 35) + (portNumber_1 * 15),
                y1: (dynamicContent_1[0].Y + 35) + (portNumber_1 * 15),
                fill: color,
                stroke: color,
                strokeWidth: 2,
            });
        }, portList_1);
        viewPortLines = append(generateLines_1(inputPorts), generateLines_1(outputPorts));
        const viewBox = ofArray([react.createElement("polygon", {
            onMouseUp: (ev_5) => {
                document.removeEventListener("mousemove", handleMouseMove.current);
                props.Dispatch(new Msg(3, props.Symbol.Id));
            },
            onMouseDown: (ev_6) => {
                const multipleSelection_1 = fold((acc_1, elem_1) => (elem_1.Selected ? (acc_1 + 1) : acc_1), 0, model) | 0;
                if (multipleSelection_1 <= 1) {
                    props.Dispatch(new Msg(7, topLeft, bottomRight));
                }
                props.Dispatch(new Msg(1, props.Symbol.Id, posOf(ev_6.pageX, ev_6.pageY)));
                document.addEventListener("mousemove", handleMouseMove.current);
            },
            points: toText(printf("%f %f, %f %f, %f %f , %f %f"))(topLeft.X)(topLeft.Y)(topRight.X)(topRight.Y)(bottomRight.X)(bottomRight.Y)(bottomLeft.X)(bottomLeft.Y),
            fill: color,
            stroke: color,
            strokeWidth: 2,
        }), react.createElement("text", {
            onMouseUp: (ev_7) => {
                document.removeEventListener("mousemove", handleMouseMove.current);
                props.Dispatch(new Msg(3, props.Symbol.Id));
            },
            onMouseDown: (ev_8) => {
                const multipleSelection_2 = fold((acc_2, elem_2) => (elem_2.Selected ? (acc_2 + 1) : acc_2), 0, model) | 0;
                if (multipleSelection_2 <= 1) {
                    props.Dispatch(new Msg(7, topLeft, bottomRight));
                }
                props.Dispatch(new Msg(1, props.Symbol.Id, posOf(ev_8.pageX, ev_8.pageY)));
                document.addEventListener("mousemove", handleMouseMove.current);
            },
            x: topLeft.X + (0.5 * (topRight.X - topLeft.X)),
            y: topLeft.Y + 3,
            style: {
                textAnchor: "middle",
                userSelect: "none",
            },
        }, toString(props.Symbol.Selected)), react.createElement("text", {
            onMouseUp: (ev_9) => {
                document.removeEventListener("mousemove", handleMouseMove.current);
                props.Dispatch(new Msg(3, props.Symbol.Id));
            },
            onMouseDown: (ev_10) => {
                const multipleSelection_3 = fold((acc_3, elem_3) => (elem_3.Selected ? (acc_3 + 1) : acc_3), 0, model) | 0;
                if (multipleSelection_3 <= 1) {
                    props.Dispatch(new Msg(7, topLeft, bottomRight));
                }
                props.Dispatch(new Msg(1, props.Symbol.Id, posOf(ev_10.pageX, ev_10.pageY)));
                document.addEventListener("mousemove", handleMouseMove.current);
            },
            x: topLeft.X + 2,
            y: topLeft.Y + 15,
            style: {
                textAnchor: "left",
                userSelect: "none",
            },
        }, toText(printf("IN"))), react.createElement("text", {
            onMouseUp: (ev_11) => {
                document.removeEventListener("mousemove", handleMouseMove.current);
                props.Dispatch(new Msg(3, props.Symbol.Id));
            },
            onMouseDown: (ev_12) => {
                const multipleSelection_4 = fold((acc_4, elem_4) => (elem_4.Selected ? (acc_4 + 1) : acc_4), 0, model) | 0;
                if (multipleSelection_4 <= 1) {
                    props.Dispatch(new Msg(7, topLeft, bottomRight));
                }
                props.Dispatch(new Msg(1, props.Symbol.Id, posOf(ev_12.pageX, ev_12.pageY)));
                document.addEventListener("mousemove", handleMouseMove.current);
            },
            x: topRight.X - 11,
            y: topLeft.Y + 15,
            style: {
                textAnchor: "middle",
                userSelect: "none",
            },
        }, toText(printf("OUT")))]);
        const outputPortList = ofSeq(rangeNumber(0, 1, length(props.Symbol.Component.OutputPorts) - 1));
        const inputPortList = ofSeq(rangeNumber(0, 1, length(props.Symbol.Component.InputPorts) - 1));
        const output = new PortType(1);
        const input = new PortType(0);
        const viewOverall = append(viewBox, append(viewPorts, viewPortLines));
        return react.createElement("g", {
            style: {
                dominantBaseline: "hanging",
                fontSize: "10px",
                fontWeight: "Bold",
                fill: "Blue",
            },
        }, ...viewOverall);
    }, "Component", Helpers_equalsButFunctions, void 0, "renderSymbol", "/home/cwlroda/projects/hlp21qs/hlp21-indiv-05-SH-wlc3317/src/Renderer/Symbol.fs", 272);
}

export function view(model, dispatch) {
    return Array.from(map((_arg1) => {
        const symbol = _arg1;
        const id = symbol.Id;
        return renderSymbol(model)(new RenderSymbolProps(symbol, dispatch, id));
    }, model));
}

export function symbolPos(symModel, sId) {
    const sym_1 = find((sym) => equals(sym.Id, sId), symModel);
    return new XYPos(sym_1.Component.X, sym_1.Component.Y);
}

export function updateSymbolModelWithComponent(symModel, comp) {
    return toFail(printf("Not Implemented"));
}

export function calculateOutputWidth(wId, outputPortNumber, inputPortWidths) {
    return toFail(printf("Not implemented"));
}

export function extractComponent(symModel, sId) {
    return toFail(printf("Not implemented"));
}

export function extractComponents(symModel) {
    return toFail(printf("Not implemented"));
}

