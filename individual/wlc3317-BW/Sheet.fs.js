import { Union, Record } from "./.fable/fable-library.3.1.0/Types.js";
import { init as init_1, update as update_1, view as view_1, Msg as Msg_1, Msg$reflection as Msg$reflection_1, Model$reflection as Model$reflection_1 } from "./BusWire.fs.js";
import { union_type, record_type } from "./.fable/fable-library.3.1.0/Reflection.js";
import { toConsole, printf, toText } from "./.fable/fable-library.3.1.0/String.js";
import { printStats, MouseOp, MouseT, XYPos } from "./Helpers.fs.js";
import * as react from "react";
import { keyValueList } from "./.fable/fable-library.3.1.0/MapUtil.js";
import { CSSProp } from "./.fable/Fable.React.7.0.1/Fable.React.Props.fs.js";
import { singleton } from "./.fable/fable-library.3.1.0/List.js";
import { Cmd_map, Cmd_OfFunc_result, Cmd_none } from "./.fable/Fable.Elmish.3.1.0/cmd.fs.js";
import { HighLightColor } from "./CommonTypes.fs.js";

export class Model extends Record {
    constructor(Wire) {
        super();
        this.Wire = Wire;
    }
}

export function Model$reflection() {
    return record_type("Sheet.Model", [], Model, () => [["Wire", Model$reflection_1()]]);
}

export class KeyboardMsg extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["CtrlS", "AltC", "AltV", "AltZ", "AltShiftZ", "DEL"];
    }
}

export function KeyboardMsg$reflection() {
    return union_type("Sheet.KeyboardMsg", [], KeyboardMsg, () => [[], [], [], [], [], []]);
}

export class Msg extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Wire", "KeyPress"];
    }
}

export function Msg$reflection() {
    return union_type("Sheet.Msg", [], Msg, () => [[["Item", Msg$reflection_1()]], [["Item", KeyboardMsg$reflection()]]]);
}

export const zoom = 1;

export function displaySvgWithZoom(zoom_1, svgReact, dispatch) {
    let css_2;
    let sizeInPixels;
    const arg10 = 1000 * zoom_1;
    sizeInPixels = toText(printf("%.2fpx"))(arg10);
    const mDown = (ev) => {
        if (ev.buttons !== 0) {
            return true;
        }
        else {
            return false;
        }
    };
    const mouseOp = (op, ev_1) => {
        dispatch(new Msg(0, new Msg_1(3, new MouseT(new XYPos(ev_1.x / zoom_1, ev_1.y / zoom_1), op))));
    };
    return react.createElement("div", {
        style: {
            height: "100vh",
            maxWidth: "100vw",
            overflowX: "auto",
            overflowY: "auto",
        },
        onMouseDown: (ev_2) => {
            mouseOp(new MouseOp(1), ev_2);
        },
        onMouseUp: (ev_3) => {
            mouseOp(new MouseOp(0), ev_3);
        },
        onMouseMove: (ev_4) => {
            mouseOp(mDown(ev_4) ? (new MouseOp(3)) : (new MouseOp(2)), ev_4);
        },
    }, react.createElement("svg", {
        style: {
            border: "3px solid green",
            height: sizeInPixels,
            width: sizeInPixels,
        },
    }, react.createElement("g", keyValueList([(css_2 = singleton(new CSSProp(367, toText(printf("scale(%f)"))(zoom_1))), ["style", keyValueList(css_2, 1)])], 1), react.createElement("text", {
        x: 500,
        y: 50,
        style: {
            textAnchor: "middle",
            dominantBaseline: "middle",
            fontSize: "40px",
            fontWeight: "Bold",
            fill: "Green",
        },
    }, "sample text"), svgReact, react.createElement("polygon", {
        points: "10,10 900,900 10,900",
        strokeWidth: "5px",
        stroke: "Black",
        fillOpacity: 0.1,
        fill: "Blue",
    }))));
}

export function view(model, dispatch) {
    const wDispatch = (wMsg) => {
        dispatch(new Msg(0, wMsg));
    };
    const wireSvg = view_1(model.Wire, wDispatch);
    return displaySvgWithZoom(zoom, wireSvg, dispatch);
}

export function update(msg, model) {
    if (msg.tag === 1) {
        if (msg.fields[0].tag === 4) {
            printStats();
            return [model, Cmd_none()];
        }
        else {
            const s = msg.fields[0];
            const c = (s.tag === 1) ? (new HighLightColor(1)) : ((s.tag === 2) ? (new HighLightColor(3)) : ((s.tag === 3) ? (new HighLightColor(0)) : (new HighLightColor(5))));
            toConsole(printf("Key:%A"))(c);
            return [model, Cmd_OfFunc_result(new Msg(0, new Msg_1(2, c)))];
        }
    }
    else {
        const wMsg = msg.fields[0];
        const patternInput = update_1(wMsg, model.Wire);
        const wModel = patternInput[0];
        const wCmd = patternInput[1];
        return [new Model(wModel), Cmd_map((arg0) => (new Msg(0, arg0)), wCmd)];
    }
}

export function init() {
    const patternInput = init_1(400, void 0);
    const model = patternInput[0];
    const cmds = patternInput[1];
    return [new Model(model), Cmd_map((arg0) => (new Msg(0, arg0)), cmds)];
}

