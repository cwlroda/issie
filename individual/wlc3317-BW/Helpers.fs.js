import { Union, Record } from "./.fable/fable-library.3.1.0/Types.js";
import { union_type, record_type, float64_type } from "./.fable/fable-library.3.1.0/Reflection.js";
import { v4 } from "uuid";
import { op_Subtraction, toNumber, fromBits, op_Division } from "./.fable/fable-library.3.1.0/Long.js";
import { getTicks, now } from "./.fable/fable-library.3.1.0/Date.js";
import { createAtom, max, comparePrimitives, min } from "./.fable/fable-library.3.1.0/Util.js";
import { toList, tryFind, add, ofSeq } from "./.fable/fable-library.3.1.0/Map.js";
import { map, defaultArg } from "./.fable/fable-library.3.1.0/Option.js";
import { iterate } from "./.fable/fable-library.3.1.0/List.js";
import { printf, toConsole } from "./.fable/fable-library.3.1.0/String.js";

export class XYPos extends Record {
    constructor(X, Y) {
        super();
        this.X = X;
        this.Y = Y;
    }
}

export function XYPos$reflection() {
    return record_type("Helpers.XYPos", [], XYPos, () => [["X", float64_type], ["Y", float64_type]]);
}

export class MouseOp extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Up", "Down", "Move", "Drag"];
    }
}

export function MouseOp$reflection() {
    return union_type("Helpers.MouseOp", [], MouseOp, () => [[], [], [], []]);
}

export class MouseT extends Record {
    constructor(Pos, Op) {
        super();
        this.Pos = Pos;
        this.Op = Op;
    }
}

export function MouseT$reflection() {
    return record_type("Helpers.MouseT", [], MouseT, () => [["Pos", XYPos$reflection()], ["Op", MouseOp$reflection()]]);
}

export const uuid = v4;

export function timeNowInMicroS() {
    let copyOfStruct;
    return op_Division((copyOfStruct = now(), getTicks(copyOfStruct)), fromBits(10, 0, false));
}

export class Stats extends Record {
    constructor(Min, Max, Av, Num) {
        super();
        this.Min = Min;
        this.Max = Max;
        this.Av = Av;
        this.Num = Num;
    }
}

export function Stats$reflection() {
    return record_type("Helpers.Stats", [], Stats, () => [["Min", float64_type], ["Max", float64_type], ["Av", float64_type], ["Num", float64_type]]);
}

export function addTimeToStats(t, st) {
    return new Stats(min(comparePrimitives, st.Min, t), max(comparePrimitives, st.Max, t), ((st.Av * st.Num) + t) / (st.Num + 1), st.Num + 1);
}

export const executionStats = createAtom(ofSeq([], {
    Compare: comparePrimitives,
}));

export function recordExecutionTimeStats(name, f, arg) {
    let value_1;
    const timeLimit = 0;
    const t1 = timeNowInMicroS();
    const execTime = () => (toNumber(op_Subtraction(timeNowInMicroS(), t1)) / 1000);
    const res = f(arg);
    let iterations = 1;
    while (execTime() < timeLimit) {
        iterations = (iterations + 1);
        const value = f(arg);
        void value;
    }
    const t = execTime() / iterations;
    executionStats(add(name, (value_1 = (new Stats(t, t, t, 1)), defaultArg(map((st) => addTimeToStats(t, st), tryFind(name, executionStats())), value_1)), executionStats()), true);
    return res;
}

export function printStats() {
    iterate((tupledArg) => {
        const name = tupledArg[0];
        const st = tupledArg[1];
        const arg50 = (~(~st.Num)) | 0;
        toConsole(printf("%s time: min=%.3fms max=%.3fms av=%.3fms samples:%d"))(name)(st.Min)(st.Max)(st.Av)(arg50);
    }, toList(executionStats()));
    executionStats(ofSeq([], {
        Compare: comparePrimitives,
    }), true);
}

export const canvasUnscaledDimensions = new XYPos(1000, 1000);

