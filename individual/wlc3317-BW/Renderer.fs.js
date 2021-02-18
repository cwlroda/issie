import { uncurry, jsOptions } from "./.fable/fable-library.3.1.0/Util.js";
import { init, view, update, Msg, KeyboardMsg } from "./Sheet.fs.js";
import * as electron from "electron";
import { map } from "./.fable/fable-library.3.1.0/Array.js";
import { Cmd_ofSub, Cmd_map } from "./.fable/Fable.Elmish.3.1.0/cmd.fs.js";
import { recordExecutionTimeStats } from "./Helpers.fs.js";
import { toConsole, printf, toText } from "./.fable/fable-library.3.1.0/String.js";
import { ProgramModule_mkProgram, ProgramModule_withSubscription, ProgramModule_withTrace, ProgramModule_run } from "./.fable/Fable.Elmish.3.1.0/program.fs.js";
import { Program_withReactBatched } from "./.fable/Fable.Elmish.React.3.0.1/react.fs.js";

export function editMenu(dispatch) {
    let menuSeparator;
    const sep = {};
    sep.type = "separator";
    menuSeparator = sep;
    const makeRoleItem = (role) => jsOptions((item) => {
        item.role = role;
    });
    const makeKeyItem = (label, accelerator, action) => jsOptions((item_1) => {
        item_1.label = label;
        item_1.accelerator = accelerator;
        item_1.click = ((_arg3, _arg2, _arg1) => {
            action();
        });
    });
    return jsOptions((invisibleMenu) => {
        invisibleMenu.type = "submenu";
        invisibleMenu.label = "Edit";
        invisibleMenu.visible = true;
        invisibleMenu.submenu = [makeKeyItem("Default", "CmdOrCtrl+S", () => {
            dispatch(new KeyboardMsg(0));
        }), makeKeyItem("Blue", "Alt+C", () => {
            dispatch(new KeyboardMsg(1));
        }), makeKeyItem("Green", "Alt+V", () => {
            dispatch(new KeyboardMsg(2));
        }), makeKeyItem("Default", "delete", () => {
            dispatch(new KeyboardMsg(5));
        }), makeKeyItem("Red", "Alt+Z", () => {
            dispatch(new KeyboardMsg(3));
        }), menuSeparator, makeKeyItem("Print Statistics", "Alt+Shift+Z", () => {
            dispatch(new KeyboardMsg(4));
        }), makeRoleItem("forceReload"), makeRoleItem("reload"), makeRoleItem("toggleDevTools"), makeRoleItem("zoomIn"), makeRoleItem("zoomOut")];
    });
}

export function attachMenusAndKeyShortcuts(dispatch) {
    const sub = (dispatch_1) => {
        const menu = electron.remote.Menu.buildFromTemplate(map((arg0) => arg0, [editMenu(dispatch_1)]));
        menu.items[0].visible = true;
        electron.remote.app.applicationMenu = menu;
    };
    return Cmd_map((arg0_1) => (new Msg(1, arg0_1)), Cmd_ofSub(sub));
}

export function update$0027(msg) {
    return (arg) => recordExecutionTimeStats("Update", (model) => update(msg, model), arg);
}

export const view$0027 = (arg) => recordExecutionTimeStats("View", (model) => ((dispatch) => view(model, dispatch)), arg);

export function printMsg(msg) {
    let pattern_matching_result, busWireMouseMsg, key, symMouseMsg, x;
    if (msg.tag === 1) {
        pattern_matching_result = 1;
        key = msg.fields[0];
    }
    else if (msg.fields[0].tag === 3) {
        pattern_matching_result = 0;
        busWireMouseMsg = msg.fields[0].fields[0];
    }
    else if (msg.fields[0].tag === 0) {
        if (msg.fields[0].fields[0].tag === 0) {
            pattern_matching_result = 2;
            symMouseMsg = msg.fields[0].fields[0].fields[0];
        }
        else {
            pattern_matching_result = 3;
            x = msg;
        }
    }
    else {
        pattern_matching_result = 3;
        x = msg;
    }
    switch (pattern_matching_result) {
        case 0: {
            return toText(printf("BusWireMsg:%A"))(busWireMouseMsg.Op);
        }
        case 1: {
            return toText(printf("%A"))(key);
        }
        case 2: {
            return toText(printf("SymbolMsg:%A"))(symMouseMsg.Op);
        }
        case 3: {
            return toText(printf("Other:%A"))(x);
        }
    }
}

export function traceFn(msg, model) {
    const arg10 = printMsg(msg);
    toConsole(printf("Msg=%A\n\n"))(arg10);
}

ProgramModule_run(ProgramModule_withTrace((msg_1, model) => {
    traceFn(msg_1, model);
}, ProgramModule_withSubscription(attachMenusAndKeyShortcuts, Program_withReactBatched("app", ProgramModule_mkProgram(init, uncurry(2, update$0027), uncurry(2, view$0027))))));

