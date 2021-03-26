module Renderer

    open Elmish
    open Elmish.React
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Browser
    
    open Helpers
    open Electron
    
  
    
    
    open Sheet

    

    
    // Editor Keybindings (also items on Edit menu)
    // Use Elmish subscriptions to attach external source of events such as keyboard
    // shortcuts. According to electron documentation, the way to configure keyboard
    // shortcuts is by creating a menu.
    let menuSeparator =
       let sep = createEmpty<MenuItemOptions>
       sep.``type`` <- MenuItemType.Separator
       sep
    let makeRoleItem (role:MenuItemRole) =
        jsOptions<MenuItemOptions> <| fun item ->
            item.role <- role
    let makeKeyItem (label:string) (accelerator : string) (action : unit -> unit) =
        jsOptions<MenuItemOptions> <| fun item ->
            item.label <- label
            item.accelerator <- accelerator
            item.click <- fun _ _ _ -> action()
    
    let keyInputMenu dispatch =
        let makeKeyInput (accelerator: string) (action: unit->unit) =
            makeKeyItem accelerator accelerator action

        jsOptions<MenuItemOptions> <| fun invisibleMenu ->
            invisibleMenu.``type`` <- MenuItemType.SubMenu
            invisibleMenu.label <- ""
            // this option isn't working
            invisibleMenu.visible <- false // false if you want keys but no "Edit" menu
            invisibleMenu.submenu <-
                [| makeKeyInput "CancelAction" (fun () -> dispatch <| Interface InterfaceMsg.CancelAction)
                   makeKeyInput "Alt+A" (fun () -> dispatch <| Interface InterfaceMsg.SelectAll)
                   makeKeyInput "Ctrl+Shift+=" (fun () -> dispatch <| Interface (InterfaceMsg.Zoom ZoomRequest.In))
                   makeKeyInput "Ctrl+-" (fun () -> dispatch <| Interface (InterfaceMsg.Zoom ZoomRequest.Out))
                   makeKeyInput "Ctrl+=" (fun () -> dispatch <| Interface (InterfaceMsg.Zoom ZoomRequest.Reset))
                   makeKeyInput "Alt+N" (fun () -> dispatch (
                                                        let newComp = Symbol.createSpecificComponent (posOf 0. 0.) (Symbol.createCompType (Symbol.rng.Next(0,25))) (Symbol.randomName ())

                                                        Sheet.Interface <| InterfaceMsg.CreateObjects {Symbols=[newComp];Wires=[]}
                                                   ))
                |]
                |> U2.Case1

    let editMenu dispatch =
        jsOptions<MenuItemOptions> <| fun invisibleMenu ->
            invisibleMenu.``type`` <- MenuItemType.SubMenu
            invisibleMenu.label <- "Edit"
            invisibleMenu.visible <- true // false if you want keys but no "Edit" menu
            invisibleMenu.submenu <-
                [| makeKeyItem "Blue" "Alt+C" (fun () -> dispatch <| Interface InterfaceMsg.Copy)
                   makeKeyItem "Green" "Alt+V" (fun () -> dispatch <| Interface InterfaceMsg.Paste)
                   makeKeyItem "Default"  "delete" (fun () -> dispatch <| Interface InterfaceMsg.DeleteSelected)
                   makeKeyItem "Red" "Alt+Z" (fun () -> dispatch <| Interface InterfaceMsg.Undo)
                   makeKeyItem "Toggle Debug" "Alt+Shift+D" (fun () -> dispatch <| Interface InterfaceMsg.ToggleWireDebug)
                   menuSeparator
                   makeKeyItem "Print Statistics" "Alt+Shift+Z" (fun () -> dispatch <| Interface InterfaceMsg.Redo)
                   makeRoleItem MenuItemRole.ForceReload
                   makeRoleItem MenuItemRole.Reload
                   makeRoleItem MenuItemRole.ToggleDevTools
                |]
                |> U2.Case1
    
    let attachMenusAndKeyShortcuts dispatch =
        let sub dispatch =
            let menu = 
                [| editMenu dispatch
                   keyInputMenu dispatch |]          
                |> Array.map U2.Case1
                |> electron.remote.Menu.buildFromTemplate   
            menu.items.[0].visible <- Some true
            menu.items.[1].visible <- Some false
            electron.remote.app.applicationMenu <- Some menu
    
        Cmd.ofSub sub   

    let update' = fun msg -> recordExecutionTimeStats "Update" (Sheet.update msg)
    let view'  = recordExecutionTimeStats "View" Sheet.view
    let printMsg (msg:Msg) =
        match msg with
        | Interface key -> sprintf "%A" key
        | MouseMsg (symMouseMsg, _) -> sprintf "SymbolMsg:%A" symMouseMsg.Op
        | x -> sprintf "Other:%A" x

    let traceFn (msg:Msg) model = printfn "Msg=%A\n\n" (printMsg msg)
    // App
    Program.mkProgram Sheet.init update' view'
    |> Program.withReactBatched "app"
    |> Program.withSubscription attachMenusAndKeyShortcuts
    //|> Program.withTrace traceFn
    //|> Program.withConsoleTrace
    |> Program.run

