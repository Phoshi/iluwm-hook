module IluwmKeyboardHook

open HotkeyDefinitions

let mutable mode = "default"
let mutable majorMode = "default"

let longest (hotkeys:HotkeyDefinitions.T list) =
    let length (hk: HotkeyDefinitions.T) =
        hk.mods.Length
        
    hotkeys
    |> List.sortByDescending length
    |> List.tryHead
let matchedHotkey (hotkeys: HotkeyDefinitions.T list) (key: Keypress.T): HotkeyDefinitions.T option =
    let modifiersMatch (mods: Modifier list) =
        let modDown modifier =
            match modifier with
            | Control -> key.control
            | Shift -> key.shift
            | Windows -> key.meta
            
        List.forall
            modDown
            mods
            
    let _keyPressed (hk: HotkeyDefinitions.T) =
        hk.key = key.key && modifiersMatch hk.mods
            
    hotkeys
    |> List.where _keyPressed
    |> longest

let inMode mode (hotkeys: HotkeyDefinitions.T list) =
    hotkeys
    |> List.filter (fun h -> h.mode = mode)
    
let hook (hotkeys: HotkeyDefinitions.T list) (key: Keypress.T) =
    match matchedHotkey (inMode mode hotkeys) key with
    | Some hotkey ->
        match hotkey.hookInstructions with
        | Some ["minor"; m] ->
            mode <- m
            true 
        | _ -> 
            printfn "Running %s(%s): %s" (LowLevelKeyboardHook.prettyPrint key) hotkey.command (IPC.command hotkey.command)
            mode <- majorMode
            true
    | None -> false
    

