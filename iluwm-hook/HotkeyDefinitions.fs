module HotkeyDefinitions

open Microsoft.FSharpLu.Json

type T = {
    mods: Modifier list
    key: int
    command: string
    mode: string
    hookInstructions: string list option
}
and Modifier = Shift | Windows | Control

type WireFormat = item list
and item = int list * int * string * string list option * string

let deserialise text =
    Compact.deserialize<WireFormat> text
    
let LWin = 0x5b
let LShiftKey = 0xA0
let LControlKey = 0xA2
    
let translate mods =
    let _translate m =
        match m with
        | k when k = LWin -> Windows
        | k when k = LShiftKey -> Shift
        | k when k = LControlKey -> Control
        | _ -> failwith "panic!" 
    mods
    |> List.map _translate
    
let format (wf: WireFormat) =
    wf
    |> List.map (fun (mods, key, command, hookInstructions, mode) -> { mods = translate mods; key = key; command = command; hookInstructions = hookInstructions; mode = mode })
    
let hotkeys () =
    IPC.query "hotkeys"
    |> deserialise
    |> format