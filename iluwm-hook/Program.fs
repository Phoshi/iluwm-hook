// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Native
open HotkeyDefinitions

[<EntryPoint>]
let main argv =
    let hotkeys = HotkeyDefinitions.hotkeys ()
    
    let hook = LowLevelKeyboardHook.hook (IluwmKeyboardHook.hook hotkeys)
    
    MessagePump.pumpQueue ()
    0 // return an integer exit code