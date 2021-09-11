module LowLevelKeyboardHook
open System
open System.Runtime.InteropServices
open Native
open Native.Hook

let DoNotCallMoreHooks = IntPtr 1

let mutable hookProc: HookProc option = None

let mutable currentlyDown: int list = []
let mutable eaten: int list = []
let mutable eatNextWinKey: bool = false


let isInList ls (key: Keypress.T) =
    List.contains key.key ls
    
let addToList f ls (key: Keypress.T) =
    f (key.key :: ls)
    
let removeFromList f ls (key: Keypress.T) =
    f (List.where ((<>) key.key) ls)
    
let isDown k = isInList currentlyDown k
let registerDown k = addToList (fun l -> currentlyDown <- l) currentlyDown k
let registerUp k = removeFromList (fun l -> currentlyDown <- l) currentlyDown k

let isEaten k = isInList eaten k
let registerEaten k = addToList (fun l -> eaten <- l) eaten k
let registerDigested k = removeFromList (fun l -> eaten <- l) eaten k

let purgeEatenKeys () =
    eaten <-
        eaten
        |> List.filter (fun k ->
            let key = Keystroke.create k
            let isDown = Native.Hook.isKeyDown key
            if not isDown then
                printfn "Purging erroneously undigested key %A" key
            isDown)
        
let purgePressedKeys () =
    currentlyDown <-
        currentlyDown
        |> List.filter (fun k ->
            let key = Keystroke.create k
            let isDown = Native.Hook.isKeyDown key
            if not isDown then
                printfn "Purging erroneously down key %A" key
            isDown)

let prettyPrint (key: Keypress.T) =
    let m p c = if p then c else ""
    sprintf "%s%s%s%s%A" (m key.alt "!") (m key.control "^") (m key.meta "#") (m key.shift "+") (Keystroke.create key.key)
    
let isWindowsKey (key: Keypress.T) =
    match key.key |> Keystroke.create with
    | Keystroke.T.LWin -> true
    | Keystroke.T.RWin -> true
    | _ -> false

let hookWith f : HookProc =
    let _hookWith code wParam lParam =
        printf "llkh"
        if code < 0 then
            CallNextHookEx(IntPtr.Zero, code, wParam, lParam)
        else
            purgeEatenKeys ()
            purgePressedKeys ()
            let keypress = (Keypress.create (Marshal.ReadInt32 lParam))
            printfn " %s: %s" (prettyPrint keypress) (if wParam = (int KeyState.WM_KEYDOWN) then "DOWN" else "UP")
            
            let eat =
                if wParam = (int KeyState.WM_KEYDOWN) then
                    if not <| isDown keypress then
                        registerDown keypress
                        
                        f keypress
                    else if isEaten keypress then
                        true
                    else 
                        false
                else
                    registerUp keypress
                    false
                
            if eat then
                registerEaten keypress
                if keypress.meta then
                    eatNextWinKey <- true
                DoNotCallMoreHooks
            else
                if wParam = (int KeyState.WM_KEYUP) && isEaten keypress then
                    printfn "Digesting %i" keypress.key
                    registerDigested keypress
                    DoNotCallMoreHooks
                else if wParam = (int KeyState.WM_KEYUP) && isWindowsKey keypress then
                    printfn "Suppressing windows key..."
                    KeyboardInput.tapKey Keystroke.T.Mask
                    eatNextWinKey <- false
                    
                    CallNextHookEx(IntPtr.Zero, code, wParam, lParam)
                else
                    CallNextHookEx(IntPtr.Zero, code, wParam, lParam)
            
    HookProc _hookWith

let hook proc =
    let u32Handle = LoadLibrary "User32"
    let hookproc = hookWith proc
    
    hookProc <- Some hookproc
    
    SetWindowsHookEx(WH_KEYBOARD_LL, hookproc, u32Handle, 0u)
    
let unhook hook =
    UnhookWindowsHookEx(hook)

