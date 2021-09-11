module Native

open System
open System.Runtime.InteropServices

[<DllImport("User32.dll")>]
extern bool RegisterHotKey(IntPtr hWnd, int id, uint fsModifiers, uint vk)

let MOD_ALT = 0x1u
let MOD_CONTROL = 0x2u
let MOD_SHIFT = 0x4u
let MOD_WIN = 0x8u
let MOD_NOREPEAT = 0x4000u
let mutable hotkeyId = 0;
let registerHotkey mods key =
    hotkeyId <- hotkeyId + 1
    RegisterHotKey(IntPtr.Zero, hotkeyId, mods, key)
    
module Hook =
    type KeyboardHookStruct =
        struct
            val vkCode: int32
            val scanCode: int32
            val flags: int32
            val time: int32
            val dwExtraInfo: int32
        end
        
    type HookProc = delegate of int * int * IntPtr -> IntPtr
    
    let WH_KEYBOARD_LL = 13
    type KeyState =
        | WM_KEYDOWN = 0x0100
        | WM_KEYUP = 0x0101
    
    [<DllImport("User32.dll", SetLastError = true)>]
    extern IntPtr SetWindowsHookEx(int id, HookProc proc, IntPtr hInstance, uint32 threadId)
    
    [<DllImport "User32.dll">]
    extern IntPtr UnhookWindowsHookEx(IntPtr hInstance)
    [<DllImport "User32.dll">]
    extern IntPtr CallNextHookEx(IntPtr id, int nCode, int wParam, IntPtr lParam)
    [<DllImport "kernel32.dll">]
    extern IntPtr LoadLibrary(string lpFileName)
    [<DllImport "User32.dll">]
    extern uint16 GetKeyState(int nVirtKey)
    
    [<DllImport "User32.dll">]
    extern uint16 GetAsyncKeyState(int vKey)
    
    let dbg key state (extstate: uint16) =
        printfn "%A: %b (%s)" key state (Convert.ToString(extstate |> int, 2))
    
    let isKeyDown (key: Keystroke.T) =
        let state = GetKeyState(int key)
        
        if (state &&& 0x8000us) = 0x8000us then
//            dbg key true state
            true
        else
//            dbg key false state
            false
            
module KeyboardInput =
    [<StructLayout(LayoutKind.Sequential)>]
    type private MOUSEINPUT = struct
        val dx: int32
        val dy:int32
        val mouseData:uint32
        val dwFlags: uint32
        val time: uint32
        val dwExtraInfo: UIntPtr
        new(_dx, _dy, _mouseData, _dwFlags, _time, _dwExtraInfo) = {dx=_dx; dy=_dy; mouseData=_mouseData; dwFlags=_dwFlags; time=_time; dwExtraInfo=_dwExtraInfo}
    end
    
    [<StructLayout(LayoutKind.Sequential)>]
    type private KEYBDINPUT = struct
        val wVk: uint16
        val wScan: uint16
        val dwFlags: uint32
        val time: uint32
        val dwExtraInfo: UIntPtr
        new(_wVk, _wScan, _dwFlags, _time, _dwExtraInfo) = {wVk =_wVk; wScan = _wScan; dwFlags = _dwFlags; time = _time; dwExtraInfo = _dwExtraInfo}
    end
    
    [<StructLayout(LayoutKind.Sequential)>]
    type private HARDWAREINPUT = struct
        val uMsg: uint32
        val wParamL: uint16
        val wParamH: uint16
        new(_uMsg, _wParamL, _wParamH) = {uMsg = _uMsg; wParamL = _wParamL; wParamH = _wParamH}
    end
    
    
    [<StructLayout(LayoutKind.Explicit)>]
    type private InputUnion = struct
        [<FieldOffset(0)>]
        val mutable mi : MOUSEINPUT
    
        [<FieldOffset(0)>]
        val mutable ki : KEYBDINPUT
    
        [<FieldOffset(0)>]
        val mutable hi : HARDWAREINPUT 
    end
    
    [<StructLayout(LayoutKind.Sequential)>]
    type private LPINPUT  = struct
        val mutable ``type``: int // 1 is keyboard
        val mutable u: InputUnion
    end
    
    module private Native =
        [<DllImport("user32.dll", SetLastError=true)>]
        extern uint32 SendInput(uint32 nInputs, LPINPUT* pInputs, int cbSize)
        
    let KEYEVENTF_KEYUP = 0x0002u
    let KEYEVENTF_KEYDOWN = 0x0000u 
    
    let tapKey (key: Keystroke.T) =
        let mutable down = LPINPUT()
        down.``type`` <- 1
        down.u.ki <- KEYBDINPUT(uint16 key, uint16 0, KEYEVENTF_KEYDOWN, uint32 0, UIntPtr.Zero)
        Native.SendInput(uint32 1, &&down, Marshal.SizeOf(down))
        |> ignore
        
        let mutable up = LPINPUT()
        up.``type`` <- 1
        up.u.ki <- KEYBDINPUT(uint16 key, uint16 0, KEYEVENTF_KEYUP, uint32 0, UIntPtr.Zero)
        Native.SendInput(uint32 1, &&up, Marshal.SizeOf(down))
        |> ignore
        
        
    
module MessagePump =
    [<AutoOpen>]
    module private Native =
        type Point =
            struct
                val x: int64
                val y: int64
            end
            
        type WinMsg =
            struct
                val hwnd: IntPtr
                val message: uint32
                val wParam: IntPtr
                val lParam: IntPtr
                val time: uint32
                val pt: Point
            end
            
        [<DllImport "user32.dll">]
        extern int GetMessage(WinMsg& lpmessage, IntPtr hWnd, uint32 wMsgFilterMin, uint32 wMsgFilterMax)
        
        [<DllImport "user32.dll">]
        extern bool TranslateMessage(WinMsg& lpMsg)
        
        [<DllImport "user32.dll">]
        extern IntPtr DispatchMessage(WinMsg& lpMsg)

    let pumpQueue () =
       let mutable msg: WinMsg = WinMsg()
       
       while GetMessage(&msg, IntPtr.Zero, 0u, 0u) > 0 do
           TranslateMessage(&msg) |> ignore
           DispatchMessage(&msg) |> ignore
