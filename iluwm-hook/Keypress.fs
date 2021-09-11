module Keypress

type T = {
    key: int
    control: bool
    shift: bool
    meta: bool
    alt: bool
}

let private _create key c s m a =
    {
        key = key
        control = c
        shift = s
        meta = m
        alt = a
    }
    
let down keys =
    keys
    |> List.exists Native.Hook.isKeyDown
    
let create key =
    _create
        key
        (down [Keystroke.T.LControlKey; Keystroke.T.RControlKey; Keystroke.T.Control; Keystroke.T.ControlKey])
        (down [Keystroke.T.LShiftKey; Keystroke.T.RShiftKey; Keystroke.T.Shift; Keystroke.T.ShiftKey])
        (down [Keystroke.T.LWin; Keystroke.T.RWin])
        (down [Keystroke.T.Alt; Keystroke.T.RWin])
        