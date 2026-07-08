namespace Aardvark.Application.WinForms

open System
open System.Windows.Forms
open Aardvark.Base
open Aardvark.Application

type private WinFormsButtons = System.Windows.Forms.MouseButtons

type Mouse() as this =
    inherit EventMouse(false)
    let mutable ctrl : Option<Control> = None

    let size() =
        match ctrl with
            | Some ctrl -> V2i(ctrl.ClientSize.Width, ctrl.ClientSize.Height)
            | _ -> V2i.Zero

    let (~%) (m : WinFormsButtons) =
        let mutable buttons = MouseButtons.None

        if m.HasFlag WinFormsButtons.Left then
            buttons <- buttons ||| MouseButtons.Left

        if m.HasFlag WinFormsButtons.Right then
            buttons <- buttons ||| MouseButtons.Right

        if m.HasFlag WinFormsButtons.Middle then
            buttons <- buttons ||| MouseButtons.Middle

        if m.HasFlag WinFormsButtons.XButton1 then
            buttons <- buttons ||| MouseButtons.Button4

        if m.HasFlag WinFormsButtons.XButton2 then
            buttons <- buttons ||| MouseButtons.Button5

        buttons

    let (~%%) (e : MouseEventArgs) =
        let s = size()
        let pp = PixelPosition(e.X, e.Y, s.X, s.Y)
        pp

    let mousePos() =
         match ctrl with
            | Some ctrl -> 
                let p = ctrl.PointToClient(Control.MousePosition)
                let s = ctrl.ClientSize
        
                let x = clamp 0 (s.Width-1) p.X
                let y = clamp 0 (s.Height-1) p.Y

                PixelPosition(x, y, ctrl.ClientSize.Width, ctrl.ClientSize.Height)
            | _ ->
                PixelPosition(0,0,0,0)


    let onMouseDownHandler = MouseEventHandler(fun s e -> this.Down(%%e, %e.Button))
    let onMouseUpHandler = MouseEventHandler(fun s e -> this.Up (%%e, %e.Button))
    let onMouseMoveHandler = MouseEventHandler(fun s e -> this.Move %%e)
    let onMouseWheelHandler = MouseEventHandler(fun s e -> this.Scroll (%%e, (float e.Delta)))
    let onMouseEnter = EventHandler(fun s e -> this.Enter (mousePos()))
    let onMouseLeave = EventHandler(fun s e -> this.Leave (mousePos()))
    let onMouseClickHandler = MouseEventHandler(fun s e -> this.Click(%%e, %e.Button))
    let onMouseDoubleClickHandler = MouseEventHandler(fun s e -> this.DoubleClick(%%e, %e.Button))

    let addHandlers() =
        match ctrl with
            | Some ctrl ->
                ctrl.MouseDown.AddHandler onMouseDownHandler
                ctrl.MouseUp.AddHandler onMouseUpHandler
                ctrl.MouseMove.AddHandler onMouseMoveHandler
                ctrl.MouseWheel.AddHandler onMouseWheelHandler
                ctrl.MouseEnter.AddHandler onMouseEnter
                ctrl.MouseLeave.AddHandler onMouseLeave
                ctrl.MouseClick.AddHandler onMouseClickHandler
                ctrl.MouseDoubleClick.AddHandler onMouseDoubleClickHandler
            | _ ->()

    let removeHandlers() =
        match ctrl with
            | Some ctrl ->
                ctrl.MouseDown.RemoveHandler onMouseDownHandler
                ctrl.MouseUp.RemoveHandler onMouseUpHandler
                ctrl.MouseMove.RemoveHandler onMouseMoveHandler
                ctrl.MouseWheel.RemoveHandler onMouseWheelHandler
                ctrl.MouseEnter.RemoveHandler onMouseEnter
                ctrl.MouseLeave.RemoveHandler onMouseLeave
                ctrl.MouseClick.RemoveHandler onMouseClickHandler
                ctrl.MouseDoubleClick.RemoveHandler onMouseDoubleClickHandler
            | None -> ()

    member x.SetControl(c : Control) =
        removeHandlers()
        ctrl <- Some c
        addHandlers()
        
    member x.DragMouse pX pY =
        let s = size()
        let pp = PixelPosition(pX, pY, s.X, s.Y)
        this.Move pp

    member x.Dispose() = removeHandlers()

    interface IDisposable with
        member x.Dispose() = x.Dispose()