namespace Aardvark.Application

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open System.Collections.Concurrent

[<Flags>]
type MouseButtons =
    | None    = 0x0000
    | Left    = 0x0001
    | Right   = 0x0002
    | Middle  = 0x0004
    | Button4 = 0x0008
    | Button5 = 0x0010
    | Button6 = 0x0020
    | Button7 = 0x0040
    | Button8 = 0x0080

type IMouse =
    abstract member Position : aval<PixelPosition>
    abstract member IsDown : MouseButtons -> aval<bool>
    abstract member TotalScroll : aval<float>
    abstract member Inside : aval<bool>

    abstract member Down : IEvent<MouseButtons>
    abstract member Up : IEvent<MouseButtons>
    abstract member Move : IEvent<PixelPosition * PixelPosition>
    abstract member Click : IEvent<MouseButtons>
    abstract member DoubleClick : IEvent<MouseButtons>
    abstract member Scroll : IEvent<float>
    abstract member Enter : IEvent<PixelPosition>
    abstract member Leave : IEvent<PixelPosition>

type internal MouseClickState() =
    static let clickSize = V2i(4)
    static let doubleClickTime = TimeSpan.FromMilliseconds 500.0

    let mutable count = 0
    let mutable prevTime = DateTime()
    let mutable prevPosition = V2i.Zero

    let isDoubleClick (now: DateTime) (position: PixelPosition) =
        let dp = abs (position.Position - prevPosition)
        now - prevTime < doubleClickTime && Vec.allSmallerOrEqual dp clickSize

    member this.Down(position: PixelPosition) =
        lock this (fun _ ->
            let now = DateTime.Now
            count <- if isDoubleClick now position then count + 1 else 1
            prevTime <- now
            prevPosition <- position.Position
        )

    member this.Up(position: PixelPosition) =
        lock this (fun _ ->
            if isDoubleClick DateTime.Now position then
                if count % 2 = 0 then 2 else 1
            else
                0
        )

type EventMouse(autoGenerateClickEvents : bool) =
    let position = AVal.init <| PixelPosition()
    let buttons = ConcurrentDictionary<MouseButtons, cval<bool>>()
    let scroll = cval 0.0
    let inside = cval false

    let downEvent = EventSource<MouseButtons>()
    let upEvent = EventSource<MouseButtons>()
    let clickEvent = EventSource<MouseButtons>()
    let doubleClickEvent = EventSource<MouseButtons>()
    let scrollEvent = EventSource<float>()
    let enterEvent = EventSource<PixelPosition>()
    let leaveEvent = EventSource<PixelPosition>()
    let moveEvent = EventSource<PixelPosition * PixelPosition>()

    let setPos (p : PixelPosition) =
        if p <> position.GetValue() then
            position.Value <- p

    let getDown button =
        buttons.GetOrAdd(button, fun b -> AVal.init false)

    let getClickState =
        if autoGenerateClickEvents then
            let store = ConcurrentDictionary<MouseButtons, MouseClickState>()
            fun btn -> store.GetOrAdd(btn, fun _ -> MouseClickState())
        else
            Unchecked.defaultof<_>

    let handleDown (pos : PixelPosition) (b : MouseButtons) =
        if autoGenerateClickEvents then
            let state = getClickState b
            state.Down pos

        downEvent.Emit b

    let handleUp (pos : PixelPosition) (b : MouseButtons) =
        if autoGenerateClickEvents then
            let state = getClickState b
            match state.Up pos with
            | 1 -> clickEvent.Emit b
            | 2 -> doubleClickEvent.Emit b
            | _ -> ()

        upEvent.Emit b

    abstract member Down : PixelPosition * MouseButtons -> unit
    abstract member Up : PixelPosition * MouseButtons -> unit
    abstract member Click : PixelPosition * MouseButtons -> unit
    abstract member DoubleClick : PixelPosition * MouseButtons -> unit
    abstract member Scroll : PixelPosition * float -> unit
    abstract member Enter : PixelPosition -> unit
    abstract member Leave : PixelPosition -> unit
    abstract member Move : PixelPosition -> unit

    default x.Down(pos : PixelPosition, b : MouseButtons) =
        let m = getDown b
        transact (fun () -> m.Value <- true; setPos pos)
        handleDown pos b

    default x.Up(pos : PixelPosition, b : MouseButtons) =
        let m = getDown b
        transact (fun () -> m.Value <- false; setPos pos)
        handleUp pos b

    default x.Click(pos : PixelPosition, b : MouseButtons) =
        transact (fun () -> setPos pos)
        clickEvent.Emit b

    default x.DoubleClick(pos : PixelPosition, b : MouseButtons) =
        transact (fun () -> setPos pos)
        doubleClickEvent.Emit b

    default x.Scroll(pos : PixelPosition, b : float) =
        transact (fun () -> scroll.Value <- scroll.Value + b; setPos pos)
        scrollEvent.Emit b

    default x.Enter(p : PixelPosition) =
        transact (fun () -> inside.Value <- true; setPos p)
        enterEvent.Emit p

    default x.Leave(p : PixelPosition) =
        transact (fun () -> inside.Value <- false; setPos p)
        leaveEvent.Emit p

    default x.Move(p : PixelPosition) =
        let last = position.GetValue()
        transact (fun () -> setPos p)
        moveEvent.Emit ((last, p))

    member x.IsDown button = getDown button :> aval<_>

    member x.Use(o : IMouse) =
        let pos = o.Position
        let loc() = AVal.force pos
        let subscriptions =
            [
                o.Down.Values.Subscribe(fun v -> x.Down(loc(), v))
                o.Up.Values.Subscribe(fun v -> x.Up(loc(), v))
                o.Click.Values.Subscribe(fun v -> x.Click(loc(), v))
                o.DoubleClick.Values.Subscribe(fun v -> x.DoubleClick(loc(), v))
                o.Scroll.Values.Subscribe(fun v -> x.Scroll(loc(), v))
                o.Enter.Values.Subscribe(x.Enter)
                o.Leave.Values.Subscribe(x.Leave)
                o.Move.Values.Subscribe(fun (_,n) -> x.Move(n))
            ]

        { new IDisposable with
            member x.Dispose() = subscriptions |> List.iter (fun i -> i.Dispose()) 
        }

    interface IMouse with
        member x.Position = position :> aval<_>
        member x.IsDown button = x.IsDown button
        member x.TotalScroll = scroll :> aval<_>
        member x.Inside = inside :> aval<_>
        member x.Down = downEvent :> IEvent<_>
        member x.Up = upEvent :> IEvent<_>
        member x.Move = moveEvent :> IEvent<_>
        member x.Click = clickEvent :> IEvent<_>
        member x.DoubleClick = doubleClickEvent :> IEvent<_>
        member x.Scroll = scrollEvent :> IEvent<_>
        member x.Enter = enterEvent :> IEvent<_>
        member x.Leave = leaveEvent :> IEvent<_>