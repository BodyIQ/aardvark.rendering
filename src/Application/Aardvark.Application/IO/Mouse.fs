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

    let downPosAndTime = System.Collections.Generic.Dictionary<MouseButtons, int * DateTime * PixelPosition>()

    let DoubleClickTime = 500
    let DoubleClickSizeWidth = 4
    let DoubleClickSizeHeight = 4

    let handleDown (pos : PixelPosition) (b : MouseButtons) =
        match downPosAndTime.TryGetValue b with
        | true, (oc, ot, op) ->
            let dt = DateTime.Now - ot
            let dp = pos.Position - op.Position

            if dt.TotalMilliseconds < float DoubleClickTime && abs dp.X <= DoubleClickSizeWidth && abs dp.Y < DoubleClickSizeHeight then
                downPosAndTime.[b] <- (oc + 1, DateTime.Now, pos)
            else
                downPosAndTime.[b] <- (1, DateTime.Now, pos)

        | _ ->
            downPosAndTime.[b] <- (1, DateTime.Now, pos)

        downEvent.Emit(b)

    let handleUp (pos : PixelPosition) (b : MouseButtons) =
        match downPosAndTime.TryGetValue b with
        | true, (c, t, p) ->
            let dt = DateTime.Now - t
            let dp = pos.Position - p.Position

            if autoGenerateClickEvents then
                if dt.TotalMilliseconds < float DoubleClickTime && abs dp.X <= DoubleClickSizeWidth && abs dp.Y < DoubleClickSizeHeight then
                    if c < 2 then clickEvent.Emit(b)
                    else doubleClickEvent.Emit(b)
                else
                    ()
        | _ ->
            ()
        upEvent.Emit(b)

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