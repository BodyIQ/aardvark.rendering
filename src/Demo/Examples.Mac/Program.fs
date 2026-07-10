open System
open System.Threading.Tasks
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.Glfw
open FSharp.Data.Adaptive

let private createTriangleScene () =
    let positions =
        [|
            V3f(-0.7f, -0.55f, 0.0f)
            V3f( 0.7f, -0.55f, 0.0f)
            V3f( 0.0f,  0.65f, 0.0f)
        |]

    let colors = [| C4b.Red; C4b.Green; C4b.Blue |]

    Sg.draw IndexedGeometryMode.TriangleList
    |> Sg.vertexAttribute DefaultSemantic.Positions (AVal.constant positions)
    |> Sg.vertexAttribute DefaultSemantic.Colors (AVal.constant colors)
    |> Sg.shader {
        do! DefaultSurfaces.vertexColor
    }

let private createLodScene () =
    let low =
        Sg.box' C4b.DarkCyan (Box3d.FromCenterAndSize(V3d.Zero, V3d.III))
        |> Sg.effect [
            DefaultSurfaces.trafo |> toEffect
            DefaultSurfaces.constantColor C4f.DarkCyan |> toEffect
            DefaultSurfaces.simpleLighting |> toEffect
        ]

    let high =
        Sg.unitSphere' 5 C4b.Orange
        |> Sg.effect [
            DefaultSurfaces.trafo |> toEffect
            DefaultSurfaces.constantColor C4f.Orange |> toEffect
            DefaultSurfaces.simpleLighting |> toEffect
        ]

    Sg.lod (fun scope -> Vec.length (scope.cameraPosition - scope.bb.Center) < 4.0) low high

let private withCamera (win : Window) (scene : ISg) =
    let initialView = CameraView.lookAt (V3d(2.8, 2.8, 2.3)) V3d.Zero V3d.OOI
    let view = DefaultCameraController.control win.Mouse win.Keyboard win.Time initialView

    let proj =
        win.Sizes
        |> AVal.map (fun s ->
            let aspect =
                if s.Y = 0 then 1.0
                else float s.X / float s.Y

            Frustum.perspective 60.0 0.1 100.0 aspect
        )

    scene
    |> Sg.viewTrafo (view |> AVal.map CameraView.viewTrafo)
    |> Sg.projTrafo (proj |> AVal.map Frustum.projTrafo)

[<EntryPoint; STAThread>]
let main argv =
    Aardvark.Init()

    let smoke = argv |> Array.exists ((=) "--smoke")

    use app = new VulkanApplication(DebugLevel.Normal)
    use win = app.CreateGameWindow(960, 640, samples = 1, vsync = true)

    let scene =
        Sg.ofList [
            createTriangleScene ()
            createLodScene () |> Sg.trafo (AVal.constant (Trafo3d.Translation(0.0, 0.0, -0.75)))
        ]
        |> withCamera win

    let task = app.Runtime.CompileRender(win.FramebufferSignature, scene)
    win.RenderTask <- task

    if smoke then
        Task.Run(fun () ->
            System.Threading.Thread.Sleep 1500
            win.Close()
        )
        |> ignore

    win.Run()
    0
