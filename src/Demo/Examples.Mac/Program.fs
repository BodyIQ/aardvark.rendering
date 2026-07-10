open System
open System.Threading
open System.Threading.Tasks
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.Glfw
open FSharp.Data.Adaptive

type CameraConfig =
    {
        eye : V3d
        target : V3d
        near : float
        far : float
    }

type Sample =
    {
        name : string
        source : string
        description : string
        camera : CameraConfig
        create : Window -> ISg
    }

let private camera eye target =
    { eye = eye; target = target; near = 0.05; far = 500.0 }

let private coloredEffect color =
    [
        DefaultSurfaces.trafo |> toEffect
        DefaultSurfaces.constantColor color |> toEffect
        DefaultSurfaces.simpleLighting |> toEffect
    ]

let private vertexColorEffect =
    [
        DefaultSurfaces.trafo |> toEffect
        DefaultSurfaces.vertexColor |> toEffect
    ]

let private createTriangleScene (_ : Window) =
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
    |> Sg.effect vertexColorEffect

let private createTutorialQuad (_ : Window) =
    let index = [| 0; 1; 2; 0; 2; 3 |]
    let positions = [| V3f(-1.0f, -1.0f, 0.0f); V3f(1.0f, -1.0f, 0.0f); V3f(1.0f, 1.0f, 0.0f); V3f(-1.0f, 1.0f, 0.0f) |]
    let colors = [| C4b(210uy, 62uy, 80uy); C4b(240uy, 160uy, 70uy); C4b(70uy, 165uy, 130uy); C4b(70uy, 120uy, 210uy) |]

    IndexedGeometry(
        IndexedGeometryMode.TriangleList,
        index,
        SymDict.ofList [
            DefaultSemantic.Positions, positions :> Array
            DefaultSemantic.Colors, colors :> Array
        ],
        SymDict.empty
    )
    |> Sg.ofIndexedGeometry
    |> Sg.effect vertexColorEffect

let private createLodScene (_ : Window) =
    let low =
        Sg.box' C4b.DarkCyan (Box3d.FromCenterAndSize(V3d.Zero, V3d.III))
        |> Sg.effect (coloredEffect C4f.DarkCyan)

    let high =
        Sg.unitSphere' 5 C4b.Orange
        |> Sg.effect (coloredEffect C4f.Orange)

    let marker =
        Sg.draw IndexedGeometryMode.LineList
        |> Sg.vertexAttribute DefaultSemantic.Positions (AVal.constant [| V3f(-2.0f, 0.0f, 0.0f); V3f(2.0f, 0.0f, 0.0f); V3f(0.0f, -2.0f, 0.0f); V3f(0.0f, 2.0f, 0.0f) |])
        |> Sg.vertexAttribute DefaultSemantic.Colors (AVal.constant [| C4b.White; C4b.White; C4b.White; C4b.White |])
        |> Sg.effect vertexColorEffect

    Sg.ofList [
        Sg.lod (fun scope -> Vec.length (scope.cameraPosition - scope.bb.Center) < 4.0) low high
        marker
    ]

let private createPrimitiveScene (_ : Window) =
    let primitives =
        [|
            IndexedGeometryPrimitives.Sphere.solidPhiThetaSphere (Sphere3d(V3d.Zero, 0.5)) 18 (C4b(160uy, 120uy, 190uy))
            IndexedGeometryPrimitives.Sphere.wireframePhiThetaSphere (Sphere3d(V3d.Zero, 0.5)) 12 (C4b(90uy, 120uy, 180uy))
            IndexedGeometryPrimitives.Sphere.solidSubdivisionSphere (Sphere3d(V3d.Zero, 0.5)) 2 (C4b(120uy, 0uy, 220uy))
            IndexedGeometryPrimitives.Quad.solidQuadrangle V3d.OOO V3d.IOO V3d.IOI V3d.OOI C4b.White V3d.OIO
            IndexedGeometryPrimitives.Triangle.solidTrianglesWithColor [ Triangle3d(V3d(0.1, 0.1, 0.0), V3d(0.9, 0.2, 0.1), V3d(0.4, 0.9, 0.0)) ] (C4b(220uy, 210uy, 60uy))
            IndexedGeometryPrimitives.Stuff.coordinateCross V3d.III
            IndexedGeometryPrimitives.Box.solidBox (Box3d(V3d.Zero, V3d.III)) (C4b(240uy, 150uy, 220uy))
            IndexedGeometryPrimitives.Box.wireBox (Box3d(V3d.Zero, V3d.III)) (C4b(120uy, 220uy, 240uy))
            IndexedGeometryPrimitives.Torus.solidTorus (Torus3d(V3d.Zero, V3d.OOI, 0.55, 0.15)) (C4b(200uy, 180uy, 255uy)) 18 10
            IndexedGeometryPrimitives.Cone.solidCone V3d.OOO V3d.OOI 1.0 0.35 18 (C4b(120uy, 130uy, 170uy))
            IndexedGeometryPrimitives.Cylinder.solidCylinder V3d.OOO V3d.OOI 1.0 0.28 0.28 18 (C4b(250uy, 100uy, 140uy))
            IndexedGeometryPrimitives.Tetrahedron.solidTetrahedron V3d.OOO 0.8 (C4b(26uy, 100uy, 240uy))
        |]

    let perRow = 4

    primitives
    |> Array.mapi (fun i geometry ->
        let x = float (i % perRow) * 1.7
        let y = float (i / perRow) * 1.7

        geometry
        |> Sg.ofIndexedGeometry
        |> Sg.translate x y 0.0
    )
    |> Array.toList
    |> Sg.ofList
    |> Sg.trafo (AVal.constant (Trafo3d.Translation(-2.6, -1.7, 0.0)))
    |> Sg.effect vertexColorEffect

let private createInstancingScene (_ : Window) =
    let geometry = Primitives.unitSphere 4

    let trafos =
        [|
            for x in -6 .. 6 do
                for y in -6 .. 6 do
                    let fx = float x
                    let fy = float y
                    let z = 0.2 * sin (0.8 * fx) * cos (0.8 * fy)
                    yield Trafo3d.Scale(0.08) * Trafo3d.Translation(0.28 * fx, 0.28 * fy, z)
        |]

    geometry
    |> Sg.instancedGeometry (AVal.constant trafos)
    |> Sg.effect [
        DefaultSurfaces.instanceTrafo |> toEffect
        DefaultSurfaces.trafo |> toEffect
        DefaultSurfaces.constantColor (C4f(0.9f, 0.2f, 0.25f, 1.0f)) |> toEffect
        DefaultSurfaces.simpleLighting |> toEffect
    ]

let private createTerrainScene (_ : Window) =
    let count = V2i(80, 80)
    let positions = Array.zeroCreate<V3f> ((count.X + 1) * (count.Y + 1))
    let colors = Array.zeroCreate<C4b> positions.Length
    let normals = Array.create positions.Length V3f.OOI
    let getId (x : int) (y : int) = x + y * (count.X + 1)

    for y in 0 .. count.Y do
        for x in 0 .. count.X do
            let u = float x / float count.X
            let v = float y / float count.Y
            let px = 12.0 * (u - 0.5)
            let py = 12.0 * (v - 0.5)
            let h = 0.55 * sin (px * 1.6) * cos (py * 1.1) + 0.25 * sin (px * 3.4 + py)
            let c = byte (90 + int (90.0 * ((h + 0.8) / 1.6 |> clamp 0.0 1.0)))

            positions.[getId x y] <- V3f(px, py, h)
            colors.[getId x y] <- C4b(50uy, c, 120uy, 255uy)

    let indices = Array.zeroCreate<int> (count.X * count.Y * 6)
    let mutable oi = 0
    for y in 1 .. count.Y do
        for x in 1 .. count.X do
            let i00 = getId (x - 1) (y - 1)
            let i10 = getId x (y - 1)
            let i01 = getId (x - 1) y
            let i11 = getId x y

            indices.[oi + 0] <- i00
            indices.[oi + 1] <- i10
            indices.[oi + 2] <- i01
            indices.[oi + 3] <- i10
            indices.[oi + 4] <- i11
            indices.[oi + 5] <- i01
            oi <- oi + 6

    IndexedGeometry(
        IndexedGeometryMode.TriangleList,
        indices,
        SymDict.ofList [
            DefaultSemantic.Positions, positions :> Array
            DefaultSemantic.Colors, colors :> Array
            DefaultSemantic.Normals, normals :> Array
        ],
        SymDict.empty
    )
    |> Sg.ofIndexedGeometry
    |> Sg.effect vertexColorEffect

let private createPointCloudScene (_ : Window) =
    let rand = Random 10
    let pointCount = 4096

    let positions =
        Array.init pointCount (fun _ ->
            let r = 1.7 + 0.5 * rand.NextDouble()
            let a = rand.NextDouble() * Constant.PiTimesTwo
            let z = -0.9 + 1.8 * rand.NextDouble()
            V3f(r * cos a, r * sin a, z)
        )

    let colors =
        Array.init pointCount (fun i ->
            let t = float i / float pointCount
            C4b(byte (80 + int (120.0 * t)), byte (170 - int (80.0 * t)), 220uy, 255uy)
        )

    Sg.draw IndexedGeometryMode.PointList
    |> Sg.vertexAttribute DefaultSemantic.Positions (AVal.constant positions)
    |> Sg.vertexAttribute DefaultSemantic.Colors (AVal.constant colors)
    |> Sg.effect vertexColorEffect

let private createOverviewScene (win : Window) =
    Sg.ofList [
        createTriangleScene win
        |> Sg.trafo (AVal.constant (Trafo3d.Scale(1.2) * Trafo3d.Translation(-3.0, -2.2, 0.0)))

        createTutorialQuad win
        |> Sg.trafo (AVal.constant (Trafo3d.Scale(0.75) * Trafo3d.Translation(-0.7, -2.3, 0.0)))

        createLodScene win
        |> Sg.trafo (AVal.constant (Trafo3d.Scale(0.65) * Trafo3d.Translation(2.0, -2.2, 0.0)))

        createInstancingScene win
        |> Sg.trafo (AVal.constant (Trafo3d.Scale(0.9) * Trafo3d.Translation(-2.5, 1.1, 0.0)))

        createTerrainScene win
        |> Sg.trafo (AVal.constant (Trafo3d.Scale(0.28) * Trafo3d.Translation(0.8, 1.0, -0.4)))

        createPointCloudScene win
        |> Sg.trafo (AVal.constant (Trafo3d.Scale(0.8) * Trafo3d.Translation(2.7, 1.1, 0.0)))
    ]

let private samples =
    [
        {
            name = "overview"
            source = "Examples.Mac"
            description = "Small gallery combining the portable Mac samples"
            camera = camera (V3d(0.0, -8.5, 5.5)) V3d.Zero
            create = createOverviewScene
        }
        {
            name = "triangle"
            source = "Program.fs / HelloWorld-style"
            description = "Minimal vertex-color triangle"
            camera = camera (V3d(0.0, -3.0, 2.0)) V3d.Zero
            create = createTriangleScene
        }
        {
            name = "tutorial-quad"
            source = "Tutorial.fs"
            description = "IndexedGeometry quad with per-vertex color"
            camera = camera (V3d(2.3, -3.0, 2.1)) V3d.Zero
            create = createTutorialQuad
        }
        {
            name = "live-lod"
            source = "LiveDemo.fs"
            description = "Uses the linked Sg.lod semantic"
            camera = camera (V3d(2.8, -3.0, 2.3)) V3d.Zero
            create = createLodScene
        }
        {
            name = "primitives"
            source = "IGPrimitives.fs / IndexedGeometryPrimitives.fs"
            description = "A grid of built-in indexed geometry primitives"
            camera = camera (V3d(3.5, -8.5, 6.0)) (V3d(2.4, 1.8, 0.2))
            create = createPrimitiveScene
        }
        {
            name = "instancing"
            source = "Instancing.fs"
            description = "Instanced sphere grid using DefaultSurfaces.instanceTrafo"
            camera = camera (V3d(0.0, -5.2, 3.4)) V3d.Zero
            create = createInstancingScene
        }
        {
            name = "terrain"
            source = "Terrain.fs"
            description = "CPU-generated terrain grid; avoids tessellation for KosmicKrisp"
            camera = camera (V3d(3.5, -9.0, 5.0)) V3d.Zero
            create = createTerrainScene
        }
        {
            name = "points"
            source = "LoD.fs / PostProcessing.fs"
            description = "Point-list rendering without geometry-shader point sprites"
            camera = camera (V3d(0.0, -5.0, 2.8)) V3d.Zero
            create = createPointCloudScene
        }
    ]

let private unsupported =
    [
        "AssimpInterop.fs / Eigi.fs / Sponza.fs: require external model assets that are not present in this repo checkout"
        "ComputeShader.fs: separate compute coverage; not a scene sample"
        "CullingTest.fs / GeometryComposition.fs / HelloWorld.fs / Program.fs / Wobble.fs: WinForms or Windows-hosted entrypoints"
        "HarwareFeatures.fs: capability dump rather than a visual scene"
        "Jpeg.fs: image-codec experiment, not a Vulkan scene"
        "LevelOfDetail.fs: older Interactive/WinForms LOD harness; LiveDemo.fs LOD semantic is covered"
        "Render2TexturePrimitive*.fs / Render2TextureComposable.fs: render-to-texture path still needs a KosmicKrisp clear-path review"
        "Shadows.fs: custom shadow pipeline not yet ported to the Slim harness"
        "Stereo.fs: stereo-specific host setup"
        "Tessellation.fs / TessellatedSphere.fs: KosmicKrisp on this Mac reports no tessellation-shader feature"
        "ShaderSwitches.fs: shader compiler/signature diagnostic, not a windowed sample"
        "Trie.fs: data-structure test helper, not a rendering sample"
    ]

let private printSamples () =
    printfn "Runnable samples:"
    for sample in samples do
        printfn "  %-14s %s (%s)" sample.name sample.description sample.source

    printfn ""
    printfn "Known non-ported or intentionally skipped demos:"
    for entry in unsupported do
        printfn "  - %s" entry

let private tryGetSample name =
    samples
    |> List.tryFind (fun sample -> String.Equals(sample.name, name, StringComparison.OrdinalIgnoreCase))

let private parseSampleName (argv : string[]) =
    let rec loop i =
        if i >= argv.Length then "overview"
        else
            match argv.[i] with
            | "--sample" | "-s" when i + 1 < argv.Length -> argv.[i + 1]
            | value when not (value.StartsWith("-", StringComparison.Ordinal)) -> value
            | _ -> loop (i + 1)

    loop 0

let private withCamera (win : Window) (config : CameraConfig) (scene : ISg) =
    let initialView = CameraView.lookAt config.eye config.target V3d.OOI
    let view = DefaultCameraController.control win.Mouse win.Keyboard win.Time initialView

    let proj =
        win.Sizes
        |> AVal.map (fun s ->
            let aspect =
                if s.Y = 0 then 1.0
                else float s.X / float s.Y

            Frustum.perspective 60.0 config.near config.far aspect
        )

    scene
    |> Sg.viewTrafo (view |> AVal.map CameraView.viewTrafo)
    |> Sg.projTrafo (proj |> AVal.map Frustum.projTrafo)

[<EntryPoint; STAThread>]
let main argv =
    let smoke = argv |> Array.exists ((=) "--smoke")
    let debug = argv |> Array.exists ((=) "--debug")
    let listOnly = argv |> Array.exists (fun a -> a = "--list" || a = "-l")

    if listOnly then
        printSamples ()
        0
    else
        let sampleName = parseSampleName argv

        match tryGetSample sampleName with
        | None ->
            eprintfn "Unknown sample '%s'." sampleName
            eprintfn ""
            printSamples ()
            2

        | Some sample ->
            Aardvark.Init()
            printfn "Running sample '%s' from %s" sample.name sample.source

            let debugLevel =
                if debug then DebugLevel.Normal
                else DebugLevel.None

            use app = new VulkanApplication(debugLevel)
            use win = app.CreateGameWindow(960, 640, samples = 1, vsync = true)

            let scene = sample.create win |> withCamera win sample.camera
            let task = app.Runtime.CompileRender(win.FramebufferSignature, scene)
            win.RenderTask <- task

            if smoke then
                Task.Run(fun () ->
                    Thread.Sleep 1500
                    win.Close()
                )
                |> ignore

            win.Run()
            0
