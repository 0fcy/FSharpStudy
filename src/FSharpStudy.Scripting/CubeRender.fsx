open System
open System.Text

[<Struct>]
type Vector3 = { X: float; Y: float; Z: float }

[<Struct>]
type Vector4 =
    { X: float; Y: float; Z: float; W: float }
    static member unitX = { X = 1; Y = 0; Z = 0; W = 0 }
    static member unitY = { X = 0; Y = 1; Z = 0; W = 0 }
    static member unitZ = { X = 0; Y = 0; Z = 1; W = 0 }
    static member unitW = { X = 0; Y = 0; Z = 0; W = 1 }

    static member unit x y z w = { X = x; Y = y; Z = z; W = w }
    static member scalar a = { X = a; Y = a; Z = a; W = a }
    static member point x y z = { X = x; Y = y; Z = z; W = 1 }
    static member dir x y z = { X = x; Y = y; Z = z; W = 0 }

    static member deconstruct v = (v.X, v.Y, v.Z, v.W)
    static member arr v = [| v.X; v.Y; v.Z; v.W |]

    static member maxBy f v = Vector4.arr v |> Array.maxBy f


    static member dot (a: Vector4) (b: Vector4) = a.X * b.X + a.Y * b.Y + a.Z * b.Z + a.W * b.W
    static member normalize (v: Vector4) =
        let l = sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)
        if l < 1e-12
            then v
            else Vector4.unit (v.X / l) (v.Y / l) (v.Z / l) v.W

    override this.ToString() =
        let values = Vector4.arr this
        let pad =
            values
            |> Array.map (sprintf "%g")
            |> Array.map String.length
            |> Array.max

        values 
        |> Array.map (fun v -> sprintf "¦ %-*g ¦" pad v)
        |> String.concat String.Empty



type Matrix4x4 = Vector4 * Vector4 * Vector4 * Vector4

module Matrix =
    let identity: Matrix4x4 = (Vector4.unitX, Vector4.unitY, Vector4.unitZ, Vector4.unitW)

    let translate x y z = (Vector4.unitX, Vector4.unitY, Vector4.unitZ, Vector4.unit x y z 1)
    let rotateX (angle: float) : Matrix4x4 =
        let struct (s, c) = Math.SinCos angle
        (
            { X = 1; Y = 0; Z = 0; W = 0 },
            { X = 0; Y = c; Z = s; W = 0 },
            { X = 0; Y = -s; Z = c; W = 0 },
            { X = 0; Y = 0; Z = 0; W = 1 }
        )
    let rotateY (angle: float) : Matrix4x4 =
        let struct (s, c) = Math.SinCos angle
        (
            { X = c; Y = 0; Z = -s; W = 0 },
            { X = 0; Y = 1; Z = 0; W = 0 },
            { X = s; Y = 0; Z = c; W = 0 },
            { X = 0; Y = 0; Z = 0; W = 1 }
        )
    let rotateZ (angle: float) : Matrix4x4 =
        let struct (s, c) = Math.SinCos angle
        (
            { X = c; Y = s; Z = 0; W = 0 },
            { X = -s; Y = c; Z = 0; W = 0 },
            { X = 0; Y = 0; Z = 1; W = 0 },
            { X = 0; Y = 0; Z = 0; W = 1 }
        )

    let transpose (matrix: Matrix4x4): Matrix4x4 =
        let (r0, r1, r2, r3) = matrix
        (
            Vector4.unit r0.X r0.Y r0.Z r0.W,
            Vector4.unit r1.X r1.Y r1.Z r1.W,
            Vector4.unit r2.X r2.Y r2.Z r2.W,
            Vector4.unit r3.X r3.Y r3.Z r3.W
        )

    let transform (m: Matrix4x4) (v: Vector4) =
        let c0, c1, c2, c3 = m
        Vector4.unit
            (c0.X * v.X + c1.X * v.Y + c2.X * v.Z + c3.X * v.W)
            (c0.Y * v.X + c1.Y * v.Y + c2.Y * v.Z + c3.Y * v.W)
            (c0.Z * v.X + c1.Z * v.Y + c2.Z * v.Z + c3.Z * v.W)
            (c0.W * v.X + c1.W * v.Y + c2.W * v.Z + c3.W * v.W)

    let multiply (a: Matrix4x4) (b: Matrix4x4) : Matrix4x4 =
        let b0, b1, b2, b3 = b
        (transform a b0, transform a b1, transform a b2, transform a b3)



    let projection fovY aspect near far =
        let f = 1. / (tan (fovY * 0.5))
        let nf = 1. / (near - far)
        (
            Vector4.unit (f / aspect) 0 0 0,
            Vector4.unit 0 f 0 0,
            Vector4.unit 0 0 ((far + near) * nf) -1,
            Vector4.unit 0 0 ((2. * far * near) * nf) 0
        )

    let print (m: Matrix4x4) : string =
        let (r0, r1, r2, r3) = m
        sprintf "%s\n%s\n%s\n%s\n" (r0.ToString()) (r1.ToString()) (r2.ToString()) (r3.ToString())


[<Struct>]
type Point =
    { SX: float; SY: float; SZ: float; InvW: float; Visible: bool }
    static member unit sx sy sz invW visible = { SX = sx; SY = sy; SZ = sz; InvW = invW; Visible = visible }
    static member fromVector (cols: int) (rows: int) (v: Vector4) =
        if v.W <= 1e-6 then
            Unchecked.defaultof<Point>
        else
            let invW = 1. / v.W
            let nx = v.X * invW
            let ny = v.Y * invW
            let nz = v.Z * invW
            {
                SX = (nx + 1.) * 0.5 * float (cols - 1);
                SY = (1. - ny) * 0.5 * float (rows - 1);
                SZ = nz
                InvW = invW;
                Visible = true
            }

    static member coordinate p = struct (int (floor (p.SX + 0.5)), int (floor (p.SY + 0.5)))

type RenderState = 
    { Buffer: char[]; ZBuffer: float[]; Cols: int; Rows: int }

module Renderer =
    //let linePositions (a: Point) (b: Point) : (int * int * float) list =
    //    let struct (ax, ay) = Point.coordinate a
    //    let struct (bx, by) = Point.coordinate b
        
    //    let dx = abs (bx - ax)
    //    let dy = -abs (by - ay)
    //    let sx = if a.SX < b.SX then 1 else -1
    //    let sy = if a.SY < b.SY then 1 else -1

    //    let rec iterate x y err acc =
    //        let newAcc = (x, y) :: acc
    //        if x = bx && y = by then
    //            List.rev newAcc
    //        else
    //            let e2 = 2 * err
    //            let (newErr, finalX) = if e2 >= dy then (err + dy, x + sx) else (err, x)
    //            let (finalErr, finalY) = if e2 <= dx then (newErr + dx, y + sy) else (newErr, y)
    //            iterate finalX finalY finalErr newAcc

    //    iterate ax ay (dx + dy) []

    let linePositions (a: Point) (b: Point) : (int * int * float) list =
        let struct (ax, ay) = Point.coordinate a
        let struct (bx, by) = Point.coordinate b
        let totalSteps = max (abs (bx - ax)) (abs (by - ay))
    
        [0 .. totalSteps]
        |> List.map (fun i ->
            let t = float i / float totalSteps
            let x = ax + int (round (t * float (bx - ax)))
            let y = ay + int (round (t * float (by - ay)))
            let z = a.SZ + t * (b.SZ - a.SZ)
            x, y, z)

    let renderLine (state: RenderState) (line: Point * Point) : RenderState =
        let (a, b) = line
        if not (a.Visible && b.Visible) then state
        else
            linePositions a b
            |> List.fold (fun st (x, y, z) ->
                if x < 0 || x >= st.Cols || y < 0 || y >= st.Rows then st
                else
                    let idx = y * st.Cols + x
                    if z < st.ZBuffer.[idx] then
                        Array.set st.Buffer idx '#'
                        Array.set st.ZBuffer idx z
                        { st with 
                            Buffer = Array.copy st.Buffer
                            ZBuffer = Array.copy st.ZBuffer
                        }
                    else
                        st
                ) state

    let renderAll (lines: (Point * Point) array) (cols: int) (rows: int) : char[] =
        let initialState = 
            { Buffer = Array.create (cols * rows) ' '
              ZBuffer = Array.init (cols * rows) (fun _ -> System.Double.MaxValue)
              Cols = cols; Rows = rows }
    
        lines
        |> Array.fold renderLine initialState
        |> fun s -> s.Buffer

let safeDim (get: unit -> int) (fallback: int) =
    try
        let v = get ()
        if v > 0 then v else fallback
    with _ -> fallback

let cols = Console.WindowWidth
let rows = Console.WindowHeight
let gridRows = rows - 1        // leave the last line free so we never scroll

// ---- Geometry: a hollow triangle, unit circumradius, in the XY plane ----
let tri = [| Vector4.point 0.0 2.0 0.0; Vector4.point -1.8660 -1.5 0.0; Vector4.point 1.8660 -1.5 0.0 |]

let edges = [| (0, 1); (1, 2); (2, 0) |]

// ---- Camera ----------------------------------------------------------
// Sitting at the origin looking down -Z. The "view transform" is just
// shoving the world 4 units away from us.
let view = Matrix.translate 0.0 0.0 -2.0

// Terminal cells are ~twice as tall as wide, so the physical aspect
// ratio is cols / (2 * gridRows). This is the ONLY aspect correction.
let cellAspect = 2.0
let aspect = float cols / (cellAspect * float gridRows)
let proj = Matrix.projection (Math.PI / 3.0) aspect 0.1 100.0

// ---- Buffers allocated ONCE ------------------------------------------
let buf = Array.create (cols * gridRows) ' '
let sb  = StringBuilder(cols * gridRows + gridRows * 8)

let sw = Diagnostics.Stopwatch.StartNew()
let mutable angle = 0.0
let mutable distance = -3.
let mutable prev  = 0.0
let mutable running = true

Console.CancelKeyPress.Add(fun a ->
    a.Cancel <- true
    running <- false)

Console.Write "\x1b[?25l"          // hide cursor

while running do
    let now = sw.Elapsed.TotalSeconds
    let dt  = now - prev
    prev <- now
    angle <- angle + 5. * dt      // radians per second, framerate independent

    if Console.KeyAvailable then
        let key = Console.ReadKey(true)
        if key.Key = ConsoleKey.UpArrow then
            distance <- distance + dt * 5.
        else if key.Key = ConsoleKey.DownArrow then
            distance <- distance - dt * 5.

    Array.fill buf 0 buf.Length ' '

    // MODEL: spin about Y first, then tilt about X.
    // (rightmost applies first)
    let model = Matrix.multiply (Matrix.rotateX 0.35) (Matrix.rotateY angle)

    printf "distance %g" distance
    let view = Matrix.translate 0.0 0.0 distance
    
    // model -> view -> projection, composed once per frame
    let mvp = Matrix.multiply proj (Matrix.multiply view model)

    // Every vertex through the whole pipeline.
    let pts =
        tri
        |> Array.map (Matrix.transform mvp)
        |> Array.map (Point.fromVector cols gridRows)

    let lines = (pts, pts) ||> Array.allPairs

    //let pixels =
    //    edges
    //    |> Array.collect (fun (i, j) ->
    //        let a, b = pts.[i], pts.[j]
    //        if a.Visible && b.Visible then
    //            Renderer.linePositions a b |> List.toArray
    //        else
    //            [||])

    //for x, y in pixels do
    //    if x >= 0 && x < cols && y >= 0 && y < gridRows then
    //        buf.[y * cols + x] <- '#'

    //sb.Clear() |> ignore
    //sb.Append("\x1b[H") |> ignore          // cursor home, no clear
    //for y in 0 .. gridRows - 1 do
    //    if y > 0 then sb.Append('\n') |> ignore
    //    sb.Append(buf, y * cols, cols) |> ignore
    //Console.Write(sb.ToString())


    let pixels = Renderer.renderAll lines cols gridRows
    
    sb.Clear() |> ignore
    sb.Append("\x1b[H") |> ignore          // cursor home, no clear
    for y in 0 .. gridRows - 1 do
        if y > 0 then sb.Append('\n') |> ignore
        sb.Append(pixels, y * cols, cols) |> ignore
    Console.Write(sb.ToString())

    Threading.Thread.Sleep 16              // ~60 fps cap

Console.Write "\x1b[?25h\x1b[0m"           // show cursor, reset attrs