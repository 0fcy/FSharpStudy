open System

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
    static member deconstruct v = (v.X, v.Y, v.Z, v.W)
    static member asArray v = [| v.X; v.Y; v.Z; v.W |]

    static member dot (a: Vector4) (b: Vector4) = a.X * b.X + a.Y * b.Y + a.Z * b.Z + a.W * b.W
    static member normalize (v: Vector4) =
        let l = sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)
        if l < 1e-12
            then v
            else Vector4.unit (v.X / l) (v.Y / l) (v.Z / l) v.W

    override this.ToString() =
        let values = Vector4.asArray this
        let pad =
            values
            |> Array.map (sprintf "%.2f")
            |> Array.map String.length
            |> Array.max

        values 
        |> Array.map (fun v -> sprintf "¦ %-*.2f ¦" pad v)
        |> String.concat "\n"

type Matrix4x4 = Vector4 * Vector4 * Vector4 * Vector4

module Matrix =
    let identity: Matrix4x4 = (Vector4.unitX, Vector4.unitY, Vector4.unitZ, Vector4.unitW)
    
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

    let multiply (a: Matrix4x4) (b: Matrix4x4) : Matrix4x4 =
        let bc0, bc1, bc2, bc3 = b
        let (ar0, ar1, ar2, ar3) = transpose a
        (
            { X = Vector4.dot ar0 bc0; Y = Vector4.dot ar1 bc0; Z = Vector4.dot ar2 bc0; W = Vector4.dot ar3 bc0 },
            { X = Vector4.dot ar0 bc1; Y = Vector4.dot ar1 bc1; Z = Vector4.dot ar2 bc1; W = Vector4.dot ar3 bc1 },
            { X = Vector4.dot ar0 bc2; Y = Vector4.dot ar1 bc2; Z = Vector4.dot ar2 bc2; W = Vector4.dot ar3 bc2 },
            { X = Vector4.dot ar0 bc3; Y = Vector4.dot ar1 bc3; Z = Vector4.dot ar2 bc3; W = Vector4.dot ar3 bc3 }
        )

    let print (matrix: Matrix4x4) : string =
        let (r0, r1, r2, r3) = matrix
        sprintf "%s\n%s\n%s\n%s" (r0.ToString()) (r1.ToString()) (r2.ToString()) (r3.ToString())

//let r0 = { X = 1; Y = 2; Z = 3; W = 4 }
//let r1 = { X = 3; Y = 5; Z = 7; W = 9 }
//let r2 = { X = 5; Y = 8; Z = 11; W = 14 }
//let r3 = { X = 7; Y = 11; Z = 15; W = 19 }

//let a = (r0, r1, r2, r3)

//let rotate45 = Matrix.rotateX (System.Math.PI / 4.)
//let rotated =
//    a
//    |> Matrix.multiply rotate45

//let a = Matrix.identity
//let b = Matrix.identity
//let c = Matrix.multiply a b
//printfn "%s" (Matrix.print c)

let v = Vector4.unit 1.0012 2001 3.2412553 4.2
let u = Vector4.unit 1 2 3 4
let vAsString = v.ToString()
printfn "%s" vAsString
printfn ""
printfn "%s" (u.ToString().PadRight(3))