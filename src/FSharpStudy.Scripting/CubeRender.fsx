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
    member this.dot(other: Vector4) = this.X * other.X + this.Y * other.Y + this.Z * other.Z + this.W * other.W

    override this.ToString (): string = sprintf "%g %g %g %g" this.X this.Y this.Z this.W
        

type Matrix4x4 = Vector4 * Vector4 * Vector4 * Vector4

module Matrix =
    let identity: Matrix4x4 = (Vector4.unitX, Vector4.unitY, Vector4.unitZ, Vector4.unitW)
    
    let rotateX (angle: float) : Matrix4x4 =
        let struct (s, c) = Math.SinCos angle
        (
            { X = 1; Y = 0; Z = 0; W = 0 },
            { X = 0; Y = c; Z = -s; W = 0 },
            { X = 0; Y = s; Z = c; W = 0 },
            { X = 0; Y = 0; Z = 0; W = 1 }
        )

    let rotateY (angle: float) : Matrix4x4 =
        let struct (s, c) = Math.SinCos angle
        (
            { X = c; Y = 0; Z = s; W = 0 },
            { X = 0; Y = 1; Z = 0; W = 0 },
            { X = -s; Y = 0; Z = c; W = 0 },
            { X = 0; Y = 0; Z = 0; W = 1 }
        )

    let rotateZ (angle: float) : Matrix4x4 =
        let struct (s, c) = Math.SinCos angle
        (
            { X = c; Y = -s; Z = 0; W = 0 },
            { X = s; Y = c; Z = 0; W = 0 },
            { X = 0; Y = 0; Z = 1; W = 0 },
            { X = 0; Y = 0; Z = 0; W = 1 }
        )

    let transpose (matrix: Matrix4x4): Matrix4x4 =
        let (r0, r1, r2, r3) = matrix
        (
            Vector4.unit r0.X r1.X r2.X r3.X,
            Vector4.unit r0.Y r1.Y r2.Y r3.Y,
            Vector4.unit r0.Z r1.Z r2.Z r3.Z,
            Vector4.unit r0.W r1.W r2.W r3.W
        )

    let multiply (a: Matrix4x4) (b: Matrix4x4) : Matrix4x4 =
        let bc0, bc1, bc2, bc3 = transpose b
        let (ar0, ar1, ar2, ar3) = a
        (
            { X = ar0.dot bc0; Y = ar0.dot bc1; Z = ar0.dot bc2; W = ar0.dot bc3 },
            { X = ar1.dot bc0; Y = ar1.dot bc1; Z = ar1.dot bc2; W = ar1.dot bc3 },
            { X = ar2.dot bc0; Y = ar2.dot bc1; Z = ar2.dot bc2; W = ar2.dot bc3 },
            { X = ar3.dot bc0; Y = ar3.dot bc1; Z = ar3.dot bc2; W = ar3.dot bc3 }
        )

    let print (matrix: Matrix4x4) : string =
        let (r0, r1, r2, r3) = matrix
        sprintf "%s\n%s\n%s\n%s" (r0.ToString()) (r1.ToString()) (r2.ToString()) (r3.ToString())

let r0 = { X = 1; Y = 2; Z = 3; W = 4 }
let r1 = { X = 3; Y = 5; Z = 7; W = 9 }
let r2 = { X = 5; Y = 8; Z = 11; W = 14 }
let r3 = { X = 7; Y = 11; Z = 15; W = 19 }

let a = (r0, r1, r2, r3)

let rotate45 = Matrix.rotateX (System.Math.PI / 4.)
let rotated =
    a
    |> Matrix.multiply rotate45

printfn "%s" (Matrix.print rotated)