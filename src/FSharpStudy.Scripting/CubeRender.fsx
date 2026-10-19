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

let r10 = { X = 1; Y = 2; Z = 3; W = 4 }
let r11 = { X = 3; Y = 5; Z = 7; W = 9 }
let r12 = { X = 5; Y = 8; Z = 11; W = 14 }
let r13 = { X = 7; Y = 11; Z = 15; W = 19 }

let r20 = { X = 1; Y = 2; Z = 3; W = 4 }
let r21 = { X = 3; Y = 5; Z = 7; W = 9 }
let r22 = { X = 5; Y = 8; Z = 11; W = 14 }
let r23 = { X = 7; Y = 11; Z = 15; W = 19 }

let a = (r10, r11, r12, r13)
let b = (r20, r21, r22, r23)
let multiplied = Matrix.multiply a b

printfn "%s" (Matrix.print multiplied)