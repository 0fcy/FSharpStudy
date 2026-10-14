[<Struct>]
type Vector3 = { X: float; Y: float; Z: float }

[<Struct>]
type Vector4 = { X: float; Y: float; Z: float; W: float }
    with override this.ToString (): string =
            sprintf "%g %g %g %g" this.X this.Y this.Z this.W

type Matrix4x4 = Vector4 * Vector4 * Vector4 * Vector4

module Matrix =
    let identity: Matrix4x4 =
        ({ X = 1; Y = 0; Z = 0; W = 0 },
        { X = 0; Y = 1; Z = 0; W = 0 }, 
        { X = 0; Y = 0; Z = 1; W = 0 }, 
        { X = 0; Y = 0; Z = 0; W = 1 })
    
    let transpose (matrix: Matrix4x4): Matrix4x4 =
        let (r0, r1, r2, r3) = matrix
        ({ X = r0.X; Y = r1.X; Z = r2.X; W = r3.X },
        { X = r0.Y; Y = r1.Y; Z = r2.Y; W = r3.Y }, 
        { X = r0.Z; Y = r1.Z; Z = r2.Z; W = r3.Z }, 
        { X = r0.W; Y = r1.W; Z = r2.W; W = r3.W })


    let print (matrix: Matrix4x4) : string =
        let (r0, r1, r2, r3) = matrix
        sprintf "%s\n%s\n%s\n%s" (r0.ToString()) (r1.ToString()) (r2.ToString()) (r3.ToString())

let r0 = { X = 1; Y = 2; Z = 3; W = 4 }
let r1 = { X = 3; Y = 5; Z = 7; W = 9 }
let r2 = { X = 5; Y = 8; Z = 11; W = 14 }
let r3 = { X = 7; Y = 11; Z = 15; W = 19 }
let matrix = (r0, r1, r2, r3)
let translated = matrix |> Matrix.transpose

printfn "%s" (Matrix.print matrix)
printfn "Transposed:\n%s" (Matrix.print translated)