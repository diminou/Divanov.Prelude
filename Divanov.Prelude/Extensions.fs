namespace Divanov.Prelude

[<AutoOpen>]
module Extensions =
    
    let (^^) (x: bool) (y: bool): bool = (x || y) && (not (x && y))

    let inline flip f x y = f y x

    type System.Boolean with
        member this.ToInt(): int = if this then 1 else 0
        member this.ToDouble(): double = if this then 1.0 else 0.0
        member this.ToSingle(): single = if this then 1.0f else 0.0f
        member this.ToByte(): byte = if this then 1uy else 0uy
