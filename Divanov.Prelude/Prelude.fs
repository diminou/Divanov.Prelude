namespace Divanov.Prelude


module Misc =
    type SyncRandom(r: System.Random) =
        let r = r
        member __.Next() =
            System.Threading.Monitor.Enter r
            try r.Next()
            finally System.Threading.Monitor.Exit r
        member __.Next(i: int) =
            System.Threading.Monitor.Enter r
            try r.Next(i)
            finally System.Threading.Monitor.Exit r
        member __.NextDouble() =
            System.Threading.Monitor.Enter r
            try r.NextDouble()
            finally System.Threading.Monitor.Exit r
        member __.NextBytes(b: byte[]) =
            System.Threading.Monitor.Enter r
            try
                r.NextBytes(b)
            finally System.Threading.Monitor.Exit r
    
    let lockArg (f: 'a -> 'b when 'a: not struct): 'a -> 'b = fun (a: 'a) ->
        let f2 () = f a
        lock a f2
        