module Divanov.Prelude


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

module Async =
    type Cts = System.Threading.CancellationTokenSource
    type Ct = System.Threading.CancellationToken
    type Lock () =
        let mutable trigger = 0
        member __.Acquire (): bool =
            System.Threading.Interlocked.CompareExchange(&trigger, 1, 0) = 0
        member __.RunOnce (thunk: unit -> unit): bool =
            if __.Acquire ()
            then thunk (); true
            else false

    let race (a1: Async<'T>) (a2: Async<'T>): Async<'T> =
        Async.FromContinuations( fun (cont, excont, cancont) ->
            let mutable result: 'T option = None
            let source1 = new Cts ()
            let source2 = new Cts ()
            let token1 = source1.Token
            let token2 = source2.Token
            let lock = Lock ()
            let update (res: 'T) =
                lock.RunOnce(fun () -> result <- Some res)
            let canceling (source: Cts, a: Async<'T>) =
                async {
                    let! res = a
                    if update res
                    then
                        source.Cancel()
                        return cont (Option.get result)
                    else return ()
                }
            [ ((source2, a1), token1); ((source1, a2), token2) ]
            |> List.iter (fun (ctuple, t) -> Async.Start (canceling ctuple, t))
        )

    let raceMany (l: List<Async<'T>>): Async<'T> =
        List.reduce race l

    let map (f: 'T -> 'U) (a: Async<'T>): Async<'U> =
        async {
            let! b = a
            return (f b)
        }
    
    let bind (f: 'T -> Async<'U>) (a: Async<'T>): Async<'U> =
        async {
            let! b = a
            return! (f b)
        }

module Dictionary =
    open System.Collections.Generic
    let update (pair: 't * 'u when 't: comparison) (d: Dictionary<'t, 'u>): Dictionary<'t, 'u> =
        let k, _ = pair
        d.Remove k |> ignore
        d.Add pair
        d

    let addCount (element: 't when 't: comparison) (d: Dictionary<'t, int>): Dictionary<'t, int> =
        if d.ContainsKey element
        then
            let count = d.Item element
            update (element, count + 1) d
        else
            d.Add (element, 1)
            d
    let removeCount (element: 't when 't: comparison) (d: Dictionary<'t, int>): Dictionary<'t, int> =
        if d.ContainsKey element
        then
            let count = d.Item element
            update (element, count - 1) d
        else update (element, 1) d

module Map =
    let update (key: 'a) (f: 'b -> 'b) (map: Map<'a, 'b>): Map<'a, 'b> =
        match map.TryFind key with
        | None -> map
        | Some v -> map.Add (key, f v)

    let addRegardless (key: 'a) (upd: 'b option -> 'b) (map: Map<'a, 'b>): Map<'a, 'b> =
        map.Add (key, map.TryFind key |> upd)

module List =
    let mapValues (f: 'a -> 'b) (l: ('k * 'a) list): ('k * 'b) list =
        List.map (fun elt -> (fst elt, snd elt |> f)) l
    
    let tryTail (l: 'a list): 'a list option =
        if List.isEmpty l then None else Some (List.tail l)

module Array =
    let tryTail (l: 'a []): 'a [] option =
        if Array.isEmpty l then None else Some (Array.tail l)
    
    let argMinBy (by: 't -> 'u when 'u: comparison) (arr: 't []): int =
        Array.mapi (fun i e -> Lazy.Create( fun () -> (i, by e))) arr
        |> Array.fold
            (fun (i, m) lz ->
                    let (ie, elt) = lz.Force()
                    if elt < m then (ie, elt) else (i, m)) (0, by arr.[0])
        |> fst

    let argMaxBy (by: 't -> 'u when 'u: comparison) (arr: 't []): int =
        Array.mapi (fun i e -> Lazy.Create( fun () -> (i, by e))) arr
        |> Array.fold
            (fun (i, m) lz ->
                    let (ie, elt) = lz.Force()
                    if elt > m then (ie, elt) else (i, m)) (0, by arr.[0])
        |> fst

    let argMin (arr: 't [] when 't: comparison) = argMinBy id arr

    let argMax (arr: 't []  when 't: comparison) = argMaxBy id arr

    let argMinNBy (by: 't -> 'u when 'u: comparison) (n: int) (arr: 't []) =
        let indexed: Lazy<int * 'u> [] = Array.mapi (fun i e -> Lazy.Create (fun () -> (i, by e))) arr
        let buffer = Array.take n indexed |> Array.map (fun x -> x.Force())
        let rest = Array.skip n indexed
        let updateBuffer (l: Lazy<int * 'u>): unit =
            let i, e = l.Force()
            if Array.map (fun (k, v) -> e < v) buffer |> Array.fold (||) false
            then Array.sortInPlaceBy snd buffer; buffer.[0] <- (i, e)
        Array.iter updateBuffer rest
        buffer |> Array.map fst

    let argMaxNBy (by: 't -> 'u when 'u: comparison) (n: int) (arr: 't []) =
        let indexed: Lazy<int * 'u> [] = Array.mapi (fun i e -> Lazy.Create (fun () -> (i, by e))) arr
        let buffer = Array.take n indexed |> Array.map (fun x -> x.Force())
        let rest = Array.skip n indexed
        let updateBuffer (l: Lazy<int * 'u>): unit =
            let i, e = l.Force()
            if Array.map (fun (k, v) -> e > v) buffer |> Array.fold (||) false
            then Array.sortInPlaceBy snd buffer; buffer.[Array.length buffer - 1] <- (i, e)
        Array.iter updateBuffer rest
        buffer |> Array.map fst |> Array.rev

    let argMinN (n: int) (arr: 't [] when 't: comparison) = argMinNBy id n arr

    let argMaxN (n: int) (arr: 't [] when 't: comparison) = argMaxNBy id n arr

    let items (ns: int []) (arr: 't []): 't [] =
        Array.Parallel.map (fun i -> arr.[i]) ns

module Seq =
    let tryTail (s: 'a seq): 'a seq option =
        if Seq.isEmpty s then None else Some (Seq.tail s)