module Divanov.Prelude


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
    
    let argMin (arr: 't []): int =
        Array.mapi (fun i e -> (i, e))  arr
        |> Array.fold
            (fun (i, m) (ie, elt) ->
                    if elt < m then (ie, elt) else (i, m)) (0, arr.[0])
        |> fst

    let argMinN (n: int) (arr: 't []) =
        let indexed: (int * 't) [] = Array.mapi (fun i e -> (i, e)) arr
        let buffer = Array.take n indexed
        let rest = Array.skip n indexed
        let updateBuffer (i: int, e: 't): unit =
            if Array.map (fun (k, v) -> e < v) buffer |> Array.fold (||) false
            then  Array.sortInPlaceBy snd buffer; buffer.[0] <- (i, e)
        Array.iter updateBuffer rest
        buffer |> Array.map fst

    let items (ns: int []) (arr: 't []): 't [] =
        Array.Parallel.map (fun i -> arr.[i]) ns

module Seq =
    let tryTail (s: 'a seq): 'a seq option =
        if Seq.isEmpty s then None else Some (Seq.tail s)