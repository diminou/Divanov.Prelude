module Divanov.Prelude

module Async =
    type Lock () =
        let mutable trigger = 0
        member __.Acquire (): bool =
            System.Threading.Interlocked.CompareExchange(&trigger, 1, 0) = 0
        member __.RunOnce (thunk: unit -> unit): bool =
            if __.Acquire ()
            then thunk (); true
            else false

    let race (a1: Async<'T>) (a2: Async<'T>): Async<'T> =
        let source1 = new System.Threading.CancellationTokenSource ()
        let source2 = new System.Threading.CancellationTokenSource ()
        let token1 = source1.Token
        let token2 = source2.Token
        let lock = Lock ()
        async {
            let mutable result: 'T option = None
            let canceling (a: Async<'T>, cs: System.Threading.CancellationTokenSource): Async<unit> =
                async {
                    if lock.RunOnce (fun () -> result <- Some <| Async.RunSynchronously a)
                    then return cs.Cancel ()
                    else return ()
                }
            let asyncs = [(canceling (a1, source2), token1); (canceling (a2, source1), token2)]
            List.iter (fun (comp, tok) -> Async.Start(comp, tok)) asyncs
            return Option.get result
        }

    let raceMany (l: List<Async<'T>>): Async<'T> =
        List.reduce race l