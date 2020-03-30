namespace Divanov.Prelude

[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess>]
module Map =
    let update (key: 'a) (f: 'b -> 'b) (map: Map<'a, 'b>): Map<'a, 'b> =
        match map.TryFind key with
        | None -> map
        | Some v -> map.Add (key, f v)

    let addRegardless (key: 'a) (upd: 'b option -> 'b) (map: Map<'a, 'b>): Map<'a, 'b> =
        map.Add (key, map.TryFind key |> upd)

[<RequireQualifiedAccess>]
module List =
    let mapValues (f: 'a -> 'b) (l: ('k * 'a) list): ('k * 'b) list =
        List.map (fun elt -> (fst elt, snd elt |> f)) l
    
    let tryTail (l: 'a list): 'a list option =
        if List.isEmpty l then None else Some (List.tail l)


[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess>]
module Seq =
    let tryTail (s: 'a seq): 'a seq option =
        if Seq.isEmpty s then None else Some (Seq.tail s)

[<RequireQualifiedAccess>]
module Heap =

    type 't T =
        private {
            Comparison: 't -> 't -> int
            Count: int
            Container: 't []
        }
    
    let DEFAULTCAP = 64
    let empty (comparison: 't -> 't -> int): 't T =
        {
            Comparison = comparison
            Count = 0
            Container = Array.replicate DEFAULTCAP Unchecked.defaultof<'t>
        }

    let isEmpty (h: 't T): bool = h.Count <= 0
    let size (h: 't T): int = h.Count

    let private maxSize (h: 't T): int = Array.length h.Container
    
    let private resize (h: 't T): 't T =
        let newLen = Array.length h.Container * 2
        let newContainer = Array.replicate newLen Unchecked.defaultof<'t>
        Array.blit h.Container 0 newContainer 0 h.Count
        { h with Container = newContainer }

    let private scaleDown (h: 't T): 't T =
        let newLen = Array.length h.Container / 2
        if newLen >= DEFAULTCAP then
            let newArr = h.Container.[ 0 .. newLen - 1 ]
            { h with Container = newArr }
        else h
    
    let private left (i: int): int = 2 * i + 1
    let private right (i: int): int = 2 * i + 2
    let private parent (i: int): int = (i - 1) / 2

    let private add' (element: 't) (heap: 't T): 't T =
        if size heap + 1 <= maxSize heap then
            heap.Container.[heap.Count] <- element
            { heap with Count = heap.Count + 1 }
        else
            let newHeap = resize heap
            newHeap.Container.[newHeap.Count] <- element
            { newHeap with Count = newHeap.Count + 1 }

    let rec private siftUp (index: int) (heap: 't T): unit =
        if index <= 0 then ()
        else
            let fc = heap.Comparison
            let parentIndex = parent index
            let elt = heap.Container.[index]
            let parent = heap.Container.[parentIndex]
            if fc parent elt <= 0 then ()
            else
                heap.Container.[index] <- parent
                heap.Container.[parentIndex] <- elt
                siftUp parentIndex heap

    let push (elt: 't) (heap: 't T): 't T =
        let newHeap' = add' elt heap
        let index = newHeap'.Count - 1
        siftUp index newHeap'
        newHeap'

    let head (heap: 't T): 't =
        match heap.Count with
        | 0 -> invalidArg "heap" "an empty heap has no head"
        | _ -> heap.Container.[0]

    let tryHead (heap: 't T): 't option =
        match heap.Count with
        | 0 -> None
        | _ -> Some <| head heap

    let private tail' (h: 't T): 't T =
        let candidate = h.Container.[h.Count - 1]
        h.Container.[0] <- candidate
        if h.Count - 1 <= maxSize h / 2 && h.Count - 1 > DEFAULTCAP
        then { scaleDown h with Count = h.Count - 1 }
        else { h with Count = h.Count - 1 }

    let rec private siftDown (index: int) (h: 't T): unit =
        let cf = h.Comparison
        let lft = left index
        let rght = right index
        let elt = h.Container.[index]
        if lft >= h.Count then ()
        elif rght >= h.Count then
            let lelt = h.Container.[lft]
            if cf elt lelt <= 0 then ()
            else h.Container.[lft] <- elt ; h.Container.[index] <- lelt
        else
            let lelt = h.Container.[lft]
            let relt = h.Container.[rght]
            if cf elt lelt <= 0 && cf elt relt <= 0 then ()
            elif cf lelt relt <= 0
            then
                h.Container.[lft] <- elt ; h.Container.[index] <- lelt
                siftDown lft h
            else
                h.Container.[rght] <- elt ; h.Container.[index] <- relt
                siftDown rght h

    let tail (h: 't T): 't T =
        match h.Count with
        | 0 -> invalidArg "h" "An empty heap has no tail"
        | _ ->
            let h' = tail' h
            siftDown 0 h'
            h'

    let tryTail (h: 't T): 't T option =
        match h.Count with
        | 0 -> None
        | _ -> Some <| tail h

    let toArray (h: 't T): 't [] =
        h.Container.[ 0 .. h.Count - 1 ]

    let rec toSeq (h: 't T): 't seq =
        seq {
            if not (isEmpty h) then yield head h; yield! toSeq(tail h)
        }

    let ofArray (arr: 't [] when 't : comparison): 't T =
        let count = Array.length arr
        let sorted = Array.sort arr
        let comp: 't -> 't -> int = compare
        {
            Comparison = comp
            Count = count
            Container = sorted
        }