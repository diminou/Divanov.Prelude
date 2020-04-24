namespace Divanov.Prelude

[<RequireQualifiedAccess>]
module Dictionary =
    open System.Collections.Generic
    let inline update (pair: 't * 'u when 't: comparison) (d: Dictionary<'t, 'u>): Dictionary<'t, 'u> =
        let k, _ = pair
        d.Remove k |> ignore
        d.Add pair
        d

    let inline addCount (element: 't when 't: comparison) (d: Dictionary<'t, int>): Dictionary<'t, int> =
        if d.ContainsKey element
        then
            let count = d.Item element
            update (element, count + 1) d
        else
            d.Add (element, 1)
            d
    let inline removeCount (element: 't when 't: comparison) (d: Dictionary<'t, int>): Dictionary<'t, int> =
        if d.ContainsKey element
        then
            let count = d.Item element
            update (element, count - 1) d
        else update (element, 1) d

[<RequireQualifiedAccess>]
module Map =
    let inline update (key: 'a) (f: 'b -> 'b) (map: Map<'a, 'b>): Map<'a, 'b> =
        match map.TryFind key with
        | None -> map
        | Some v -> map.Add (key, f v)

    let inline addRegardless (key: 'a) (upd: 'b option -> 'b) (map: Map<'a, 'b>): Map<'a, 'b> =
        map.Add (key, map.TryFind key |> upd)

[<RequireQualifiedAccess>]
module List =
    let inline mapValues (f: 'a -> 'b) (l: ('k * 'a) list): ('k * 'b) list =
        List.map (fun elt -> (fst elt, snd elt |> f)) l
    
    let tryTail (l: 'a list): 'a list option =
        if List.isEmpty l then None else Some (List.tail l)


[<RequireQualifiedAccess>]
module Array =
    let tryTail (l: 'a []): 'a [] option =
        if Array.isEmpty l then None else Some (Array.tail l)
    
    let inline argMinBy (by: 't -> 'u when 'u: comparison) (arr: 't []): int =
        Array.mapi (fun i e -> Lazy.Create( fun () -> (i, by e))) arr
        |> Array.fold
            (fun (i, m) lz ->
                    let (ie, elt) = lz.Force()
                    if elt < m then (ie, elt) else (i, m)) (0, by arr.[0])
        |> fst

    let inline argMaxBy (by: 't -> 'u when 'u: comparison) (arr: 't []): int =
        Array.mapi (fun i e -> Lazy.Create( fun () -> (i, by e))) arr
        |> Array.fold
            (fun (i, m) lz ->
                    let (ie, elt) = lz.Force()
                    if elt > m then (ie, elt) else (i, m)) (0, by arr.[0])
        |> fst

    let inline argMin (arr: 't [] when 't: comparison) = argMinBy id arr

    let inline argMax (arr: 't []  when 't: comparison) = argMaxBy id arr

    let inline argMinNBy (by: 't -> 'u when 'u: comparison) (n: int) (arr: 't []) =
        let indexed: Lazy<int * 'u> [] = Array.mapi (fun i e -> Lazy.Create (fun () -> (i, by e))) arr
        let buffer = Array.take n indexed |> Array.map (fun x -> x.Force())
        let rest = Array.skip n indexed
        let updateBuffer (l: Lazy<int * 'u>): unit =
            let i, e = l.Force()
            if Array.map (fun (k, v) -> e < v) buffer |> Array.fold (||) false
            then Array.sortInPlaceBy snd buffer; buffer.[0] <- (i, e)
        Array.iter updateBuffer rest
        buffer |> Array.map fst

    let inline argMaxNBy (by: 't -> 'u when 'u: comparison) (n: int) (arr: 't []) =
        let indexed: Lazy<int * 'u> [] = Array.mapi (fun i e -> Lazy.Create (fun () -> (i, by e))) arr
        let buffer = Array.take n indexed |> Array.map (fun x -> x.Force())
        let rest = Array.skip n indexed
        let updateBuffer (l: Lazy<int * 'u>): unit =
            let i, e = l.Force()
            if Array.map (fun (k, v) -> e > v) buffer |> Array.fold (||) false
            then Array.sortInPlaceBy snd buffer; buffer.[Array.length buffer - 1] <- (i, e)
        Array.iter updateBuffer rest
        buffer |> Array.map fst |> Array.rev

    let inline argMinN (n: int) (arr: 't [] when 't: comparison) = argMinNBy id n arr

    let inline argMaxN (n: int) (arr: 't [] when 't: comparison) = argMaxNBy id n arr

    let inline items (ns: int []) (arr: 't []): 't [] =
        Array.Parallel.map (fun i -> arr.[i]) ns

[<RequireQualifiedAccess>]
module Seq =
    let tryTail (s: 'a seq): 'a seq option =
        if Seq.isEmpty s then None else Some (Seq.tail s)

type 't Heap =
    private {
                Comparison: 't -> 't -> int
                Count: int
                Container: 't []
            }

[<RequireQualifiedAccess>]
module Heap =    
    let DEFAULTCAP = 64
    let empty (comparison: 't -> 't -> int): 't Heap =
        {
            Comparison = comparison
            Count = 0
            Container = Array.replicate DEFAULTCAP Unchecked.defaultof<'t>
        }

    let isEmpty (h: 't Heap): bool = h.Count <= 0
    let size (h: 't Heap): int = h.Count

    let inline private maxSize (h: 't Heap): int = Array.length h.Container
    
    let private resize (h: 't Heap): 't Heap =
        let newLen = Array.length h.Container * 2
        let newContainer = Array.replicate newLen Unchecked.defaultof<'t>
        Array.blit h.Container 0 newContainer 0 h.Count
        { h with Container = newContainer }

    let private scaleDown (h: 't Heap): 't Heap =
        let newLen = Array.length h.Container / 2
        if newLen >= DEFAULTCAP then
            let newArr = h.Container.[ 0 .. newLen - 1 ]
            { h with Container = newArr }
        else h
    
    let private left (i: int): int = 2 * i + 1
    let private right (i: int): int = 2 * i + 2
    let private parent (i: int): int = (i - 1) / 2

    let private add' (element: 't) (heap: 't Heap): 't Heap =
        if size heap + 1 <= maxSize heap then
            heap.Container.[heap.Count] <- element
            { heap with Count = heap.Count + 1 }
        else
            let newHeap = resize heap
            newHeap.Container.[newHeap.Count] <- element
            { newHeap with Count = newHeap.Count + 1 }

    let rec private siftUp (index: int) (heap: 't Heap): unit =
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

    let push (elt: 't) (heap: 't Heap): 't Heap =
        let newHeap' = add' elt heap
        let index = newHeap'.Count - 1
        siftUp index newHeap'
        newHeap'

    let head (heap: 't Heap): 't =
        match heap.Count with
        | 0 -> invalidArg "heap" "an empty heap has no head"
        | _ -> heap.Container.[0]

    let tryHead (heap: 't Heap): 't option =
        match heap.Count with
        | 0 -> None
        | _ -> Some <| head heap

    let private tail' (h: 't Heap): 't Heap =
        let candidate = h.Container.[h.Count - 1]
        h.Container.[0] <- candidate
        if h.Count - 1 <= maxSize h / 2 && h.Count - 1 > DEFAULTCAP
        then { scaleDown h with Count = h.Count - 1 }
        else { h with Count = h.Count - 1 }

    let rec private siftDown (index: int) (h: 't Heap): unit =
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

    let tail (h: 't Heap): 't Heap =
        match h.Count with
        | 0 -> invalidArg "h" "An empty heap has no tail"
        | _ ->
            let h' = tail' h
            siftDown 0 h'
            h'

    let tryTail (h: 't Heap): 't Heap option =
        match h.Count with
        | 0 -> None
        | _ -> Some <| tail h

    let toArray (h: 't Heap): 't [] =
        h.Container.[ 0 .. h.Count - 1 ]

    let rec toSeq (h: 't Heap): 't seq =
        seq {
            if not (isEmpty h) then yield head h; yield! toSeq(tail h)
        }

    let heapify (arr: 't []) (comp : 't -> 't -> int) : 't Heap =
        let count = Array.length arr
        let heap' = { Comparison = comp ; Count = count ; Container = Array.copy arr }
        let indices = [| 0 .. count - 1|] |> Array.rev
        Array.iter (fun ind -> siftDown ind heap') indices
        heap'

    let inline ofArray (arr: 't [] when 't : comparison): 't Heap =
        let comp: 't -> 't -> int = compare
        heapify arr comp