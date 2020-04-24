module Divanov.Prelude.Test.Collections

open Expecto
open Divanov.Prelude

let initialMap = Map<int, float>([(1, 1.0)])

[<Tests>]
let mapTests = 
  testList "maps" [
    testCase "updating existing key" <| fun () ->
      let newValue = 2.0
      let targetMap = Map<int, float>([(1, newValue)])
      let observedMap = Map.update 1 (fun _ -> newValue) initialMap
      Expect.equal observedMap targetMap "update did not go as expected"
    testCase "updating an inexistant key" <| fun () ->
      let newValue = 2.2
      let observedMap = Map.update 2 (fun _ -> newValue) initialMap
      Expect.equal observedMap initialMap "non-update did not go as expected"

    testCase "updating regardless" <| fun () ->
      let assignNewValue (nv: float) =
        Option.map (fun x -> x + nv) >> Option.defaultValue nv
      let observedMap' = Map.addRegardless 1 (assignNewValue 3.0) initialMap
      let observedMap = Map.addRegardless 2 (assignNewValue 4.0) observedMap'
      let targetMap = Map<int, float>([(1, 4.0); (2, 4.0)])
      Expect.equal observedMap targetMap "updates did not go as expected"
  ]

[<Tests>]
let arrayTests =
  testList "arrays" [
    testCase "tryTail" <| fun () ->
      let nonEmpty = [| 1 .. 10 |]
      let empty: int [] = Array.empty<int>
      let net = Array.tryTail nonEmpty
      let et = Array.tryTail empty
      Expect.isTrue (Option.isSome net) "could not extract tail from a non-empty array"
      Expect.isTrue (Option.isNone et) "extracted a tail from an empty array"

    testCase "argmin" <| fun () ->
      let r = System.Random ()
      let arr = [| 1 .. 22 |] |> Array.map (fun _ -> r.Next ())
      let amin = Array.argMin arr
      Expect.equal arr.[amin] (Array.min arr) "argmin does not point to the location of a minimum"

    testCase "argmax" <| fun () ->
      let r = System.Random ()
      let arr = [| 1 .. 22 |] |> Array.map (fun _ -> r.Next ())
      let amax = Array.argMax arr
      Expect.equal arr.[amax] (Array.max arr) "argmax does not point to the location of a maximum"

    testCase "items" <| fun () ->
      let arr = [| 1 .. 10 |]
      let indices = [| 4; 2; 8; 6 |]
      let expectedSelection = [| 5; 3; 9; 7 |]
      let observedSelection = Array.items indices arr
      Expect.sequenceEqual observedSelection expectedSelection "Taking multiple items of an array is broken"
  ]

[<Tests>]
let listTests =
  testList "lists" [
    testCase "tryTail" <| fun () ->
      let nonEmpty = [ 1 .. 11 ]
      let empty: int list = List.empty<int>
      let net = List.tryTail nonEmpty
      let et = List.tryTail empty
      Expect.isSome net "Could not extract tail from a non-empty list"
      Expect.isNone et "Extracted a tail from an empty list"
  ]

[<Tests>]
let seqTests =
  testList "sequences" [
    testCase "tryTail" <| fun () ->
      let nonEmpty = seq [1 .. 11]
      let empty = Seq.empty<int>
      let net = Seq.tryTail nonEmpty
      let et = Seq.tryTail empty
      Expect.isSome net "Could not extract tail from a non-empty list"
      Expect.isNone et "extracted a tail from an empty seq"
  ]

[<Tests>]
let heapTests =
  testList "heaps" [
    testCase "minimum preservation" <| fun _ ->
      let r = System.Random ()
      let elements = [ 1 .. 100 ] |> List.map (fun _ -> r.Next())
      let sorted = List.sort elements |> Seq.ofList
      let comp: int -> int -> int = compare
      let h0 = Heap.empty comp
      let hsorted =
        elements |> List.fold (fun acc elt -> Heap.push elt acc) h0
        |> Heap.toSeq
      Expect.sequenceEqual sorted hsorted "Minimum property is not preserved by the heap"

    testCase "heapify" <| fun _ ->
      let r = System.Random ()
      let elements = Array.init 100 (fun _ -> r.Next())
      let sorted = Array.sort elements
      let comp: int -> int -> int = compare
      let heap = Heap.heapify elements comp
      let hsorted = Heap.toSeq heap |> Array.ofSeq
      Expect.sequenceEqual sorted hsorted "Heapify does not work correctly" 

    testCase "heapify repeated" <| fun _ ->
      let r = System.Random ()
      let maxVariants = 2
      let elements = Array.init 10 (fun _ -> r.Next(maxVariants))
      let originalElements = Array.copy elements
      let sorted = Array.sort elements
      let heap = Heap.ofArray elements
      let hsorted = Heap.toSeq heap |> Array.ofSeq
      Expect.sequenceEqual sorted hsorted "Heapify does not correctly with repeated elements"
      Expect.sequenceEqual elements originalElements "Heapify alters its argument"
  ]
