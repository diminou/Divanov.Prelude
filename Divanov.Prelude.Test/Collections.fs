module Divanov.Prelude.Test.Collections

open Expecto

let initialMap = Map<int, float>([(1, 1.0)])

[<Tests>]
let mapTests = 
  testList "maps" [
    testCase "updating existing key" <| fun () ->
      let newValue = 2.0
      let targetMap = Map<int, float>([(1, newValue)])
      let observedMap = Divanov.Prelude.Map.update 1 (fun _ -> newValue) initialMap
      Expect.equal observedMap targetMap "update did not go as expected"
    testCase "updating an inexistant key" <| fun () ->
      let newValue = 2.2
      let observedMap = Divanov.Prelude.Map.update 2 (fun _ -> newValue) initialMap
      Expect.equal observedMap initialMap "non-update did not go as expected"

    testCase "updating regardless" <| fun () ->
      let assignNewValue (nv: float) =
        Option.map (fun x -> x + nv) >> Option.defaultValue nv
      let observedMap' = Divanov.Prelude.Map.addRegardless 1 (assignNewValue 3.0) initialMap
      let observedMap = Divanov.Prelude.Map.addRegardless 2 (assignNewValue 4.0) observedMap'
      let targetMap = Map<int, float>([(1, 4.0); (2, 4.0)])
      Expect.equal observedMap targetMap "updates did not go as expected"
  ]

[<Tests>]
let arrayTests =
  testList "arrays" [
    testCase "tryTail" <| fun () ->
      let nonEmpty = [| 1 .. 10 |]
      let empty: int [] = Array.empty<int>
      let net = Divanov.Prelude.Array.tryTail nonEmpty
      let et = Divanov.Prelude.Array.tryTail empty
      Expect.isTrue (Option.isSome net) "could not extract tail from a non-empty array"
      Expect.isTrue (Option.isNone et) "extracted a tail from an empty array"

    testCase "argmin" <| fun () ->
      let r = System.Random ()
      let arr = [| 1 .. 22 |] |> Array.map (fun _ -> r.Next ())
      let amin = Divanov.Prelude.Array.argMin arr
      Expect.equal arr.[amin] (Array.min arr) "argmin does not point to the location of a minimum"

    testCase "items" <| fun () ->
      let arr = [| 1 .. 10 |]
      let indices = [| 4; 2; 8; 6 |]
      let expectedSelection = [| 5; 3; 9; 7 |]
      let observedSelection = Divanov.Prelude.Array.items indices arr
      Expect.sequenceEqual observedSelection expectedSelection "Taking multiple items of an array is broken"
  ]

[<Tests>]
let listTests =
  testList "lists" [
    testCase "tryTail" <| fun () ->
      let nonEmpty = [ 1 .. 11 ]
      let empty: int list = List.empty<int>
      let net = Divanov.Prelude.List.tryTail nonEmpty
      let et = Divanov.Prelude.List.tryTail empty
      Expect.isSome net "Could not extract tail from a non-empty list"
      Expect.isNone et "Extracted a tail from an empty list"
  ]

[<Tests>]
let seqTests =
  testList "sequences" [
    testCase "tryTail" <| fun () ->
      let nonEmpty = seq [1 .. 11]
      let empty = Seq.empty<int>
      let net = Divanov.Prelude.Seq.tryTail nonEmpty
      let et = Divanov.Prelude.Seq.tryTail empty
      Expect.isSome net "Could not extract tail from a non-empty list"
      Expect.isNone et "extracted a tail from an empty seq"
  ]