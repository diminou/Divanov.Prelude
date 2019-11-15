module Divanov.Prelude.Test.Map

open Expecto

let initialMap = Map<int, float>([(1, 1.0)])

[<Tests>]
let mapTests = 
  testList "async" [
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