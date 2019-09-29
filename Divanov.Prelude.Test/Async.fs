module Divanov.Prelude.Test.Async

open Expecto

let waitReturn (n: int): Async<int> =
  async {
    do! Async.Sleep n
    return n
  }

[<Tests>]
let async = 
  testList "async" [
    testCase "racing wait times" <| fun () ->
      let waitTimes = [0 .. 10 .. 50]
      let waitAsyncs = List.map waitReturn waitTimes
      let race = Divanov.Prelude.Async.raceMany waitAsyncs
      let result: int = Async.RunSynchronously race
      Expect.all waitTimes ((<=) result) "This must be the fastest result"
  ]