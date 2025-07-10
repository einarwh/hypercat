module PredTests

open System
open Xunit
open Legal
open Cat

let testPred (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "pred") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testPredLegal stack = 
    Assert.True(stack |> lookupPrecond "pred")

let testPredIllegal stack = 
    Assert.False(stack |> lookupPrecond "pred")

[<Fact>]
let ``Pred on empty stack throws stack underflow error`` () =
    let call = (fun () -> testPred [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Pred on stack of one int item`` () =
    testPred [ IntItem 4 ] [ IntItem 3 ]

[<Fact>]
let ``Pred on stack of one int item - zero`` () =
    testPred [ IntItem 0 ] [ IntItem -1 ]

[<Fact>]
let ``Pred on stack of two`` () =
    testPred [ IntItem 1; StringItem "s" ] [ IntItem 0; StringItem "s"  ]

[<Fact>]
let ``Pred is legal given stack with integer on top`` () =
    testPredLegal [ IntItem 0; StringItem "x" ] 

[<Fact>]
let ``Pred is illegal given stack with string on top`` () =
    testPredIllegal [ StringItem "y"; IntItem 2 ] 

[<Fact>]
let ``Pred is illegal given empty stack`` () =
    testPredIllegal [] 
