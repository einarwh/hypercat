module SwapTests

open System
open Xunit
open Legal
open Cat

let testSwap (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "swap") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testSwapLegal stack = 
    Assert.True(stack |> lookupPrecond "swap")

let testSwapIllegal stack = 
    Assert.False(stack |> lookupPrecond "swap")

[<Fact>]
let ``Swap on empty stack throws stack underflow error`` () =
    let call = (fun () -> testSwap [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Swap on stack with one item throws stack underflow error`` () =
    let call = (fun () -> testSwap [ IntItem 1 ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Swap on stack with two items swaps them`` () =
    testSwap [ IntItem 0; StringItem "s" ] [ StringItem "s"; IntItem 0 ]

[<Fact>]
let ``Swap on stack with three items swaps top two`` () =
    testSwap [ IntItem 0; StringItem "s"; IntItem 2 ] [ StringItem "s"; IntItem 0; IntItem 2 ]

[<Fact>]
let ``Swap is legal given stack with two items`` () =
    testSwapLegal [ IntItem 1; StringItem "x" ] 

[<Fact>]
let ``Swap is legal given stack with three items`` () =
    testSwapLegal [ IntItem 1; StringItem "x"; StringItem "x" ] 

[<Fact>]
let ``Swap is illegal given empty stack`` () =
    testSwapIllegal [] 

[<Fact>]
let ``Swap is illegal given stack with one item`` () =
    testSwapIllegal [ IntItem 0 ] 
