module NegTests

open System
open Xunit
open Legal
open Cat

let testNeg (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "neg") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testNegLegal stack = 
    Assert.True(stack |> lookupPrecond "neg")

let testNegIllegal stack = 
    Assert.False(stack |> lookupPrecond "neg")

[<Fact>]
let ``Neg on empty stack throws stack underflow error`` () =
    let call = (fun () -> testNeg [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Neg on 1`` () =
    testNeg [ IntItem 1 ] [ IntItem -1 ]

[<Fact>]
let ``Neg on 0`` () =
    testNeg [ IntItem 0 ] [ IntItem 0 ]

[<Fact>]
let ``Neg on stack of two`` () =
    testNeg [ IntItem 3; StringItem "s" ] [ IntItem -3; StringItem "s"  ]

[<Fact>]
let ``Neg is legal given stack with integer on top`` () =
    testNegLegal [ IntItem 1; StringItem "x" ] 

[<Fact>]
let ``Neg is illegal given stack with string on top`` () =
    testNegIllegal [ StringItem "y"; IntItem 2 ] 

[<Fact>]
let ``Neg is illegal given empty stack`` () =
    testNegIllegal [] 
