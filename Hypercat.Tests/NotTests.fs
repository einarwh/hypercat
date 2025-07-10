module NotTests

open System
open Xunit
open Legal
open Cat

let testNot (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "not") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testNotLegal stack = 
    Assert.True(stack |> lookupPrecond "not")

let testNotIllegal stack = 
    Assert.False(stack |> lookupPrecond "not")

[<Fact>]
let ``Not on empty stack throws stack underflow error`` () =
    let call = (fun () -> testNot [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Not on false yields true`` () =
    testNot [ BoolItem false ] [ BoolItem true ]

[<Fact>]
let ``Not on true yields false`` () =
    testNot [ BoolItem true ] [ BoolItem false ]

[<Fact>]
let ``Not on integer is type error`` () =
    let call = fun () -> testNot [ IntItem 1 ] []
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Not on stack of two`` () =
    testNot [ BoolItem false; StringItem "s" ] [ BoolItem true; StringItem "s"  ]

[<Fact>]
let ``Not is legal given stack with boolean on top`` () =
    testNotLegal [ BoolItem true; StringItem "x" ] 

[<Fact>]
let ``Not is illegal given stack with string on top`` () =
    testNotIllegal [ StringItem "y"; IntItem 2 ] 

[<Fact>]
let ``Not is illegal given empty stack`` () =
    testNotIllegal [] 
