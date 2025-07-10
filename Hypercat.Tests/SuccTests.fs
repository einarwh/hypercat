module SuccTests

open System
open Xunit
open Legal
open Cat

let testSucc (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "succ") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testSuccLegal stack = 
    Assert.True(stack |> lookupPrecond "succ")

let testSuccIllegal stack = 
    Assert.False(stack |> lookupPrecond "succ")

[<Fact>]
let ``Succ on empty stack throws stack underflow error`` () =
    let call = (fun () -> testSucc [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Succ on stack of one int item`` () =
    testSucc [ IntItem 0 ] [ IntItem 1 ]

[<Fact>]
let ``Succ on stack of two`` () =
    testSucc [ IntItem 0; StringItem "s" ] [ IntItem 1; StringItem "s"  ]

[<Fact>]
let ``Succ is legal given stack with integer on top`` () =
    testSuccLegal [ IntItem 1; StringItem "x" ] 

[<Fact>]
let ``Succ is illegal given stack with string on top`` () =
    testSuccIllegal [ StringItem "y"; IntItem 2 ] 

[<Fact>]
let ``Succ is illegal given empty stack`` () =
    testSuccIllegal [] 
