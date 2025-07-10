module IntTests

open System
open Xunit
open Legal
open Cat

let testInt (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "int") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testIntLegal stack = 
    Assert.True(stack |> lookupPrecond "int")

let testIntIllegal stack = 
    Assert.False(stack |> lookupPrecond "int")

[<Fact>]
let ``Int on empty stack throws stack underflow error`` () =
    let call = (fun () -> testInt [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Int on valid string yields int`` () =
    testInt [ StringItem "123" ] [ IntItem 123 ]

[<Fact>]
let ``Int on invalid string yields type error`` () =
    let call = (fun () -> testInt [ StringItem "xyz" ] [])
    let ex = Assert.Throws<Exception>(call)
    Assert.Equal<string>("xyz is not an integer value", ex.Message)
    
[<Fact>]
let ``Int on stack of two`` () =
    testInt [ StringItem "1"; StringItem "s" ] [ IntItem 1; StringItem "s" ]

[<Fact>]
let ``Int is legal given stack with string on top`` () =
    testIntLegal [ StringItem "x"; BoolItem true ] 

[<Fact>]
let ``Int is illegal given stack with non-string on top`` () =
    testIntIllegal [ BoolItem true; StringItem "y" ] 

[<Fact>]
let ``Int is illegal given empty stack`` () =
    testIntIllegal [] 
