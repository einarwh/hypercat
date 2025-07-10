module FlattenTests

open System
open Xunit
open Legal
open Cat

let testFlatten (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "flatten") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testFlattenLegal stack = 
    Assert.True(stack |> lookupPrecond "flatten")

let testFlattenIllegal stack = 
    Assert.False(stack |> lookupPrecond "flatten")

[<Fact>]
let ``Flatten on empty stack throws stack underflow error`` () =
    let call = (fun () -> testFlatten [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Flatten on list with items yields stack with items prepended`` () =
    testFlatten 
        [ ListItem [ IntItem 0; BoolItem true; StringItem "x" ]; StringItem "str" ] 
        [ IntItem 0; BoolItem true; StringItem "x"; StringItem "str" ]

[<Fact>]
let ``Flatten on empty list yields stack with no items prepended`` () =
    testFlatten 
        [ ListItem []; StringItem "str" ] 
        [ StringItem "str" ]

[<Fact>]
let ``Flatten on string yields type error`` () =
    let call = (fun () -> testFlatten [ StringItem "str" ] [])
    let ex = Assert.Throws<TypeError>(call)
    Assert.Equal<string>("TypeError \"flatten\"", ex.Message)

[<Fact>]
let ``Flatten on integer yields type error`` () =
    let call = (fun () -> testFlatten [ IntItem 123 ] [])
    let ex = Assert.Throws<TypeError>(call)
    Assert.Equal<string>("TypeError \"flatten\"", ex.Message)
    
[<Fact>]
let ``Flatten is legal given stack with list on top`` () =
    testFlattenLegal [ ListItem [] ] 

[<Fact>]
let ``Flatten is illegal given stack with string on top`` () =
    testFlattenIllegal [ StringItem ""; ListItem [] ] 

[<Fact>]
let ``Flatten is illegal given empty stack`` () =
    testFlattenIllegal [] 
