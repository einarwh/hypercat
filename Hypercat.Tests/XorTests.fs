module XorTests

open System
open Xunit
open Legal
open Cat

let testXor (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "xor") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testXorLegal stack = 
    Assert.True(stack |> lookupPrecond "xor")

let testXorIllegal stack = 
    Assert.False(stack |> lookupPrecond "xor")

[<Fact>]
let ``Xor - all four combinations`` () =
    testXor [ BoolItem true; BoolItem true ] [ BoolItem false ]
    testXor [ BoolItem false; BoolItem true ] [ BoolItem true ]
    testXor [ BoolItem true; BoolItem false ] [ BoolItem true ]
    testXor [ BoolItem false; BoolItem false ] [ BoolItem false ]

[<Fact>]
let ``Xor with stack underflow error`` () =
    let call = (fun () -> testXor [ BoolItem true ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Xor with type error`` () =
    let call = (fun () -> testXor [ StringItem "true"; BoolItem true ] [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Xor is legal given [ bool bool ]`` () =
    testXorLegal [ BoolItem true; BoolItem true ] 

[<Fact>]
let ``Xor is legal given [ bool bool ... ]`` () =
    testXorLegal [ BoolItem true; BoolItem true; StringItem "???" ]

[<Fact>]
let ``Xor is illegal given empty stack`` () =
    testXorIllegal []

[<Fact>]
let ``Xor is illegal given [ bool ]`` () =
    testXorIllegal [ BoolItem false ]

[<Fact>]
let ``Xor is illegal given [ bool string ]`` () =
    testXorIllegal [ BoolItem true ; StringItem "true" ]
