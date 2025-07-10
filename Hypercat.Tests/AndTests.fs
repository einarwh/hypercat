module AndTests

open System
open Xunit
open Legal
open Cat

let testAnd (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "and") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testAndLegal stack = 
    Assert.True(stack |> lookupPrecond "and")

let testAndIllegal stack = 
    Assert.False(stack |> lookupPrecond "and")

[<Fact>]
let ``And - all four combinations`` () =
    testAnd [ BoolItem true; BoolItem true ] [ BoolItem true ]
    testAnd [ BoolItem false; BoolItem true ] [ BoolItem false ]
    testAnd [ BoolItem false; BoolItem false ] [ BoolItem false ]
    testAnd [ BoolItem true; BoolItem false ] [ BoolItem false ]

[<Fact>]
let ``And with stack underflow error`` () =
    let call = (fun () -> testAnd [ BoolItem true ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``And with type error`` () =
    let call = (fun () -> testAnd [ StringItem "true"; BoolItem true ] [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``And is legal given [ bool bool ]`` () =
    testAndLegal [ BoolItem true; BoolItem true ] 

[<Fact>]
let ``And is legal given [ bool bool ... ]`` () =
    testAndLegal [ BoolItem true; BoolItem true; StringItem "???" ]

[<Fact>]
let ``And is illegal given empty stack`` () =
    testAndIllegal []

[<Fact>]
let ``And is illegal given [ bool ]`` () =
    testAndIllegal [ BoolItem false ]

[<Fact>]
let ``And is illegal given [ bool string ]`` () =
    testAndIllegal [ BoolItem true ; StringItem "true" ]
