module OrTests

open System
open Xunit
open Legal
open Cat

let testOr (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "or") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testOrLegal stack = 
    Assert.True(stack |> lookupPrecond "or")

let testOrIllegal stack = 
    Assert.False(stack |> lookupPrecond "or")

[<Fact>]
let ``Or - all four combinations`` () =
    testOr [ BoolItem true; BoolItem true ] [ BoolItem true ]
    testOr [ BoolItem false; BoolItem true ] [ BoolItem true ]
    testOr [ BoolItem true; BoolItem false ] [ BoolItem true ]
    testOr [ BoolItem false; BoolItem false ] [ BoolItem false ]

[<Fact>]
let ``Or with stack underflow error`` () =
    let call = (fun () -> testOr [ BoolItem true ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Or with type error`` () =
    let call = (fun () -> testOr [ StringItem "true"; BoolItem true ] [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Or is legal given [ bool bool ]`` () =
    testOrLegal [ BoolItem true; BoolItem true ] 

[<Fact>]
let ``Or is legal given [ bool bool ... ]`` () =
    testOrLegal [ BoolItem true; BoolItem true; StringItem "???" ]

[<Fact>]
let ``Or is illegal given empty stack`` () =
    testOrIllegal []

[<Fact>]
let ``Or is illegal given [ bool ]`` () =
    testOrIllegal [ BoolItem false ]

[<Fact>]
let ``Or is illegal given [ bool string ]`` () =
    testOrIllegal [ BoolItem true ; StringItem "true" ]
