module FalseTests

open System
open Xunit
open Legal
open Cat

let testFalse (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "false") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testFalseLegal stack = 
    Assert.True(stack |> lookupPrecond "false")

let testFalseIllegal stack = 
    Assert.False(stack |> lookupPrecond "false")

[<Fact>]
let ``False on empty stack`` () =
    testFalse [] [ BoolItem false ]

[<Fact>]
let ``False on stack of one item`` () =
    testFalse [ IntItem 0 ] [ BoolItem false; IntItem 0 ]

[<Fact>]
let ``False is legal given non-empty stack`` () =
    testFalseLegal [ IntItem 1 ] 

[<Fact>]
let ``False is legal given empty stack`` () =
    testFalseLegal [] 
