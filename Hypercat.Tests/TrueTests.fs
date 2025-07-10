module TrueTests

open System
open Xunit
open Legal
open Cat

let testTrue (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "true") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testTrueLegal stack = 
    Assert.True(stack |> lookupPrecond "true")

let testTrueIllegal stack = 
    Assert.False(stack |> lookupPrecond "true")

[<Fact>]
let ``True on empty stack`` () =
    testTrue [] [ BoolItem true ]

[<Fact>]
let ``True on stack of one item`` () =
    testTrue [ IntItem 0 ] [ BoolItem true; IntItem 0 ]

[<Fact>]
let ``True is legal given non-empty stack`` () =
    testTrueLegal [ IntItem 1 ] 

[<Fact>]
let ``True is legal given empty stack`` () =
    testTrueLegal [] 
