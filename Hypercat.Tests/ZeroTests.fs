module ZeroTests

open System
open Xunit
open Legal
open Cat

let testZero (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "zero") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testZeroLegal stack = 
    Assert.True(stack |> lookupPrecond "zero")

let testZeroIllegal stack = 
    Assert.False(stack |> lookupPrecond "zero")

[<Fact>]
let ``Zero on empty stack`` () =
    testZero [] [ IntItem 0 ]

[<Fact>]
let ``Zero on stack of one item`` () =
    testZero [ StringItem "" ] [ IntItem 0; StringItem "" ]

[<Fact>]
let ``Zero is legal given non-empty stack`` () =
    testZeroLegal [ StringItem "" ] 

[<Fact>]
let ``Zero is legal given empty stack`` () =
    testZeroLegal [] 
