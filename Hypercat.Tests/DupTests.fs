module DupTests

open System
open Xunit
open Legal
open Cat

let testDup (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "dup") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testDupLegal stack = 
    Assert.True(stack |> lookupPrecond "dup")

let testDupIllegal stack = 
    Assert.False(stack |> lookupPrecond "dup")

[<Fact>]
let ``Dup on empty stack throws stack underflow error`` () =
    let call = (fun () -> testDup [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Dup on stack of one int item`` () =
    testDup [ IntItem 0 ] [ IntItem 0; IntItem 0 ]

[<Fact>]
let ``Dup on stack of one string item`` () =
    testDup [ StringItem "s" ] [ StringItem "s"; StringItem "s" ]

[<Fact>]
let ``Dup on stack of two`` () =
    testDup [ IntItem 0; StringItem "s" ] [ IntItem 0; IntItem 0; StringItem "s"  ]

[<Fact>]
let ``Dup is legal given non-empty stack`` () =
    testDupLegal [ IntItem 1 ] 

[<Fact>]
let ``Dup is legal given list marker`` () =
    testDupLegal [ ListMarker ] 

[<Fact>]
let ``Dup is illegal given proc marker`` () =
    testDupIllegal [ ProcMarker ] 

[<Fact>]
let ``Dup is illegal given empty stack`` () =
    testDupIllegal [] 
