module PopTests

open System
open Xunit
open Legal
open Cat

let testPop (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "pop") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testPopLegal stack = 
    Assert.True(stack |> lookupPrecond "pop")

let testPopIllegal stack = 
    Assert.False(stack |> lookupPrecond "pop")

[<Fact>]
let ``Pop on empty stack throws stack underflow error`` () =
    let call = (fun () -> testPop [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Pop on stack of one`` () =
    testPop [ IntItem 0 ] []

[<Fact>]
let ``Pop on stack of two`` () =
    testPop [ IntItem 0; StringItem "s" ] [ StringItem "s" ]

[<Fact>]
let ``Pop is legal given non-empty stack`` () =
    testPopLegal [ IntItem 1 ] 

[<Fact>]
let ``Pop is illegal given empty stack`` () =
    testPopIllegal [] 
