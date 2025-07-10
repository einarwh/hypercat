module ReduceTests

open System
open Xunit
open Legal
open Cat

let testReduce (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "reduce") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testReduceLegal stack = 
    Assert.True(stack |> lookupPrecond "reduce")

let testReduceIllegal stack = 
    Assert.False(stack |> lookupPrecond "reduce")

[<Fact>]
let ``Reduce on empty stack throws stack underflow error`` () =
    let call = (fun () -> testReduce [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Reduce on stack with one item throws stack underflow error`` () =
    let call = (fun () -> testReduce [ ListItem [] ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Reduce on stack with two lists throws type error`` () =
    let call = (fun () -> testReduce [ ListItem []; ListItem [] ] [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Reduce on stack with two procs throws type error`` () =
    let call = (fun () -> testReduce [ ProcItem []; ProcItem [] ] [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Reduce over an empty list throws stack underflow error`` () =
    let call = (fun () -> testReduce [ ListItem []; ProcItem [] ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Reduce with a list with one item`` () =
    testReduce 
        [ ListItem [ IntItem 0 ]; ProcItem [] ] 
        [ IntItem 0 ]

[<Fact>]
let ``Reduce with non-empty list and non-empty proc`` () =
    testReduce 
        [ ListItem [ IntItem 1; IntItem 2; IntItem 3 ]; ProcItem [ NameItem "add" ] ]
        [ NameItem "exec"
          ProcItem [ NameItem "add" ]
          IntItem 3 
          NameItem "exec"
          ProcItem [ NameItem "add" ]
          IntItem 2 
          IntItem 1 ]

[<Fact>]
let ``Reduce is legal given stack with list and proc`` () =
    testReduceLegal [ ListItem []; ProcItem [] ] 

[<Fact>]
let ``Reduce is illegal given stack with list and proc in the wrong order`` () =
    testReduceIllegal [ ProcItem []; ListItem [] ] 

[<Fact>]
let ``Reduce is illegal given stack with just a list`` () =
    testReduceIllegal [ ListItem [] ] 

[<Fact>]
let ``Reduce is illegal given stack with just a proc`` () =
    testReduceIllegal [ ProcItem [] ] 

[<Fact>]
let ``Reduce is illegal given empty stack`` () =
    testReduceIllegal [] 
