module MapTests

open System
open Xunit
open Legal
open Cat

// "map" proc list 

let testMap (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "map") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testMapLegal stack = 
    Assert.True(stack |> lookupPrecond "map")

let testMapIllegal stack = 
    Assert.False(stack |> lookupPrecond "map")

[<Fact>]
let ``Map on empty stack throws stack underflow error`` () =
    let call = (fun () -> testMap [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Map on stack with one item throws stack underflow error`` () =
    let call = (fun () -> testMap [ ListItem [] ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Map on stack with two lists throws type error`` () =
    let call = (fun () -> testMap [ ListItem []; ListItem [] ] [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Map on stack with two procs throws type error`` () =
    let call = (fun () -> testMap [ ProcItem []; ProcItem [] ] [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Map over an empty list gives an empty list`` () =
    testMap 
        [ ProcItem [ NameItem "succ" ]; ListItem [] ] 
        [ ListItem [] ]

[<Fact>]
let ``Map with an empty proc`` () =
    testMap 
        [ ProcItem []; ListItem [ IntItem 0 ] ] 
        [ ListItem [ NameItem "exec"; ProcItem []; IntItem 0 ] ]

[<Fact>]
let ``Map with non-empty list and non-empty proc`` () =
    testMap 
        [ ProcItem [ NameItem "succ" ]; ListItem [ IntItem 1; IntItem 2; IntItem 3 ] ]
        [ ListItem [ NameItem "exec"
                     ProcItem [ NameItem "succ" ]
                     IntItem 1 
                     NameItem "exec"
                     ProcItem [ NameItem "succ" ]
                     IntItem 2
                     NameItem "exec"
                     ProcItem [ NameItem "succ" ]
                     IntItem 3 ] ]

[<Fact>]
let ``Map is legal given stack with list and proc`` () =
    testMapLegal [ ProcItem []; ListItem [] ] 

[<Fact>]
let ``Map is illegal given stack with list and proc in the wrong order`` () =
    testMapIllegal [ ListItem []; ProcItem [] ] 

[<Fact>]
let ``Map is illegal given stack with just a list`` () =
    testMapIllegal [ ListItem [] ] 

[<Fact>]
let ``Map is illegal given stack with just a proc`` () =
    testMapIllegal [ ProcItem [] ] 

[<Fact>]
let ``Map is illegal given empty stack`` () =
    testMapIllegal [] 
