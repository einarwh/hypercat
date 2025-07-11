module TakeTests

open System
open Xunit
open Legal
open Cat

// take count list 

let testTake (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "take") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testTakeLegal stack = 
    Assert.True(stack |> lookupPrecond "take")

let testTakeIllegal stack = 
    Assert.False(stack |> lookupPrecond "take")

[<Fact>]
let ``Take with empty stack throws stack underflow error`` () =
    let call = (fun () -> testTake [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Take with stack of just a list throws stack underflow error`` () =
    let call = (fun () -> testTake [ ListItem [] ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Take - take 0 elements from list of 2 elements`` () =
    testTake [ IntItem 0; ListItem [ StringItem "a"; StringItem "b" ] ] [ ListItem [] ]

[<Fact>]
let ``Take - take 1 element from list of 2 elements`` () =
    testTake [ IntItem 1; ListItem [ StringItem "a"; StringItem "b" ] ] [ ListItem [  StringItem "a" ] ]

[<Fact>]
let ``Take - take 2 elements from list of 2 elements`` () =
    testTake [ IntItem 2; ListItem [ StringItem "a"; StringItem "b" ] ] [ ListItem [ StringItem "a"; StringItem "b" ] ]

[<Fact>]
let ``Take - take 3 elements from list of 2 elements`` () =
    testTake [ IntItem 3; ListItem [ StringItem "a"; StringItem "b" ] ] [ ListItem [ StringItem "a"; StringItem "b" ] ]

[<Fact>]
let ``Take with list and count inverted throws type error`` () =
    let call = (fun () -> testTake [ ListItem []; IntItem 1 ] [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Take on integer yields type error`` () =
    let call = (fun () -> testTake [ IntItem 123; IntItem 123 ] [])
    let ex = Assert.Throws<TypeError>(call)
    Assert.Equal<string>("TypeError \"take\"", ex.Message)
    
[<Fact>]
let ``Take is legal given stack with list and integer`` () =
    testTakeLegal [ IntItem 0; ListItem [] ] 

[<Fact>]
let ``Take is illegal given stack with list and bool`` () =
    testTakeIllegal [ BoolItem false; ListItem [] ] 

[<Fact>]
let ``Take is illegal given stack with integer and list in wrong order`` () =
    testTakeIllegal [ ListItem []; IntItem 0 ] 

[<Fact>]
let ``Take is illegal given just list`` () =
    testTakeIllegal [ ListItem [] ] 

[<Fact>]
let ``Take is illegal given empty stack`` () =
    testTakeIllegal [] 
