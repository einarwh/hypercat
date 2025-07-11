module RollTests

open System
open Xunit
open Legal
open Cat

let testRoll (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "roll") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testRollLegal stack = 
    Assert.True(stack |> lookupPrecond "roll")

let testRollIllegal stack = 
    Assert.False(stack |> lookupPrecond "roll")

[<Fact>]
let ``Roll on empty stack throws stack underflow error`` () =
    let call = (fun () -> testRoll [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Roll on stack with one item throws stack underflow error`` () =
    let call = (fun () -> testRoll [ IntItem 1 ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Roll with nothing to roll is ok if you roll nothing`` () =
    testRoll [ IntItem 3; IntItem 0 ] []

[<Fact>]
let ``Roll throws type error unless there are two integers on top`` () =
    let stack = [ IntItem 3; ListMarker ]
    let call = (fun () -> testRoll stack [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Roll three items one step towards the top of the stack`` () =
    testRoll 
        [ IntItem 1; IntItem 3; StringItem "x"; BoolItem true; ListItem []; StringItem "y" ] 
        [ BoolItem true; ListItem []; StringItem "x"; StringItem "y" ]

[<Fact>]
let ``Roll three items one step towards the bottom of the stack`` () =
    testRoll 
        [ IntItem -1; IntItem 3; StringItem "x"; BoolItem true; ListItem []; StringItem "y" ] 
        [ ListItem []; StringItem "x"; BoolItem true; StringItem "y" ]

[<Fact>]
let ``Roll three items two steps towards the top of the stack`` () =
    testRoll 
        [ IntItem 2; IntItem 3; StringItem "x"; BoolItem true; ListItem []; StringItem "y" ] 
        [ ListItem []; StringItem "x"; BoolItem true; StringItem "y" ]

[<Fact>]
let ``Roll is legal given stack with at least two integers on top`` () =
    testRollLegal [ IntItem 1; IntItem 0 ] 

[<Fact>]
let ``Roll is illegal given empty stack`` () =
    testRollIllegal [] 

[<Fact>]
let ``Roll is illegal given stack with one item`` () =
    testRollIllegal [ IntItem 0 ] 
