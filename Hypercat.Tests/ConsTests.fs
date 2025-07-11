module ConsTests

open System
open Xunit
open Legal
open Cat

// "cons" item list 

let testCons (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "cons") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testConsLegal stack = 
    Assert.True(stack |> lookupPrecond "cons")

let testConsIllegal stack = 
    Assert.False(stack |> lookupPrecond "cons")

[<Fact>]
let ``Cons with empty stack throws stack underflow error`` () =
    let call = (fun () -> testCons [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Cons with stack of just a list throws stack underflow error`` () =
    let call = (fun () -> testCons [ ListItem [] ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Cons - adding string to empty list`` () =
    testCons [ StringItem "foo"; ListItem [] ] [ ListItem [ StringItem "foo" ] ]

[<Fact>]
let ``Cons - adding empty list to empty list`` () =
    testCons [ ListItem []; ListItem [] ] [ ListItem [ ListItem [] ] ]

[<Fact>]
let ``Cons - adding string to non-empty list`` () =
    testCons [ StringItem "cat"; ListItem [ BoolItem true ] ] [ ListItem [ StringItem "cat"; BoolItem true ] ]

[<Fact>]
let ``Cons - adding empty list to non-empty list`` () =
    testCons [ ListItem []; ListItem [ IntItem 0 ] ] [ ListItem [ ListItem []; IntItem 0 ] ]

[<Fact>]
let ``Cons with list and string inverted throws type error`` () =
    let stack = [ ListItem []; StringItem "foo" ]
    let call = (fun () -> testCons stack [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Cons on integer yields type error`` () =
    let call = (fun () -> testCons [ IntItem 123; IntItem 123 ] [])
    let ex = Assert.Throws<TypeError>(call)
    Assert.Equal<string>("TypeError \"cons\"", ex.Message)
    
[<Fact>]
let ``Cons is legal given stack with list and bool`` () =
    testConsLegal [ BoolItem true; ListItem [] ] 

[<Fact>]
let ``Cons is legal given stack with list and string`` () =
    testConsLegal [ StringItem "cat"; ListItem [] ] 

[<Fact>]
let ``Cons is legal given stack with list and list`` () =
    testConsLegal [ ListItem []; ListItem [] ] 

[<Fact>]
let ``Cons is illegal when list and item are in the wrong order`` () =
    testConsIllegal [ ListItem []; StringItem "cat" ] 

[<Fact>]
let ``Cons is illegal given empty stack`` () =
    testConsIllegal [] 
