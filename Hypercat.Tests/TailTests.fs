module TailTests

open System
open Xunit
open Legal
open Cat

let testTail (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "tail") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testTailLegal stack = 
    Assert.True(stack |> lookupPrecond "tail")

let testTailIllegal stack = 
    Assert.False(stack |> lookupPrecond "tail")

[<Fact>]
let ``Tail on empty stack throws stack underflow error`` () =
    let call = (fun () -> testTail [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Tail on string yields all characters except the first as string`` () =
    testTail [ StringItem "123" ] [ StringItem "23" ]

[<Fact>]
let ``Tail on list yields all items except the first as list`` () =
    testTail 
        [ ListItem [ IntItem 0; BoolItem true; StringItem "x" ] ] 
        [ ListItem [ BoolItem true; StringItem "x" ] ]

[<Fact>]
let ``Tail on empty string yields exception`` () =
    let call = (fun () -> testTail [ StringItem "" ] [])
    let ex = Assert.Throws<Exception>(call)
    Assert.Equal<string>("Empty string has no tail", ex.Message)

[<Fact>]
let ``Tail on empty list yields exception`` () =
    let call = (fun () -> testTail [ ListItem [] ] [])
    let ex = Assert.Throws<Exception>(call)
    Assert.Equal<string>("Empty list has no tail", ex.Message)

[<Fact>]
let ``Tail on integer yields type error`` () =
    let call = (fun () -> testTail [ IntItem 123 ] [])
    let ex = Assert.Throws<TypeError>(call)
    Assert.Equal<string>("TypeError \"tail\"", ex.Message)
    
[<Fact>]
let ``Tail (string) on stack of two`` () =
    testTail [ StringItem "abc"; StringItem "s" ] [ StringItem "bc"; StringItem "s" ]

[<Fact>]
let ``Tail is legal given stack with non-empty string on top`` () =
    testTailLegal [ StringItem "x"; BoolItem true ] 

[<Fact>]
let ``Tail is legal given stack with non-empty list on top`` () =
    testTailLegal [ ListItem [ IntItem 1 ] ] 

[<Fact>]
let ``Tail is illegal given stack with empty string on top`` () =
    testTailIllegal [ StringItem ""; BoolItem true ] 

[<Fact>]
let ``Tail is illegal given stack with empty list on top`` () =
    testTailIllegal [ ListItem [] ] 

[<Fact>]
let ``Tail is illegal given stack with non-string primitive on top`` () =
    testTailIllegal [ BoolItem true; StringItem "y" ] 

[<Fact>]
let ``Tail is illegal given empty stack`` () =
    testTailIllegal [] 
