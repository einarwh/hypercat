module HeadTests

open System
open Xunit
open Legal
open Cat

let testHead (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "head") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testHeadLegal stack = 
    Assert.True(stack |> lookupPrecond "head")

let testHeadIllegal stack = 
    Assert.False(stack |> lookupPrecond "head")

[<Fact>]
let ``Head on empty stack throws stack underflow error`` () =
    let call = (fun () -> testHead [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Head on string yields first character as string`` () =
    testHead [ StringItem "123" ] [ StringItem "1" ]

[<Fact>]
let ``Head on list yields first item in list`` () =
    testHead 
        [ ListItem [ IntItem 0; BoolItem true; StringItem "x" ] ] 
        [ IntItem 0 ]

[<Fact>]
let ``Head on empty string yields exception`` () =
    let call = (fun () -> testHead [ StringItem "" ] [])
    let ex = Assert.Throws<Exception>(call)
    Assert.Equal<string>("Empty string has no head", ex.Message)

[<Fact>]
let ``Head on empty list yields exception`` () =
    let call = (fun () -> testHead [ ListItem [] ] [])
    let ex = Assert.Throws<Exception>(call)
    Assert.Equal<string>("Empty list has no head", ex.Message)

[<Fact>]
let ``Head on integer yields type error`` () =
    let call = (fun () -> testHead [ IntItem 123 ] [])
    let ex = Assert.Throws<TypeError>(call)
    Assert.Equal<string>("TypeError \"head\"", ex.Message)
    
[<Fact>]
let ``Head (string) on stack of two`` () =
    testHead [ StringItem "abc"; StringItem "s" ] [ StringItem "a"; StringItem "s" ]

[<Fact>]
let ``Head is legal given stack with non-empty string on top`` () =
    testHeadLegal [ StringItem "x"; BoolItem true ] 

[<Fact>]
let ``Head is legal given stack with non-empty list on top`` () =
    testHeadLegal [ ListItem [ IntItem 1 ] ] 

[<Fact>]
let ``Head is illegal given stack with empty string on top`` () =
    testHeadIllegal [ StringItem ""; BoolItem true ] 

[<Fact>]
let ``Head is illegal given stack with empty list on top`` () =
    testHeadIllegal [ ListItem [] ] 

[<Fact>]
let ``Head is illegal given stack with non-string primitive on top`` () =
    testHeadIllegal [ BoolItem true; StringItem "y" ] 

[<Fact>]
let ``Head is illegal given empty stack`` () =
    testHeadIllegal [] 
