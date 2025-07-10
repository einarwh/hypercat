module RevTests

open System
open Xunit
open Legal
open Cat

let testRev (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "rev") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testRevLegal stack = 
    Assert.True(stack |> lookupPrecond "rev")

let testRevIllegal stack = 
    Assert.False(stack |> lookupPrecond "rev")

[<Fact>]
let ``Rev on empty stack throws stack underflow error`` () =
    let call = (fun () -> testRev [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Rev on string yields reversed string`` () =
    testRev [ StringItem "123" ] [ StringItem "321" ]

[<Fact>]
let ``Rev on list yields reversed string`` () =
    testRev 
        [ ListItem [ IntItem 0; BoolItem true; StringItem "x" ] ] 
        [ ListItem [ StringItem "x"; BoolItem true; IntItem 0 ] ]

[<Fact>]
let ``Rev on empty list is not a problem`` () =
    testRev 
        [ ListItem [] ] 
        [ ListItem [] ] 

[<Fact>]
let ``Rev on integer yields type error`` () =
    let call = (fun () -> testRev [ IntItem 123 ] [])
    let ex = Assert.Throws<TypeError>(call)
    Assert.Equal<string>("TypeError \"rev\"", ex.Message)
    
[<Fact>]
let ``Rev (string) on stack of two`` () =
    testRev [ StringItem "abc"; StringItem "s" ] [ StringItem "cba"; StringItem "s" ]

[<Fact>]
let ``Rev is legal given stack with string on top`` () =
    testRevLegal [ StringItem "x"; BoolItem true ] 

[<Fact>]
let ``Rev is legal given stack with list on top`` () =
    testRevLegal [ ListItem [] ] 

[<Fact>]
let ``Rev is illegal given stack with non-string primitive on top`` () =
    testRevIllegal [ BoolItem true; StringItem "y" ] 

[<Fact>]
let ``Rev is illegal given empty stack`` () =
    testRevIllegal [] 
