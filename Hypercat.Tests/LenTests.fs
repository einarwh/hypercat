module LenTests

open System
open Xunit
open Legal
open Cat

let testLen (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "len") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testLenLegal stack = 
    Assert.True(stack |> lookupPrecond "len")

let testLenIllegal stack = 
    Assert.False(stack |> lookupPrecond "len")

[<Fact>]
let ``Len on empty stack throws stack underflow error`` () =
    let call = (fun () -> testLen [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Len on string yields length of string`` () =
    testLen [ StringItem "123" ] [ IntItem 3 ]

[<Fact>]
let ``Len on list yields length of list`` () =
    testLen 
        [ ListItem [ IntItem 0; BoolItem true; StringItem "x" ] ] 
        [ IntItem 3 ]

[<Fact>]
let ``Len on empty string yields zero`` () =
    testLen [ StringItem "" ] [ IntItem 0 ]

[<Fact>]
let ``Len on empty list yields zero`` () =
    testLen [ ListItem [] ] [ IntItem 0 ]

[<Fact>]
let ``Len on integer yields type error`` () =
    let call = (fun () -> testLen [ IntItem 123 ] [])
    let ex = Assert.Throws<TypeError>(call)
    Assert.Equal<string>("TypeError \"len\"", ex.Message)
    
[<Fact>]
let ``Len (string) on stack of two`` () =
    testLen [ StringItem "hello"; StringItem "s" ] [ IntItem 5; StringItem "s" ]

[<Fact>]
let ``Len is legal given stack with string on top`` () =
    testLenLegal [ StringItem "x"; BoolItem true ] 

[<Fact>]
let ``Len is legal given stack with list on top`` () =
    testLenLegal [ ListItem [ IntItem 1 ] ] 

[<Fact>]
let ``Len is legal given stack with empty string on top`` () =
    testLenLegal [ StringItem ""; BoolItem true ] 

[<Fact>]
let ``Len is legal given stack with empty list on top`` () =
    testLenLegal [ ListItem [] ] 

[<Fact>]
let ``Len is illegal given stack with non-string primitive on top`` () =
    testLenIllegal [ BoolItem true; StringItem "y" ] 

[<Fact>]
let ``Len is illegal given empty stack`` () =
    testLenIllegal [] 
