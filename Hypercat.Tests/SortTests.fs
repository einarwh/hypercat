module SortTests

open System
open Xunit
open Legal
open Cat

let testSort (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "sort") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testSortLegal stack = 
    Assert.True(stack |> lookupPrecond "sort")

let testSortIllegal stack = 
    Assert.False(stack |> lookupPrecond "sort")

[<Fact>]
let ``Sort on empty stack throws stack underflow error`` () =
    let call = (fun () -> testSort [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Sort on list with items yields sorted list`` () =
    testSort 
        [ ListItem [ IntItem 2; IntItem 0; IntItem 1 ]; StringItem "str" ] 
        [ ListItem [ IntItem 0; IntItem 1; IntItem 2 ]; StringItem "str" ]

[<Fact>]
let ``Sort on empty list yields empty list`` () =
    testSort 
        [ ListItem []; StringItem "str" ] 
        [ ListItem []; StringItem "str" ]

[<Fact>]
let ``Sort on string yields type error`` () =
    let call = (fun () -> testSort [ StringItem "str" ] [])
    let ex = Assert.Throws<TypeError>(call)
    Assert.Equal<string>("TypeError \"sort\"", ex.Message)

[<Fact>]
let ``Sort on integer yields type error`` () =
    let call = (fun () -> testSort [ IntItem 123 ] [])
    let ex = Assert.Throws<TypeError>(call)
    Assert.Equal<string>("TypeError \"sort\"", ex.Message)
    
[<Fact>]
let ``Sort is legal given stack with list on top`` () =
    testSortLegal [ ListItem [] ] 

[<Fact>]
let ``Sort is illegal given stack with string on top`` () =
    testSortIllegal [ StringItem ""; ListItem [] ] 

[<Fact>]
let ``Sort is illegal given empty stack`` () =
    testSortIllegal [] 
