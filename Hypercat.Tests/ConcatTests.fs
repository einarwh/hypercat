module ConcatTests

open System
open Xunit
open Legal
open Cat

let testConcat (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "concat") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testConcatLegal stack = 
    Assert.True(stack |> lookupPrecond "concat")

let testConcatIllegal stack = 
    Assert.False(stack |> lookupPrecond "concat")

[<Fact>]
let ``Concat on empty stack throws stack underflow error`` () =
    let call = (fun () -> testConcat [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Concat on stack with a single string throws stack underflow error`` () =
    let call = (fun () -> testConcat [ StringItem "str" ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Concat on stack with a single list throws stack underflow error`` () =
    let call = (fun () -> testConcat [ ListItem [] ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Concat on stack with list and string throws type error`` () =
    let call = (fun () -> testConcat [ ListItem []; StringItem "" ] [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Concat on two empty lists gives an empty list`` () =
    testConcat 
        [ ListItem []; ListItem [] ] 
        [ ListItem [] ]

[<Fact>]
let ``Concat on two lists gives the joined list`` () =
    testConcat 
        [ ListItem [ IntItem 0 ] 
          ListItem [ StringItem "a"; ] ]
        [ ListItem [ IntItem 0; StringItem "a" ] ]

[<Fact>]
let ``Concat on two empty strings gives an empty string`` () =
    testConcat 
        [ StringItem ""; StringItem "" ] 
        [ StringItem "" ]

[<Fact>]
let ``Concat on two strings gives the joined string`` () =
    testConcat 
        [ StringItem "a"; StringItem "b" ] 
        [ StringItem "ab" ]

[<Fact>]
let ``Concat on two integers yields type error`` () =
    let call = (fun () -> testConcat [ IntItem 123; IntItem 123 ] [])
    let ex = Assert.Throws<TypeError>(call)
    Assert.Equal<string>("TypeError \"concat\"", ex.Message)
    
[<Fact>]
let ``Concat is legal given stack with two strings`` () =
    testConcatLegal [ StringItem "x"; StringItem "y" ] 

[<Fact>]
let ``Concat is legal given stack with two lists`` () =
    testConcatLegal [ ListItem []; ListItem [] ] 

[<Fact>]
let ``Concat is illegal given stack with just one string`` () =
    testConcatIllegal [ StringItem "" ] 

[<Fact>]
let ``Concat is illegal given stack with just one list`` () =
    testConcatIllegal [ ListItem [] ] 

[<Fact>]
let ``Concat is illegal given stack with two ints`` () =
    testConcatIllegal [ IntItem 0; IntItem 1 ] 

[<Fact>]
let ``Concat is illegal given empty stack`` () =
    testConcatIllegal [] 
