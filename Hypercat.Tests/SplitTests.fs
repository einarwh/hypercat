module SplitTests

open System
open Xunit
open Legal
open Cat

let testSplit (originalStack : Cat) (expectedStack : Cat) = 
    match pushInput (NameInput "split") originalStack with 
    | Reduction actualStack ->
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testSplitLegal stack = 
    Assert.True(stack |> lookupPrecond "split")

let testSplitIllegal stack = 
    Assert.False(stack |> lookupPrecond "split")

[<Fact>]
let ``Split on empty stack throws stack underflow error`` () =
    let call = (fun () -> testSplit [] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Split on stack with one string throws stack underflow error`` () =
    let call = (fun () -> testSplit [ StringItem "foo bar" ] [])
    Assert.Throws<StackUnderflowError>(call)

[<Fact>]
let ``Split on stack with integer throws type error`` () =
    let call = (fun () -> testSplit [ IntItem 1; StringItem "foo" ] [])
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Split "foo bar" on " " yields list of "foo" and "bar" `` () =
    testSplit [ StringItem " "; StringItem "foo bar" ] [ ListItem [ StringItem "foo"; StringItem "bar" ] ]

[<Fact>]
let ``Split "foo bar quux" on " " yields list of "foo", "bar", "quux" `` () =
    testSplit [ StringItem " "; StringItem "foo bar quux" ] [ ListItem [ StringItem "foo"; StringItem "bar"; StringItem "quux" ] ]

[<Fact>]
let ``Split "ouroboros" on "o" yields list of "", "ur", "b", "r", "s"`` () =
    testSplit [ StringItem "o"; StringItem "ouroboros"] [ ListItem [ StringItem ""; StringItem "ur"; StringItem "b"; StringItem "r"; StringItem "s" ] ]

[<Fact>]
let ``Split is legal given at least two strings`` () =
    testSplitLegal [ StringItem "a"; StringItem "b" ] 

[<Fact>]
let ``Split is illegal given string and something else`` () =
    testSplitIllegal [ StringItem "a"; BoolItem true ] 

[<Fact>]
let ``Split is illegal given just one string`` () =
    testSplitIllegal [ StringItem "foo" ] 

[<Fact>]
let ``Split is illegal given empty stack`` () =
    testSplitIllegal [] 
