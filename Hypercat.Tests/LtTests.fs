module LtTests

open System
open Xunit
open Legal
open Cat

let testLt (originalStack : Cat) (expectedResult : bool) = 
    match pushInput (NameInput "lt") originalStack with 
    | Reduction (BoolItem actualResult :: _) ->
        Assert.Equal(expectedResult, actualResult)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testLtLegal stack = 
    Assert.True(stack |> lookupPrecond "lt")

let testLtIllegal stack = 
    Assert.False(stack |> lookupPrecond "lt")

[<Fact>]
let ``Lt 1 and 1 is false`` () =
    testLt [ IntItem 1; IntItem 1 ] false

[<Fact>]
let ``Lt "a" and "a" is false`` () =
    testLt [ StringItem "a"; StringItem "a" ] false

[<Fact>]
let ``Lt "b" and "a" is true`` () =
    testLt [ StringItem "b"; StringItem "a" ] true

[<Fact>]
let ``Lt false and false is false`` () =
    testLt [ BoolItem false; BoolItem false ] false

[<Fact>]
let ``Lt 22 and 18 is true`` () =
    testLt [ IntItem 22; IntItem 18 ] true

[<Fact>]
let ``Lt 1 and "1" is type error`` () =
    let call = fun () -> testLt [ IntItem 1; StringItem "1" ] false
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Lt is legal given [ 1 2 ]`` () =
    testLtLegal [ IntItem 1; IntItem 2 ] 

[<Fact>]
let ``Lt is legal given [ 1 2 ... ]`` () =
    testLtLegal [ IntItem 1; IntItem 2; StringItem "???" ]

[<Fact>]
let ``Lt is legal given [ "a" "b" ]`` () =
    testLtLegal [ StringItem "a"; StringItem "b" ]

[<Fact>]
let ``Lt is legal given [ true false ]`` () =
    testLtLegal [ BoolItem true; BoolItem false ]

[<Fact>]
let ``Lt is illegal given stack with one item`` () =
    testLtIllegal [ IntItem 1 ] 

[<Fact>]
let ``Lt is illegal given empty stack`` () =
    testLtIllegal [] 
