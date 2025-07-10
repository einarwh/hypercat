module GtTests

open System
open Xunit
open Legal
open Cat

let testGt (originalStack : Cat) (expectedResult : bool) = 
    match pushInput (NameInput "gt") originalStack with 
    | Reduction (BoolItem actualResult :: _) ->
        Assert.Equal(expectedResult, actualResult)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testGtLegal stack = 
    Assert.True(stack |> lookupPrecond "gt")

let testGtIllegal stack = 
    Assert.False(stack |> lookupPrecond "gt")

[<Fact>]
let ``Gt 1 and 1 is false`` () =
    testGt [ IntItem 1; IntItem 1 ] false

[<Fact>]
let ``Gt "a" and "a" is false`` () =
    testGt [ StringItem "a"; StringItem "a" ] false

[<Fact>]
let ``Gt "a" and "b" is true`` () =
    testGt [ StringItem "a"; StringItem "b" ] true

[<Fact>]
let ``Gt false and false is false`` () =
    testGt [ BoolItem false; BoolItem false ] false

[<Fact>]
let ``Gt 18 and 22 is true`` () =
    testGt [ IntItem 18; IntItem 22 ] true

[<Fact>]
let ``Gt 1 and "1" is type error`` () =
    let call = fun () -> testGt [ IntItem 1; StringItem "1" ] false
    Assert.Throws<TypeError>(call)

[<Fact>]
let ``Gt is legal given [ 1 2 ]`` () =
    testGtLegal [ IntItem 1; IntItem 2 ] 

[<Fact>]
let ``Gt is legal given [ 1 2 ... ]`` () =
    testGtLegal [ IntItem 1; IntItem 2; StringItem "???" ]

[<Fact>]
let ``Gt is legal given [ "a" "b" ]`` () =
    testGtLegal [ StringItem "a"; StringItem "b" ]

[<Fact>]
let ``Gt is legal given [ true false ]`` () =
    testGtLegal [ BoolItem true; BoolItem false ]

[<Fact>]
let ``Gt is illegal given stack with one item`` () =
    testGtIllegal [ IntItem 1 ] 

[<Fact>]
let ``Gt is illegal given empty stack`` () =
    testGtIllegal [] 
