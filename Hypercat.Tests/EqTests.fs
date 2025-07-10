module EqTests

open System
open Xunit
open Legal
open Cat

let testEq (originalStack : Cat) (expectedResult : bool) = 
    match pushInput (NameInput "eq") originalStack with 
    | Reduction (BoolItem actualResult :: _) ->
        Assert.Equal(expectedResult, actualResult)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testEqLegal stack = 
    Assert.True(stack |> lookupPrecond "eq")

let testEqIllegal stack = 
    Assert.False(stack |> lookupPrecond "eq")

[<Fact>]
let ``Eq 1 and 1 is true`` () =
    testEq [ IntItem 1; IntItem 1 ] true

[<Fact>]
let ``Eq "x" and "x" is true`` () =
    testEq [ StringItem "x"; StringItem "x" ] false

[<Fact>]
let ``Eq "x" and "y" is false`` () =
    testEq [ StringItem "x"; StringItem "y" ] false

[<Fact>]
let ``Eq false and false is true`` () =
    testEq [ BoolItem false; BoolItem false ] true

[<Fact>]
let ``Eq 22 and 18 is false`` () =
    testEq [ IntItem 22; IntItem 18 ] false

[<Fact>]
let ``Eq 1 and "1" is false`` () =
    testEq [ IntItem 1; StringItem "1" ] false

[<Fact>]
let ``Eq 1 and true is false`` () =
    testEq [ IntItem 1; BoolItem true ] false

[<Fact>]
let ``Eq is legal given [ 1 2 ]`` () =
    testEqLegal [ IntItem 1; IntItem 2 ] 

[<Fact>]
let ``Eq is legal given [ 1 2 ... ]`` () =
    testEqLegal [ IntItem 1; IntItem 2; StringItem "???" ]

[<Fact>]
let ``Eq is legal given [ 1 false ]`` () =
    testEqLegal [ IntItem 1; BoolItem false ]

[<Fact>]
let ``Eq is illegal given stack with one item`` () =
    testEqIllegal [ IntItem 1 ] 

[<Fact>]
let ``Eq is illegal given empty stack`` () =
    testEqIllegal [] 
