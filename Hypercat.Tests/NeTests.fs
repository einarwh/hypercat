module NeTests

open System
open Xunit
open Legal
open Cat

let testNe (originalStack : Cat) (expectedResult : bool) = 
    printfn "originalStack %A -> %b" originalStack expectedResult
    match pushInput (NameInput "ne") originalStack with 
    | Reduction (BoolItem actualResult :: _) ->
        Assert.Equal(expectedResult, actualResult)
    | result -> 
        failwith <| sprintf "unexpected result %A" result

let testNeLegal stack = 
    Assert.True(stack |> lookupPrecond "ne")

let testNeIllegal stack = 
    Assert.False(stack |> lookupPrecond "ne")

[<Fact>]
let ``Ne 1 and 1 is false`` () =
    testNe [ IntItem 1; IntItem 1 ] false

[<Fact>]
let ``Ne "x" and "x" is false`` () =
    testNe [ StringItem "x"; StringItem "x" ] false

[<Fact>]
let ``Ne "x" and "y" is True`` () =
    testNe [ StringItem "x"; StringItem "y" ] true

[<Fact>]
let ``Ne false and false is false`` () =
    testNe [ BoolItem false; BoolItem false ] false

[<Fact>]
let ``Ne 22 and 18 is true`` () =
    testNe [ IntItem 22; IntItem 18 ] true

[<Fact>]
let ``Ne 1 and "1" is true`` () =
    testNe [ IntItem 1; StringItem "1" ] true

[<Fact>]
let ``Ne 1 and true is true`` () =
    testNe [ IntItem 1; BoolItem true ] true

[<Fact>]
let ``Ne is legal given [ 1 2 ]`` () =
    testNeLegal [ IntItem 1; IntItem 2 ] 

[<Fact>]
let ``Ne is legal given [ 1 2 ... ]`` () =
    testNeLegal [ IntItem 1; IntItem 2; StringItem "???" ]

[<Fact>]
let ``Ne is legal given [ 1 false ]`` () =
    testNeLegal [ IntItem 1; BoolItem false ]

[<Fact>]
let ``Ne is illegal given stack with one item`` () =
    testNeIllegal [ IntItem 1 ] 

[<Fact>]
let ``Ne is illegal given empty stack`` () =
    testNeIllegal [] 
