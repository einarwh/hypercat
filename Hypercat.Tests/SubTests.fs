module SubTests

open System
open Xunit
open Legal
open Cat

let testSub n1 n2 expected = 
    match pushInput (IntInput n1) [] with 
    | Extension [ IntItem n1 ] ->
        match pushInput (IntInput n2) [ IntItem n1 ] with 
        | Extension [ IntItem n2; IntItem n1 ] ->
            match pushInput (NameInput "sub") [ IntItem n2; IntItem n1 ] with 
            | Reduction [ IntItem actual ] -> Assert.Equal(expected, actual)
            | _ -> failwith "??"
        | _ -> failwith "??"
    | _ -> failwith "??"

let testSubLegal stack = 
    Assert.True(stack |> lookupPrecond "sub")

let testSubIllegal stack = 
    Assert.False(stack |> lookupPrecond "sub")

[<Fact>]
let ``Sub 7 and 5`` () =
    testSub 7 5 2

[<Fact>]
let ``Sub 22 and 18`` () =
    testSub 22 18 4

[<Fact>]
let ``Sub -5 and 5`` () =
    testSub -5 5 -10

[<Fact>]
let ``Sub 5 and -5`` () =
    testSub 5 -5 10

[<Fact>]
let ``Sub is legal given [ 1 2 ]`` () =
    testSubLegal [ IntItem 1; IntItem 2 ] 

[<Fact>]
let ``Sub is legal given [ 1 2 ... ]`` () =
    testSubLegal [ IntItem 1; IntItem 2; StringItem "???" ]

[<Fact>]
let ``Sub is illegal given [ 1 false ]`` () =
    testSubIllegal [ IntItem 1; BoolItem false ]

[<Fact>]
let ``Sub is illegal given [ true 1 ]`` () =
    testSubIllegal [ BoolItem true ; IntItem 1 ]

[<Fact>]
let ``Sub is illegal given [ 1 ]`` () =
    testSubIllegal [ IntItem 1 ] 
