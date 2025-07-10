module MulTests

open System
open Xunit
open Legal
open Cat

let testMul n1 n2 expected = 
    match pushInput (IntInput n1) [] with 
    | Extension [ IntItem n1 ] ->
        match pushInput (IntInput n2) [ IntItem n1 ] with 
        | Extension [ IntItem n2; IntItem n1 ] ->
            match pushInput (NameInput "mul") [ IntItem n2; IntItem n1 ] with 
            | Reduction [ IntItem actual ] -> Assert.Equal(expected, actual)
            | _ -> failwith "??"
        | _ -> failwith "??"
    | _ -> failwith "??"

let testMulLegal stack = 
    Assert.True(stack |> lookupPrecond "mul")

let testMulIllegal stack = 
    Assert.False(stack |> lookupPrecond "mul")

[<Fact>]
let ``Mul 2 and 1`` () =
    testMul 7 3 21

[<Fact>]
let ``Mul 22 and 18`` () =
    testMul 12 12 144

[<Fact>]
let ``Mul -5 and 5`` () =
    testMul -5 5 -25

[<Fact>]
let ``Mul is legal given [ 1 2 ]`` () =
    testMulLegal [ IntItem 1; IntItem 2 ] 

[<Fact>]
let ``Mul is legal given [ 1 2 ... ]`` () =
    testMulLegal [ IntItem 1; IntItem 2; StringItem "???" ]

[<Fact>]
let ``Mul is illegal given [ 1 false ]`` () =
    testMulIllegal [ IntItem 1; BoolItem false ]

[<Fact>]
let ``Mul is illegal given [ true 1 ]`` () =
    testMulIllegal [ BoolItem true ; IntItem 1 ]

[<Fact>]
let ``Mul is illegal given [ 1 ]`` () =
    testMulIllegal [ IntItem 1 ] 
