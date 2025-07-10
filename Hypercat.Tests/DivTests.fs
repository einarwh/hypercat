module DivTests

open System
open Xunit
open Legal
open Cat

let testDiv n1 n2 expected = 
    match pushInput (IntInput n1) [] with 
    | Extension [ IntItem n1 ] ->
        match pushInput (IntInput n2) [ IntItem n1 ] with 
        | Extension [ IntItem n2; IntItem n1 ] ->
            match pushInput (NameInput "div") [ IntItem n2; IntItem n1 ] with 
            | Reduction [ IntItem actual ] -> Assert.Equal(expected, actual)
            | _ -> failwith "??"
        | _ -> failwith "??"
    | _ -> failwith "??"

let testDivLegal stack = 
    Assert.True(stack |> lookupPrecond "div")

let testDivIllegal stack = 
    Assert.False(stack |> lookupPrecond "div")

[<Fact>]
let ``Div 17 and 3`` () =
    testDiv 17 3 5

[<Fact>]
let ``Div 200 and 18`` () =
    testDiv 200 18 11

[<Fact>]
let ``Div -5 and 5`` () =
    testDiv -5 5 -1

[<Fact>]
let ``Div is legal given [ 1 2 ]`` () =
    testDivLegal [ IntItem 1; IntItem 2 ] 

[<Fact>]
let ``Div is legal given [ 1 2 ... ]`` () =
    testDivLegal [ IntItem 1; IntItem 2; StringItem "???" ]

[<Fact>]
let ``Div is illegal given [ 1 false ]`` () =
    testDivIllegal [ IntItem 1; BoolItem false ]

[<Fact>]
let ``Div is illegal given [ true 1 ]`` () =
    testDivIllegal [ BoolItem true ; IntItem 1 ]

[<Fact>]
let ``Div is illegal given [ 1 ]`` () =
    testDivIllegal [ IntItem 1 ] 
