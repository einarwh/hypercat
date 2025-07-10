module SwapTests

open System
open Xunit
open Legal
open Cat

let testAdd n1 n2 sum = 
    match pushInput (IntInput n1) [] with 
    | Extension [ IntItem n1 ] ->
        match pushInput (IntInput n2) [ IntItem n1 ] with 
        | Extension [ IntItem n2; IntItem n1 ] ->
            match pushInput (NameInput "add") [ IntItem n2; IntItem n1 ] with 
            | Reduction [ IntItem sum ] -> Assert.True(true)
            | _ -> failwith "??"
        | _ -> failwith "??"
    | _ -> failwith "??"

let testAddLegal stack = 
    Assert.True(stack |> lookupPrecond "add")

let testAddIllegal stack = 
    Assert.False(stack |> lookupPrecond "add")

[<Fact>]
let ``Add 2 and 1`` () =
    testAdd 2 1 3

[<Fact>]
let ``Add 22 and 18`` () =
    testAdd 22 18 40

[<Fact>]
let ``Add -5 and 5`` () =
    testAdd -5 5 0

[<Fact>]
let ``Add is legal given [ 1 2 ]`` () =
    testAddLegal [ IntItem 1; IntItem 2 ] 

[<Fact>]
let ``Add is legal given [ 1 2 ... ]`` () =
    testAddLegal [ IntItem 1; IntItem 2; StringItem "???" ]

[<Fact>]
let ``Add is illegal given [ 1 false ]`` () =
    testAddIllegal [ IntItem 1; BoolItem false ]

[<Fact>]
let ``Add is illegal given [ true 1 ]`` () =
    testAddIllegal [ BoolItem true ; IntItem 1 ]

[<Fact>]
let ``Add is illegal given [ 1 ]`` () =
    testAddIllegal [ IntItem 1 ] 
