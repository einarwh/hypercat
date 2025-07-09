module AddTests

open System
open Xunit
open Cat

let testAdd n1 n2 result = 
    match pushInput (IntInput n1) [] with 
    | Extension [ IntItem n1 ] ->
        match pushInput (IntInput n2) [ IntItem n1 ] with 
        | Extension [ IntItem n2; IntItem n1 ] ->
            match pushInput (NameInput "add") [ IntItem n2; IntItem n1 ] with 
            | Reduction [ IntItem result ] -> Assert.True(true)
            | _ -> failwith "??"
        | _ -> failwith "??"
    | _ -> failwith "??"

[<Fact>]
let ``Add 2 and 1`` () =
    testAdd 2 1 3

[<Fact>]
let ``Add 22 and 18`` () =
    testAdd 22 18 40

[<Fact>]
let ``Add -5 and 5`` () =
    testAdd -5 5 0

