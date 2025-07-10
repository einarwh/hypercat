module ListTests

open System
open Xunit
open Legal
open Cat

[<Fact>]
let ``Create an empty list`` () =
    match pushInput (NameInput "list") [] with 
    | Extension st1 ->
        match pushInput (NameInput "end") st1 with 
        | Extension st2 -> 
            Assert.Equal<Cat>([ ListItem [] ], st2)
        | result -> failwith <| sprintf "unexpected result %A after pushing end" result
    | result -> 
        failwith <| sprintf "unexpected result %A after pushing list" result

[<Fact>]
let ``Create a list with one element`` () =
    match pushInput (NameInput "list") [] with 
    | Extension st1 ->
        match pushInput (IntInput 0) st1 with 
        | Extension st2 -> 
            match pushInput (NameInput "end") st2 with 
            | Extension st3 -> 
                Assert.Equal<Cat>([ ListItem [ IntItem 0 ] ], st3)
            | result -> 
                failwith <| sprintf "unexpected result %A after pushing end" result
        | result -> 
            failwith <| sprintf "unexpected result %A after pushing 0" result
    | result -> 
        failwith <| sprintf "unexpected result %A after pushing list" result

[<Fact>]
let ``Create list with swap trick`` () =
    let stack = [ StringItem "hello" ]
    match pushInput (NameInput "list") stack with 
    | Extension st1 ->
        match pushInput (NameInput "swap") st1 with 
        | Reduction st2 -> 
            match pushInput (NameInput "end") st2 with 
            | Extension st3 -> 
                Assert.Equal<Cat>([ ListItem [ StringItem "hello" ] ], st3)
            | result -> 
                failwith <| sprintf "unexpected result %A after pushing end" result
        | result -> 
            failwith <| sprintf "unexpected result %A after pushing swap" result
    | result -> 
        failwith <| sprintf "unexpected result %A after pushing list" result
