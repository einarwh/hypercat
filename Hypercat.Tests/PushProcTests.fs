module PushProcTests

open System
open Xunit
open Cat

[<Fact>]
let ``Push proc name onto empty stack`` () =
    let input = NameInput "proc"
    let stack = []
    let expectedStack = [ ProcMarker ]
    match pushInput input stack with 
    | Extension actualStack ->  
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> failwith <| sprintf "%A ??" result

[<Fact>]
let ``Push proc name onto stack [ ProcMarker ]`` () =
    let input = NameInput "proc"
    let stack = [ ProcMarker ]
    let expectedStack = [ ProcMarker; ProcMarker ]
    match pushInput input stack with 
    | Extension actualStack ->  
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> failwith <| sprintf "%A ??" result

[<Fact(Skip = "Doesn't work so.")>]
let ``Push end name onto stack [ ProcMarker; ProcMarker ]`` () =
    let input = NameInput "end"
    let stack = [ ProcMarker; ProcMarker ]
    let expectedStack = [ NameItem "end"; ProcMarker; ProcMarker ]
    match pushInput input stack with 
    | Extension actualStack ->  
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> failwith <| sprintf "%A ??" result

[<Fact(Skip = "Doesn't work so.")>]
let ``Push end name onto stack [ NameInput "end"; ProcMarker; ProcMarker ]`` () =
    let input = NameInput "end"
    let stack = [ NameItem "end"; ProcMarker; ProcMarker ]
    let expectedStack = [ ProcItem [ ProcItem [] ] ]
    match pushInput input stack with 
    | Extension actualStack ->  
        Assert.Equal<Cat>(expectedStack, actualStack)
    | result -> failwith <| sprintf "%A ??" result
