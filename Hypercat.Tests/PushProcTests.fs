module PushProcTests

open System
open Xunit
open Cat

[<Fact>]
let ``Push proc name onto empty stack`` () =
    let input = NameInput "proc"
    let stack = []
    match pushInput input stack with 
    | Extension [ ProcMarker ] ->  
        Assert.True(true)
    | result -> failwith <| sprintf "%A ??" result

[<Fact>]
let ``Push proc name onto stack [ ProcMarker ]`` () =
    let input = NameInput "proc"
    let stack = [ ProcMarker ]
    match pushInput input stack with 
    | Extension [ ProcMarker; ProcMarker ] ->  
        Assert.True(true)
    | result -> failwith <| sprintf "%A ??" result

// [<Fact>]
// let ``Push end name onto stack [ ProcMarker; ProcMarker ]`` () =
//     let input = NameInput "end"
//     let stack = [ ProcMarker; ProcMarker ]
//     match pushInput input stack with 
//     | Extension [ NameItem "end"; ProcMarker; ProcMarker ] ->  
//         Assert.True(true)
//     | result -> failwith <| sprintf "%A ??" result

