module PushIntTests

open System
open Xunit
open Cat

[<Fact>]
let ``Push 2 onto empty stack`` () =
    let input = IntInput 2
    let stack = []
    match pushInput input stack with 
    | Extension [ IntItem 2 ] ->  
        Assert.True(true)
    | _ -> failwith "??"

[<Fact>]
let ``Push 2 onto stack [ IntItem 1 ]`` () =
    let input = IntInput 2
    let stack = [ IntItem 1 ]
    match pushInput input stack with 
    | Extension [ IntItem 2; IntItem 1 ] ->  
        Assert.True(true)
    | _ -> failwith "??"

