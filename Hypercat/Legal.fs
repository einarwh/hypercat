module Legal

open Cat
open System

let unrollUnfinished stack = 
    let rec fn acc st = 
        match st with 
        | UnfinishedProcItem procStack :: rest -> 
            (fn acc procStack @ rest)
        | _ -> 
            acc @ st 
    fn [] stack

let simulate (stack : Cat) (unrolled : CatItem list) = 
    let rec apply (stack : Cat) (inputs : Input list) = 
        printfn "apply, inputs left: %A" inputs
        match inputs with 
        | [] -> 
            printfn "Simulated stack: %A\n" stack
            stack 
        | h :: rest -> 
            let result = pushInput h stack 
            match result with 
            | Reduction st -> 
                apply st rest 
            | Extension st -> 
                apply st rest
            | Execution (st, procInputs) -> 
                apply st (procInputs @ rest)
    let inputs = toInputs [] unrolled 
    apply stack inputs

let rec oneArg stack = 
    printfn "checking oneArg... %A" stack
    match stack with 
    | UnfinishedProcItem procStack :: rest -> 
        let unrolled = unrollUnfinished procStack
        printfn "unrolled %A" unrolled
        let simulated = simulate rest unrolled
        printfn "simulated %A" simulated
        false
    | a :: _ -> true 
    | _ -> false

let rec twoArgs stack = 
    match stack with 
    | UnfinishedProcItem procStack :: _ -> 
        false
    | a :: b :: _ -> true 
    | _ -> false

let rec oneInt stack = 
    printfn "checking oneInt... %A" stack
    match stack with 
    | IntItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: rest -> 
        let unrolled = unrollUnfinished procStack
        printfn "unrolled %A" unrolled
        let simulated = simulate rest unrolled
        printfn "simulated %A" simulated
        false
    | _ -> false

let rec twoInts stack = 
    printfn "checking twoInts... %A" stack
    match stack with 
    | IntItem _ :: IntItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> 
        printfn "UnfinishedProcItem: %A" procStack
        false
    | _ -> false

let rec oneBool stack = 
    match stack with 
    | BoolItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> false
    | _ -> false

let rec twoBools stack = 
    match stack with 
    | BoolItem _ :: BoolItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> false
    | _ -> false

let noPrecond stack = true

let rec execPrecond stack = 
    match stack with 
    | ProcItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> false
    | _ -> false

let rec ifPrecond stack = 
    match stack with 
    | ProcItem _ :: BoolItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> false
    | _ -> false

let rec ifelsePrecond stack = 
    match stack with 
    | ProcItem _ :: ProcItem _ :: BoolItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> 
        false
    | _ -> false

let endPrecond stack = 
    match stack with 
    | UnfinishedProcItem _ :: _ -> true 
    | _ -> false

let preconds : (string * (Cat -> bool)) list = 
    [ ("swap", twoArgs)
      ("dup", oneArg)
      ("pop", oneArg)
      ("add", twoInts)
      ("sub", twoInts)
      ("mul", twoInts)
      ("true", noPrecond)
      ("false", noPrecond)
      ("zero", noPrecond)
      ("succ", oneInt)
      ("pred", oneInt)
      ("neg", oneInt)
      ("eq", twoArgs)
      ("ne", twoArgs)
      ("gt", twoArgs)
      ("lt", twoArgs)
      ("not", oneBool)
      ("and", twoBools)
      ("or", twoBools)
      ("exec", execPrecond)
      ("if", ifPrecond)
      ("ifelse", ifelsePrecond)
      ("begin", noPrecond)
      ("end", endPrecond)
    ]

let legalOps (stack : Cat) : string list = 
    let st = 
        match stack with 
        | UnfinishedProcItem procStack :: rest -> 
            let unrolled = unrollUnfinished procStack
            printfn "unrolled %A" unrolled
            let simulated = simulate rest unrolled
            printfn "simulated %A" simulated
            simulated
        | _ -> 
            stack
    let check (name, precond) = 
        let stk = if name = "end" then stack else st
        if precond stk then Some name else None 
    preconds |> List.choose check
