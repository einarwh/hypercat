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
        match inputs with 
        | [] -> 
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
    match stack with 
    | a :: _ -> true 
    | _ -> false

let rec twoArgs stack = 
    match stack with 
    | a :: b :: _ -> true 
    | _ -> false

let rec oneInt stack = 
    match stack with 
    | IntItem _ :: _ -> true 
    | _ -> false

let rec twoInts stack = 
    match stack with 
    | IntItem _ :: IntItem _ :: _ -> true 
    | _ -> false

let rec oneBool stack = 
    match stack with 
    | BoolItem _ :: _ -> true 
    | _ -> false

let rec twoBools stack = 
    match stack with 
    | BoolItem _ :: BoolItem _ :: _ -> true 
    | _ -> false

let rec twoStrings stack = 
    match stack with 
    | StringItem _ :: StringItem _ :: _ -> true 
    | _ -> false

let noPrecond stack = true

let rec execPrecond stack = 
    match stack with 
    | ProcItem _ :: _ -> true 
    | _ -> false

let rec oneBlock stack = 
    match stack with 
    | ProcItem _ :: _ -> true 
    | _ -> false

let rec nonEmptyBlock stack = 
    match stack with 
    | ProcItem (_ :: _) :: _ -> true 
    | _ -> false

let rec twoBlocks stack = 
    match stack with 
    | ProcItem _ :: ProcItem _ :: _ -> true 
    | _ -> false

let rec consPrecond stack = 
    match stack with 
    | ProcItem _ :: _ :: _ -> true 
    | _ -> false

let rec ifPrecond stack = 
    match stack with 
    | ProcItem _ :: BoolItem _ :: _ -> true 
    | _ -> false

let rec ifelsePrecond stack = 
    match stack with 
    | ProcItem _ :: ProcItem _ :: BoolItem _ :: _ -> true 
    | _ -> false

let endPrecond stack = 
    match stack with 
    | UnfinishedProcItem _ :: _ -> true 
    | _ -> false

let dropPrecond stack = 
    printfn "dropPrecond %A" stack
    match stack with 
    | UnfinishedProcItem (_ :: _) :: _ -> true 
    | _ -> false

let preconds : (string * (Cat -> bool)) list = 
    [ ("clear", noPrecond)
      ("swap", twoArgs)
      ("dup", oneArg)
      ("pop", oneArg)
      ("drop", dropPrecond)
      ("add", twoInts)
      ("sub", twoInts)
      ("mul", twoInts)
      ("div", twoInts)
      ("true", noPrecond)
      ("false", noPrecond)
      ("zero", noPrecond)
      ("succ", oneInt)
      ("pred", oneInt)
      ("neg", oneInt)
      ("eq", twoArgs)
      ("ne", twoArgs)
      ("gt", twoInts)
      ("lt", twoInts)
      ("not", oneBool)
      ("and", twoBools)
      ("or", twoBools)
      ("xor", twoBools)
      ("concat", twoBlocks)
      ("head", nonEmptyBlock)
      ("tail", nonEmptyBlock)
      ("cons", consPrecond)
      ("rev", oneBlock)
    //   ("map", twoBlocks)
      ("split", twoStrings)
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
            simulate rest unrolled
        | _ -> 
            stack
    let check (name, precond) = 
        let stk = 
            if name = "end" then stack 
            else if name = "drop" then stack
            else st
        if precond stk then Some name else None 
    preconds |> List.choose check
