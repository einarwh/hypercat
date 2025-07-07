module Cat

open System

type Input = IntInput of int | BoolInput of bool | NameInput of string

type CatItem = IntItem of int | BoolItem of bool | ProcItem of Cat | NameItem of string | UnfinishedProcItem of Cat 
and Cat = CatItem list

type PushResult = Reduction of Cat | Extension of Cat | Execution of Cat * Input list 

// type Op = 
//     { op: Cat -> Cat 
//       precond : Cat -> bool }

let nop (stack: Cat) : Cat = stack

let swap (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> b :: a :: rest 
    | _ -> failwith "stack underflow in swap"

let dup (stack : Cat) : Cat = 
    match stack with 
    | a :: rest -> a :: a :: rest 
    | _ -> failwith "stack underflow in dup"

let pop (stack : Cat) : Cat = 
    match stack with 
    | a :: rest -> rest 
    | _ -> failwith "stack underflow in pop"

let push (e : CatItem) (stack : Cat) : Cat =
    e :: stack

let add (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> IntItem (n2 + n1) :: rest
        | _ -> failwith "type error in add"
    | _ -> failwith "stack underflow in add"

let sub (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> IntItem (n2 - n1) :: rest
        | _ -> failwith "type error in sub"
    | _ -> failwith "stack underflow in sub"

let mul (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> IntItem (n2 * n1) :: rest
        | _ -> failwith "type error in mul"
    | _ -> failwith "stack underflow in mul"

let neg (stack : Cat) : Cat = 
    match stack with 
    | a :: rest -> 
        match a with 
        | IntItem n -> IntItem (- n) :: rest
        | _ -> failwith "type error in neg"
    | _ -> failwith "stack underflow in neg"

let eq (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> BoolItem (n2 = n1) :: rest
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 = b1) :: rest
        | _ -> failwith "type error in eq"
    | _ -> failwith "stack underflow in eq"

let ne (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> BoolItem (n2 <> n1) :: rest
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 <> b1) :: rest
        | _ -> failwith "type error in ne"
    | _ -> failwith "stack underflow in ne"

let gt (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> BoolItem (n2 > n1) :: rest
        | _ -> failwith "type error in gt"
    | _ -> failwith "stack underflow in gt"

let lt (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> BoolItem (n2 < n1) :: rest
        | _ -> failwith "type error in lt"
    | _ -> failwith "stack underflow in lt"

let notOp (stack : Cat) : Cat = 
    match stack with 
    | a :: rest -> 
        match a with 
        | BoolItem b -> BoolItem (not b) :: rest
        | _ -> failwith "type error in not"
    | _ -> failwith "stack underflow in not"

let andOp (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 && b1) :: rest
        | _ -> failwith "type error in and"
    | _ -> failwith "stack underflow in and"

let orOp (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 || b1) :: rest
        | _ -> failwith "type error in or"
    | _ -> failwith "stack underflow in or"

let trueOp (stack : Cat) : Cat = 
    stack |> push (BoolItem true) 

let falseOp (stack : Cat) : Cat = 
    stack |> push (BoolItem false) 

let zero (stack : Cat) : Cat = 
    stack |> push (IntItem 0) 

let succ (stack : Cat) : Cat = 
    stack |> push (IntItem 1) |> add

let pred (stack : Cat) : Cat = 
    stack |> push (IntItem -1) |> add

let rec oneArg stack = 
    match stack with 
    | UnfinishedProcItem procStack :: _ -> oneArg procStack
    | a :: _ -> true 
    | _ -> false

let rec twoArgs stack = 
    match stack with 
    | UnfinishedProcItem procStack :: _ -> twoArgs procStack
    | a :: b :: _ -> true 
    | _ -> false

let rec oneInt stack = 
    match stack with 
    | IntItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> oneInt procStack
    | _ -> false

let rec twoInts stack = 
    printfn "checking twoInts... %A" stack
    match stack with 
    | IntItem _ :: IntItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> 
        printfn "UnfinishedProcItem: %A" procStack
        twoInts procStack
    | _ -> false

let rec oneBool stack = 
    match stack with 
    | BoolItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> oneBool procStack
    | _ -> false

let rec twoBools stack = 
    match stack with 
    | BoolItem _ :: BoolItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> twoBools procStack
    | _ -> false

let noPrecond stack = true

let rec execPrecond stack = 
    match stack with 
    | ProcItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> execPrecond procStack
    | _ -> false

let rec ifPrecond stack = 
    match stack with 
    | ProcItem _ :: BoolItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> ifPrecond procStack
    | _ -> false

let rec ifelsePrecond stack = 
    match stack with 
    | ProcItem _ :: ProcItem _ :: BoolItem _ :: _ -> true 
    | UnfinishedProcItem procStack :: _ -> ifelsePrecond procStack
    | _ -> false

let endPrecond stack = 
    match stack with 
    | UnfinishedProcItem _ :: _ -> true 
    | _ -> false

let ops : (string * (Cat -> Cat)) list = 
    [ ("swap", swap)
      ("dup", dup)
      ("pop", pop)
      ("add", add)
      ("sub", sub)
      ("mul", mul)
      ("true", trueOp)
      ("false", falseOp)
      ("zero", zero)
      ("succ", succ)
      ("pred", pred)
      ("neg", neg)
      ("eq", eq)
      ("ne", ne)
      ("gt", gt)
      ("lt", lt)
      ("not", notOp)
      ("and", andOp)
      ("or", orOp)
    //   ("exec", nop)
    //   ("if", nop)
    //   ("ifelse", nop)
    //   ("begin", { op = nop; precond = noPrecond })
    //   ("end", { op = nop; precond = endPrecond })
    ]

let lookupProc name = 
    match ops |> List.tryFind (fun (n, op) -> n = name) with
    | Some (n, op) -> op
    | None -> failwith <| sprintf "Unknown operation %s" name

let isInsideUnfinishedProc (stack : Cat) = 
    match stack with 
    | UnfinishedProcItem _ :: _ -> true 
    | _ -> false

let rec extendDown (it : CatItem) (stack : Cat) : Cat = 
    match stack with 
    | (UnfinishedProcItem unfinished) :: rest -> 
        (UnfinishedProcItem (extendDown it unfinished)) :: rest
    | _ -> it :: stack

let extend (it : CatItem) (stack : Cat) : PushResult = 
    Extension (extendDown it stack)

let rec toInputs (inputs : Input list) (items : CatItem list) : Input list = 
    match items with 
    | [] -> inputs // Rev?
    | it :: rest -> 
        match it with 
        | IntItem n -> toInputs (IntInput n :: inputs) rest
        | BoolItem b -> toInputs (BoolInput b :: inputs) rest
        | NameItem name -> toInputs (NameInput name :: inputs) rest 
        | ProcItem procItems -> 
            let procInputs = [ NameInput "begin" ] @ toInputs [] procItems @ [ NameInput "end" ]
            toInputs (procInputs @ inputs) rest 
        | UnfinishedProcItem procItems -> 
            let procInputs = [ NameInput "begin" ] @ toInputs [] procItems
            toInputs (procInputs @ inputs) rest 

let rec endProc (item : CatItem) : CatItem = 
    match item with 
    | UnfinishedProcItem prc -> 
        match prc with 
        | (UnfinishedProcItem innerPrc) :: rest -> 
            UnfinishedProcItem ((endProc (UnfinishedProcItem innerPrc)) :: rest)
        | _ -> ProcItem prc 
    | _ -> failwith "must be unfinished proc"

let pushInput (e : Input) (stack : Cat) : PushResult =
    match e with 
    | IntInput n -> extend (IntItem n) stack 
    | BoolInput b -> extend (BoolItem b) stack 
    | NameInput name when name = "begin" -> 
        printfn "begin!"
        extend (UnfinishedProcItem []) stack
    | NameInput name when name = "end" -> 
        printfn "end!"
        match stack with 
        | UnfinishedProcItem prc :: rest -> 
            Extension (endProc (UnfinishedProcItem prc) :: rest)
        | _ -> failwith "unexpected end!"
    | NameInput name when name = "exec" -> 
        printfn "exec!"
        if isInsideUnfinishedProc stack then 
            extend (NameItem name) stack 
        else 
            match stack with 
            | ProcItem prc :: rest -> 
                Execution (rest, toInputs [] prc)
            | _ -> failwith "type error in exec"
    | NameInput name when name = "if" -> 
        printfn "if!"
        if isInsideUnfinishedProc stack then 
            extend (NameItem name) stack 
        else 
            match stack with 
            | a :: b :: rest -> 
                match (a, b) with 
                | ProcItem prc, BoolItem b -> 
                    if b then Execution (rest, toInputs [] prc)
                    else Execution (rest, [])
                | _ -> failwith "type error in if" 
            | _ -> failwith "stack underflow in if"
    | NameInput name when name = "ifelse" -> 
        printfn "ifelse!"
        if isInsideUnfinishedProc stack then 
            extend (NameItem name) stack 
        else 
            match stack with 
            | a :: b :: c :: rest -> 
                match (a, b, c) with 
                | ProcItem elseProc, ProcItem ifProc, BoolItem b -> 
                    if b then Execution (rest, toInputs [] ifProc)
                    else Execution (rest, toInputs [] elseProc)
                | _ -> failwith "type error in ifelse" 
            | _ -> failwith "stack underflow in ifelse"
    | NameInput name -> 
        printfn "push name %s" name
        if isInsideUnfinishedProc stack then 
            extend (NameItem name) stack 
        else 
            // Lookup
            let op = lookupProc name 
            Reduction (op stack)
