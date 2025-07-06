module Cat

open System

type Input = IntInput of int | BoolInput of bool | NameInput of string

type CatItem = IntItem of int | BoolItem of bool | ProcItem of Cat | NameItem of string | UnfinishedProcItem of Cat 
and Cat = CatItem list

type PushResult = Reduction of Cat | Extension of Cat | Execution of Cat * Input list 

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

let succ (stack : Cat) : Cat = 
    stack |> push (IntItem 1) |> add

let pred (stack : Cat) : Cat = 
    stack |> push (IntItem -1) |> add

let procs : (string * (Cat -> Cat)) list = 
    [ ("swap", swap)
      ("dup", dup)
      ("pop", pop)
      ("add", add)
      ("succ", succ)
      ("pred", pred) ]

let lookupProc name = 
    match procs |> List.tryFind (fun (n, op) -> n = name) with
    | Some (n, op) -> op 
    | None -> failwith <| sprintf "Unknown operation %s" name

let rec isInsideUnfinishedProc (stack : Cat) = 
    match stack with 
    | [] -> false 
    | UnfinishedProcItem _ :: _ -> true 
    | h :: rest -> isInsideUnfinishedProc rest

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
            | _ -> failwith "unexpected end!"
    | NameInput name -> 
        if isInsideUnfinishedProc stack then 
            extend (NameItem name) stack 
        else 
            // Lookup
            let op = lookupProc name 
            Reduction (op stack)

