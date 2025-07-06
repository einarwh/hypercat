module Cat

open System

type Input = IntInput of int | BoolInput of bool | NameInput of string

type CatItem = IntItem of int | BoolItem of bool | ProcItem of Cat | UnfinishedProcItem of Cat 
and Cat = CatItem list

type PushResult = Reduction of Cat | Extension of Cat 

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

let pushInput (e : Input) (stack : Cat) : PushResult =
    match e with 
    | IntInput n -> 
        Extension (IntItem n :: stack)
    | BoolInput b -> 
        Extension (BoolItem b :: stack)
    | NameInput name when name = "begin" -> 
        printfn "begin!"
        Extension (UnfinishedProcItem [] :: stack)
    | NameInput name when name = "end" -> 
        printfn "end!"
        match stack with 
        | UnfinishedProcItem prc :: rest -> 
            Extension (ProcItem prc :: rest)
        | _ -> failwith "unexpected end!"
    | NameInput name -> 
        // Lookup
        let op = lookupProc name 
        Reduction (op stack)

