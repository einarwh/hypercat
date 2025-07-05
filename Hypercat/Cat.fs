module Cat

open System

// Maybe have two kinds of stack items, value and block?
// value in turn is num, bool or name (maybe string later)
// Maybe easier to drive externally.
// Just push simple values. Everything else is lookup?
// Whenever a new value is pushed, it is evaluated. Num, bool and string evaluate to themselves. 
// Names evaluate through lookup and evaluation of the corresponding procedure.
// Distinguish between things that execute/reduce and things that don't?
// On execution/reduction, 302 FOUND redirect to reduced stack.
// On non-execution/reduction, 200 OK to non-reduced stack.
// How to distinguish and where?

type Input = IntInput of int | BoolInput of bool | NameInput of string

type CatItem = IntItem of int | BoolItem of bool | ProcItem of Cat | UnfinishedProcItem of Cat 
and Cat = CatItem list

type PushResult = Reduction of Cat | Extension of Cat 

let push (e : Input) (stack : Cat) : PushResult =
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
        Extension stack

