module Cat

open System

exception StackUnderflowError of string

exception TypeError of string 

type Input = IntInput of int | BoolInput of bool | NameInput of string

type CatItem = IntItem of int | BoolItem of bool | ProcItem of Cat | NameItem of string | UnfinishedProcItem of Cat 
and Cat = CatItem list

type PushResult = Reduction of Cat | Extension of Cat | Execution of Cat * Input list 

let nop (stack: Cat) : Cat = stack

let swap (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> b :: a :: rest 
    | _ -> raise (StackUnderflowError "swap")

let dup (stack : Cat) : Cat = 
    match stack with 
    | a :: rest -> a :: a :: rest 
    | _ -> raise (StackUnderflowError "dup")

let pop (stack : Cat) : Cat = 
    match stack with 
    | a :: rest -> rest 
    | _ -> raise (StackUnderflowError "pop")

let push (e : CatItem) (stack : Cat) : Cat =
    e :: stack

let add (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> IntItem (n2 + n1) :: rest
        | _ -> raise (TypeError "add")
    | _ -> raise (StackUnderflowError "add")

let sub (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> IntItem (n2 - n1) :: rest
        | _ -> raise (TypeError "sub")
    | _ -> raise (StackUnderflowError "sub")

let mul (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> IntItem (n2 * n1) :: rest
        | _ -> raise (TypeError "mul")
    | _ -> raise (StackUnderflowError "mul")

let div (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> IntItem (n2 / n1) :: rest
        | _ -> raise (TypeError "div")
    | _ -> raise (StackUnderflowError "div")

let neg (stack : Cat) : Cat = 
    match stack with 
    | a :: rest -> 
        match a with 
        | IntItem n -> IntItem (- n) :: rest
        | _ -> raise (TypeError "neg")
    | _ -> raise (StackUnderflowError "neg")

let eq (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> BoolItem (n2 = n1) :: rest
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 = b1) :: rest
        | _ -> BoolItem false :: rest
    | _ -> raise (StackUnderflowError "eq")

let ne (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> BoolItem (n2 <> n1) :: rest
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 <> b1) :: rest
        | _ -> BoolItem true :: rest
    | _ -> raise (StackUnderflowError "ne")

let gt (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> BoolItem (n2 > n1) :: rest
        | _ -> raise (TypeError "gt")
    | _ -> raise (StackUnderflowError "gt")

let lt (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> BoolItem (n2 < n1) :: rest
        | _ -> raise (TypeError "lt")
    | _ -> raise (StackUnderflowError "lt")

let notOp (stack : Cat) : Cat = 
    match stack with 
    | a :: rest -> 
        match a with 
        | BoolItem b -> BoolItem (not b) :: rest
        | _ -> raise (TypeError "not")
    | _ -> raise (StackUnderflowError "not")

let andOp (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 && b1) :: rest
        | _ -> raise (TypeError "and")
    | _ -> raise (StackUnderflowError "and")

let orOp (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 || b1) :: rest
        | _ -> raise (TypeError "or")
    | _ -> raise (StackUnderflowError "or")

let xorOp (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 <> b1) :: rest
        | _ -> raise (TypeError "xor")
    | _ -> raise (StackUnderflowError "xor")

let concat (stack : Cat) : Cat = 
   match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | ProcItem p1, ProcItem p2 -> ProcItem (p1 @ p2) :: rest
        | _ -> raise (TypeError "concat")
    | _ -> raise (StackUnderflowError "concat")

let head (stack : Cat) : Cat = 
   match stack with 
    | a :: rest -> 
        match a with 
        | ProcItem block ->
            match block with 
            | h :: _ -> 
                h :: rest 
            | _ -> raise (StackUnderflowError "head")
        | _ -> raise (TypeError "head")
    | _ -> raise (StackUnderflowError "head")

let tail (stack : Cat) : Cat = 
   match stack with 
    | a :: rest -> 
        match a with 
        | ProcItem block ->
            match block with 
            | _ :: t -> 
                (ProcItem t) :: rest 
            | _ -> raise (StackUnderflowError "tail")
        | _ -> raise (TypeError "tail")
    | _ -> raise (StackUnderflowError "tail")

let trueOp (stack : Cat) : Cat = 
    stack |> push (BoolItem true) 

let falseOp (stack : Cat) : Cat = 
    stack |> push (BoolItem false) 

let zero (stack : Cat) : Cat = 
    stack |> push (IntItem 0) 

let succ (stack : Cat) : Cat = 
    match stack with 
    | a :: rest -> 
        match a with 
        | IntItem n -> IntItem (n + 1) :: rest
        | _ -> raise (TypeError "succ")
    | _ -> raise (StackUnderflowError "succ")

let pred (stack : Cat) : Cat = 
    match stack with 
    | a :: rest -> 
        match a with 
        | IntItem n -> IntItem (n - 1) :: rest
        | _ -> raise (TypeError "pred")
    | _ -> raise (StackUnderflowError "pred")

let ops : (string * (Cat -> Cat)) list = 
    [ ("swap", swap)
      ("dup", dup)
      ("pop", pop)
      ("add", add)
      ("sub", sub)
      ("mul", mul)
      ("div", div)
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
      ("xor", xorOp)
      ("concat", concat)
      ("head", head)
      ("tail", tail)
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
        extend (UnfinishedProcItem []) stack
    | NameInput name when name = "end" -> 
        match stack with 
        | UnfinishedProcItem prc :: rest -> 
            Extension (endProc (UnfinishedProcItem prc) :: rest)
        | _ -> failwith "unexpected end!"
    | NameInput name when name = "exec" -> 
        if isInsideUnfinishedProc stack then 
            extend (NameItem name) stack 
        else 
            match stack with 
            | ProcItem prc :: rest -> 
                Execution (rest, toInputs [] prc)
            | _ -> failwith "type error in exec"
    | NameInput name when name = "if" -> 
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
        if isInsideUnfinishedProc stack then 
            extend (NameItem name) stack 
        else 
            // Lookup
            let op = lookupProc name 
            Reduction (op stack)
