module Cat

open System

exception StackUnderflowError of string

exception TypeError of string 

type Input = IntInput of int | BoolInput of bool | NameInput of string | StringInput of string

type CatItem = 
    IntItem of int 
    | BoolItem of bool 
    | NameItem of string 
    | StringItem of string 
    | ListItem of Cat
    | UnfinishedListItem of Cat
    | ProcItem of Cat 
    | UnfinishedProcItem of Cat 
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

let rec dropInnermost (stack : Cat) : Cat = 
    match stack with 
    | UnfinishedProcItem [] :: rest -> rest 
    | UnfinishedProcItem prc :: rest -> UnfinishedProcItem (dropInnermost prc) :: rest 
    | _ :: rest -> rest 
    | [] -> failwith "should never be empty!"

let drop (stack : Cat) : Cat = 
    match stack with 
    | a :: rest ->
        match a with 
        | UnfinishedProcItem unfinished -> 
            dropInnermost stack 
        | _ -> raise (TypeError "drop")
    | _ -> raise (StackUnderflowError "drop")

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
        | ListItem l1, ListItem l2 -> ListItem (l1 @ l2) :: rest
        | ProcItem p1, ProcItem p2 -> ProcItem (p1 @ p2) :: rest
        | _ -> raise (TypeError "concat")
    | _ -> raise (StackUnderflowError "concat")

let head (stack : Cat) : Cat = 
   match stack with 
    | a :: rest -> 
        match a with 
        | ListItem block ->
            match block with 
            | h :: _ -> h :: rest 
            | _ -> raise (StackUnderflowError "head")
        | ProcItem block ->
            match block with 
            | h :: _ -> h :: rest 
            | _ -> raise (StackUnderflowError "head")
        | _ -> raise (TypeError "head")
    | _ -> raise (StackUnderflowError "head")

let tail (stack : Cat) : Cat = 
   match stack with 
    | a :: rest -> 
        match a with 
        | ListItem block ->
            match block with 
            | _ :: t -> 
                (ListItem t) :: rest 
            | _ -> raise (StackUnderflowError "tail")
        | ProcItem block ->
            match block with 
            | _ :: t -> 
                (ProcItem t) :: rest 
            | _ -> raise (StackUnderflowError "tail")
        | _ -> raise (TypeError "tail")
    | _ -> raise (StackUnderflowError "tail")

let rev (stack : Cat) : Cat = 
   match stack with 
    | a :: rest -> 
        match a with 
        | ListItem block -> (ListItem (List.rev block)) :: rest
        | ProcItem block -> (ProcItem (List.rev block)) :: rest
        | _ -> raise (TypeError "rev")
    | _ -> raise (StackUnderflowError "rev")

let cons (stack : Cat) : Cat = 
   printfn "cons %A" stack 
   match stack with 
    | a :: b :: rest -> 
        match a with 
        | ListItem block -> (ListItem (b :: block)) :: rest
        | ProcItem block -> (ProcItem (b :: block)) :: rest
        | _ -> raise (TypeError "cons")
    | _ -> raise (StackUnderflowError "cons")

let flatten (stack : Cat) : Cat = 
   match stack with 
    | a :: rest -> 
        match a with 
        | ListItem block -> block @ rest
        | _ -> raise (TypeError "flatten")
    | _ -> raise (StackUnderflowError "flatten")

let mapItem (codeBlock : CatItem list) (item : CatItem) : CatItem list = 
    [ NameItem "exec"; ProcItem codeBlock; item ]

let executeMap (dataBlock : CatItem list) (codeBlock : CatItem list) : CatItem list = 
    let rec fn (stack : CatItem list) (dataItems : CatItem list) = 
        match dataItems with 
        | [] -> stack 
        | item :: rest -> 
            let added = NameItem "cons" :: NameItem "swap" :: NameItem "exec" :: ProcItem codeBlock :: item :: stack
            fn added rest
    fn ([ProcItem []]) dataBlock

let map (stack : Cat) : Cat = 
   match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | ProcItem dataBlock, ProcItem codeBlock -> 
            let emptyBlock = ProcItem []
            let resultBlock = dataBlock |> List.collect (fun item -> mapItem codeBlock item)
            let resBlock = executeMap dataBlock codeBlock 
            resBlock @ rest 
        | _ -> raise (TypeError "map")
    | _ -> raise (StackUnderflowError "map")

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

let split (stack : Cat) : Cat = 
   printfn "split %A" stack 
   match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | StringItem str, StringItem sep -> 
            let ss = str.Split(sep) |> Seq.toList |> List.map (fun s -> StringItem s)
            ss @ rest
        | _ -> raise (TypeError "split")
    | _ -> raise (StackUnderflowError "split")

let ops : (string * (Cat -> Cat)) list = 
    [ ("clear", fun _ -> [])
      ("swap", swap)
      ("dup", dup)
      ("pop", pop)
      ("drop", drop)
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
      ("cons", cons)
      ("flatten", flatten)
      ("rev", rev)
      ("map", map)
      ("split", split)
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
    | (UnfinishedListItem unfinished) :: rest -> 
        (UnfinishedListItem (extendDown it unfinished)) :: rest
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
        | StringItem str -> toInputs (StringInput str :: inputs) rest 
        | ListItem listItems -> 
            let listInputs = [ NameInput "list" ] @ toInputs [] listItems @ [ NameInput "end" ]
            toInputs (listInputs @ inputs) rest 
        | UnfinishedListItem listItems -> 
            let listInputs = [ NameInput "proc" ] @ toInputs [] listItems
            toInputs (listInputs @ inputs) rest 
        | ProcItem procItems -> 
            let procInputs = [ NameInput "proc" ] @ toInputs [] procItems @ [ NameInput "end" ]
            toInputs (procInputs @ inputs) rest 
        | UnfinishedProcItem procItems -> 
            let procInputs = [ NameInput "proc" ] @ toInputs [] procItems
            toInputs (procInputs @ inputs) rest 

let rec endProc (item : CatItem) : CatItem = 
    match item with 
    | UnfinishedListItem lst -> 
        match lst with 
        | (UnfinishedListItem innerLst) :: rest -> 
            UnfinishedListItem ((endProc (UnfinishedListItem innerLst)) :: rest)
        | (UnfinishedProcItem innerPrc) :: rest -> 
            UnfinishedProcItem ((endProc (UnfinishedProcItem innerPrc)) :: rest)
        | _ -> ListItem lst 
    | UnfinishedProcItem prc -> 
        match prc with 
        | (UnfinishedListItem innerLst) :: rest -> 
            UnfinishedListItem ((endProc (UnfinishedListItem innerLst)) :: rest)
        | (UnfinishedProcItem innerPrc) :: rest -> 
            UnfinishedProcItem ((endProc (UnfinishedProcItem innerPrc)) :: rest)
        | _ -> ProcItem prc 
    | _ -> failwith "must be unfinished proc"

let rec eval (op : Cat -> Cat) (stack : Cat) : Cat = 
    match stack with 
    | UnfinishedListItem innerStack :: rest ->
        (UnfinishedListItem (eval op innerStack)) :: rest
    | _ -> 
        op stack 

let pushInput (e : Input) (stack : Cat) : PushResult =
    match e with 
    | IntInput n -> extend (IntItem n) stack 
    | BoolInput b -> extend (BoolItem b) stack 
    | StringInput s -> extend (StringItem s) stack 
    | NameInput name when name = "proc" -> 
        extend (UnfinishedProcItem []) stack
    | NameInput name when name = "list" -> 
        extend (UnfinishedListItem []) stack
    | NameInput name when name = "end" -> 
        match stack with 
        | UnfinishedListItem items :: rest -> 
            Extension (endProc (UnfinishedListItem items) :: rest)
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
        if name <> "drop" && isInsideUnfinishedProc stack then 
            extend (NameItem name) stack 
        else 
            // Lookup
            let op = lookupProc name 
            let stack' = eval op stack
            Reduction stack'
