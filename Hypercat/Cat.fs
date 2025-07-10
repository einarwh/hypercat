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
    | ListMarker 
    | ListItem of Cat
    | ProcMarker
    | ProcItem of Cat 
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

let drop (stack : Cat) : Cat = 
    match stack with 
    | a :: rest -> rest
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
        | StringItem s1, StringItem s2 -> BoolItem (s1 = s2) :: rest
        | _ -> BoolItem false :: rest
    | _ -> raise (StackUnderflowError "eq")

let ne (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> BoolItem (n2 <> n1) :: rest
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 <> b1) :: rest
        | StringItem s1, StringItem s2 -> BoolItem (s1 <> s2) :: rest
        | _ -> BoolItem true :: rest
    | _ -> raise (StackUnderflowError "ne")

let gt (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> BoolItem (n2 > n1) :: rest
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 > b1) :: rest
        | StringItem s1, StringItem s2 -> BoolItem (s2 > s1) :: rest
        | _ -> raise (TypeError "gt")
    | _ -> raise (StackUnderflowError "gt")

let lt (stack : Cat) : Cat = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | IntItem n1, IntItem n2 -> BoolItem (n2 < n1) :: rest
        | BoolItem b1, BoolItem b2 -> BoolItem (b2 < b1) :: rest
        | StringItem s1, StringItem s2 -> BoolItem (s2 < s1) :: rest
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
        | StringItem s1, StringItem s2 -> StringItem (s1 + s2) :: rest
        | ProcItem p1, ProcItem p2 -> ProcItem (p1 @ p2) :: rest
        | _ -> raise (TypeError "concat")
    | _ -> raise (StackUnderflowError "concat")

let head (stack : Cat) : Cat = 
   match stack with 
    | a :: rest -> 
        match a with 
        | StringItem str ->
            if str = "" then 
                raise (Exception "Empty string has no head")
            else 
                StringItem (str[0].ToString()) :: rest
        | ListItem block ->
            match block with 
            | [] -> raise (Exception "Empty list has no head")
            | h :: _ -> h :: rest 
        | _ -> raise (TypeError "head")
    | _ -> raise (StackUnderflowError "head")

let tail (stack : Cat) : Cat = 
   match stack with 
    | a :: rest -> 
        match a with 
        | StringItem str ->
            if str = "" then 
                raise (Exception "Empty string has no tail")
            else 
                StringItem (str.Substring(1)) :: rest
        | ListItem block ->
            match block with 
            | [] -> raise (Exception "Empty list has no tail")
            | _ :: t -> (ListItem t) :: rest 
        | _ -> raise (TypeError "tail")
    | _ -> raise (StackUnderflowError "tail")

let reverseString (s : string) = 
    let charArray = s.ToCharArray()
    Array.Reverse(charArray)
    new string(charArray)

let rev (stack : Cat) : Cat = 
   match stack with 
    | a :: rest -> 
        match a with 
        | ListItem block -> (ListItem (List.rev block)) :: rest
        | StringItem str -> (StringItem (reverseString str)) :: rest
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

// let mapItem (codeBlock : CatItem list) (item : CatItem) : CatItem list = 
//     [ NameItem "exec"; ProcItem codeBlock; item ]

let executeMap (dataBlock : CatItem list) (codeBlock : CatItem list) : CatItem = 
    let rec fn (stack : CatItem list) (dataItems : CatItem list) = 
        match dataItems with 
        | [] -> stack
        | item :: rest -> 
            let added = NameItem "exec" :: ProcItem codeBlock :: item :: stack
            printfn "added %A" added
            fn added rest
    let items = fn [] (dataBlock |> List.rev)
    ListItem items

let executeReduce (dataBlock : CatItem list) (codeBlock : CatItem list) : CatItem list = 
    let rec fn (stack : CatItem list) (dataItems : CatItem list) = 
        match dataItems with 
        | [] -> stack
        | item :: rest -> 
            let reduced = NameItem "exec" :: ProcItem codeBlock :: item :: stack
            printfn "reduced %A" reduced
            fn reduced rest
    match dataBlock with
    | [] -> raise (StackUnderflowError "reduce")
    | h :: rest -> 
        let result = fn [h] rest 
        result

let map (stack : Cat) : Cat = 
    printfn "map %A" stack
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | ListItem dataBlock, ProcItem codeBlock -> 
            // let emptyBlock = ProcItem []
            // let resultBlock = dataBlock |> List.collect (fun item -> mapItem codeBlock item)
            let listItem = executeMap dataBlock codeBlock 
            listItem :: rest 
        | _ -> raise (TypeError "map")
    | _ -> raise (StackUnderflowError "map")

let reduce (stack : Cat) : Cat = 
    printfn "reduce %A" stack
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | ListItem dataBlock, ProcItem codeBlock -> 
            // let emptyBlock = ProcItem []
            let block = executeReduce dataBlock codeBlock 
            block @ rest 
        | _ -> raise (TypeError "map")
    | _ -> raise (StackUnderflowError "map")

let trueOp (stack : Cat) : Cat = 
    stack |> push (BoolItem true) 

let falseOp (stack : Cat) : Cat = 
    stack |> push (BoolItem false) 

let zero (stack : Cat) : Cat = 
    stack |> push (IntItem 0) 

let succ (stack : Cat) : Cat = 
    printfn "succ %A" stack
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
            (ListItem ss) :: rest
        | _ -> raise (TypeError "split")
    | _ -> raise (StackUnderflowError "split")

let intOp (stack : Cat) : Cat = 
   match stack with 
    | a :: rest -> 
        match a with 
        | StringItem str -> 
            match System.Int32.TryParse str with
            | true, n -> (IntItem n) :: rest 
            | _ -> raise (Exception <| sprintf "%s is not an integer value" str)
        | _ -> raise (TypeError "int")
    | _ -> raise (StackUnderflowError "int")

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
      ("split", split)
      ("int", intOp)
      ("concat", concat)
      ("head", head)
      ("tail", tail)
      ("cons", cons)
      ("flatten", flatten)
      ("rev", rev)
      ("map", map)
      ("reduce", reduce)
    ]

let lookupProc name = 
    match ops |> List.tryFind (fun (n, op) -> n = name) with
    | Some (n, op) -> op
    | None -> failwith <| sprintf "Unknown operation %s" name

let rec isInsideProc (stack : Cat) = 
    match stack with 
    | [] -> false
    | ProcMarker :: _ -> true
    | _ :: rest -> isInsideProc rest

let extend (it : CatItem) (stack : Cat) : PushResult = 
    Extension (it :: stack)

let rec toInputs (inputs : Input list) (items : CatItem list) : Input list = 
    match items with 
    | [] -> inputs // Rev?
    | it :: rest -> 
        match it with 
        | IntItem n -> 
            toInputs (IntInput n :: inputs) rest
        | BoolItem b -> 
            toInputs (BoolInput b :: inputs) rest
        | NameItem name -> 
            toInputs (NameInput name :: inputs) rest 
        | StringItem str -> 
            toInputs (StringInput str :: inputs) rest 
        | ListItem listItems -> 
            let listInputs = [ NameInput "list" ] @ toInputs [] listItems @ [ NameInput "end" ]
            toInputs (listInputs @ inputs) rest 
        | ListMarker -> 
            toInputs (NameInput "list" :: inputs) rest 
        | ProcItem procItems -> 
            let procInputs = [ NameInput "proc" ] @ toInputs [] procItems @ [ NameInput "end" ]
            toInputs (procInputs @ inputs) rest 
        | ProcMarker -> 
            toInputs (NameInput "proc" :: inputs) rest 

let rec executeProc (stack : Cat) = 
    match stack with 
    | ProcItem prc :: rest -> 
        Execution (rest, toInputs [] prc)
    | _ -> failwith "type error in exec"

let endBlock (stack : Cat) : Cat = 
    let rec findMarker (blockItems : CatItem list) (stack : Cat) = 
        match stack with 
        | [] -> failwith "no marker found"
        | ListMarker :: rest -> 
            ListItem (List.rev blockItems) :: rest 
        | ProcMarker :: rest -> 
            ProcItem (List.rev blockItems) :: rest 
        | item :: rest -> 
            findMarker (item :: blockItems) rest
    findMarker [] stack

let pushInput (e : Input) (stack : Cat) : PushResult =
    match e with 
    | IntInput n -> extend (IntItem n) stack 
    | BoolInput b -> extend (BoolItem b) stack 
    | StringInput s -> extend (StringItem s) stack 
    | NameInput name when name = "clear" -> 
        Extension []
    | NameInput name when name = "proc" -> 
        extend (ProcMarker) stack
    | NameInput name when name = "list" -> 
        extend (ListMarker) stack
    | NameInput name when name = "end" -> 
        Extension (endBlock stack)
    | NameInput name when name = "exec" -> 
        if isInsideProc stack then 
            extend (NameItem name) stack 
        else 
            executeProc stack
    | NameInput name when name = "if" -> 
        if isInsideProc stack then 
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
        if isInsideProc stack then 
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
    | NameInput name when name = "drop" -> 
        let op = lookupProc name 
        Reduction (op stack)
    | NameInput name -> 
        if isInsideProc stack then 
            extend (NameItem name) stack 
        else 
            // Lookup
            let op = lookupProc name 
            Reduction (op stack)
