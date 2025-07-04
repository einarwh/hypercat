module Stack

open System

type StackItem = NumItem of int | BoolItem of bool | BlockItem of Stack
and Stack = StackItem list

let top (stack : Stack) : StackItem = 
    match stack with 
    | [] -> failwith "top"
    | h :: _ -> h

let push (e : StackItem) (stack : Stack) : Stack =
    e :: stack

let drop (stack : Stack) : Stack = 
    match stack with 
    | tos :: rest -> rest
    | _ -> failwith "stack underflow in drop"

let swap (stack : Stack) : Stack = 
    match stack with 
    | a :: b :: rest -> b :: a :: rest 
    | _ -> failwith "stack underflow in swap"

let dup (stack : Stack) : Stack = 
    match stack with 
    | tos :: rest -> tos :: tos :: rest
    | _ -> failwith "stack underflow in dup"

let binaryMathOp (op : int -> int -> int) (stack : Stack) = 
    match stack with 
    | a :: b :: rest -> 
        match (a, b) with 
        | NumItem n1, NumItem n2 -> NumItem (op n2 n1) :: rest
        | _ -> failwith "type error in binary math operation"
    | _ -> failwith "stack underflow in binary math operation"

let binaryComparisonOp (check : StackItem -> StackItem -> bool) (stack : Stack) = 
    match stack with 
    | a :: b :: rest -> 
        BoolItem (check b a) :: rest
    | _ -> failwith "stack underflow in binary comparison operation"

let binaryLogicOp (op : StackItem -> StackItem -> bool) (stack : Stack) = 
    match stack with 
    | a :: b :: rest -> 
        BoolItem (op b a) :: rest 
    | _ -> failwith "stack underflow in binary math operation"

let add = binaryMathOp (+)

let sub = binaryMathOp (-)

let mul = binaryMathOp (*)

let div = binaryMathOp (/) 

let modulo = binaryMathOp (%)

let eq = binaryComparisonOp (=)

let ne = binaryComparisonOp (<>)

let gt = binaryComparisonOp (>)

let ge = binaryComparisonOp (>=)

let lt = binaryComparisonOp (<)

let le = binaryComparisonOp (<=)

let notOp stack = 
    match stack with 
    | tos :: rest -> 
        (not tos) :: rest
    | _ -> failwith "stackunderflow in not"

let logicAnd (item1 : StackItem) (item2 : StackItem) : bool = 
    match item1, item2 with 
    | BoolItem b1, BoolItem b2 -> b2 && b1
    | _ -> failwith "typeerror"

let logicOr (item1 : StackItem) (item2 : StackItem) : bool = 
    match item1, item2 with 
    | BoolItem b1, BoolItem b2 -> b2 || b1
    | _ -> failwith "typeerror"

let andOp = binaryLogicOp logicAnd

let orOp = binaryLogicOp logicOr

let t stack = BoolItem true :: stack

let f stack = BoolItem false :: stack

let exec stack = 
    match stack with 
    | tos :: rest -> 
        match tos with 
        | NumItem n -> tos :: rest 
        | BoolItem b -> tos :: rest 
        | BlockItem block -> tos :: rest
    | _ -> failwith "stack underflow in exec"
