module Legal

open Cat
open System

let rec oneArg stack = 
    match stack with 
    | ProcMarker :: _ -> false
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

let rec twoPrimitiveValuesOfSameType stack = 
    match stack with 
    | IntItem _ :: IntItem _ :: _ -> true 
    | BoolItem _ :: BoolItem _ :: _ -> true 
    | StringItem _ :: StringItem _ :: _ -> true 
    | _ -> false

let oneString stack = 
    match stack with 
    | StringItem _ :: _ -> true 
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

let rec oneList stack = 
    match stack with 
    | ListItem _ :: _ -> true 
    | _ -> false

let rec oneBlock stack = 
    match stack with 
    | ListItem _ :: _ -> true 
    | ProcItem _ :: _ -> true 
    | _ -> false

let rec oneListOrString stack = 
    match stack with 
    | ListItem _ :: _ -> true 
    | StringItem _ :: _ -> true 
    | _ -> false

let rec twoListsOrStrings stack = 
    match stack with 
    | ListItem _ :: ListItem _ :: _ -> true 
    | StringItem _ :: StringItem _ :: _ -> true 
    | _ -> false

let rec nonEmptyBlock stack = 
    match stack with 
    | ListItem (_ :: _) :: _ -> true 
    | ProcItem (_ :: _) :: _ -> true 
    | _ -> false

let rec nonEmptyListOrString stack = 
    match stack with 
    | ListItem (_ :: _) :: _ -> true 
    | StringItem str :: _ -> str.Length > 0 
    | _ -> false

let rec twoBlocks stack = 
    match stack with 
    | ListItem _ :: ListItem _ :: _ -> true 
    | ProcItem _ :: ProcItem _ :: _ -> true 
    | _ -> false

let rec mapPrecond stack = 
    match stack with 
    | ListItem _ :: ProcItem _ :: _ -> true 
    | _ -> false

let rec reducePrecond stack = 
    match stack with 
    | ListItem _ :: ProcItem _ :: _ -> true 
    | _ -> false

let consPrecond stack = 
    match stack with 
    | _ :: ListItem _ :: _ -> true 
    | _ -> false

let rec takePrecond stack = 
    match stack with 
    | ListItem _ :: IntItem _ :: _ -> true 
    | _ -> false

let rec ifPrecond stack = 
    match stack with 
    | ProcItem _ :: BoolItem _ :: _ -> true 
    | _ -> false

let rec ifelsePrecond stack = 
    match stack with 
    | ProcItem _ :: ProcItem _ :: BoolItem _ :: _ -> true 
    | _ -> false

let rec containsListMarker (stack : Cat) = 
    match stack with 
    | [] -> false
    | ListMarker :: _ -> true
    | _ :: rest -> containsListMarker rest

let rec containsProcMarker (stack : Cat) = 
    match stack with 
    | [] -> false
    | ProcMarker :: _ -> true
    | _ :: rest -> containsProcMarker rest

let rec containsAnyMarker (stack : Cat) = 
    match stack with 
    | [] -> false
    | ProcMarker :: _ -> true
    | ListMarker :: _ -> true
    | _ :: rest -> containsAnyMarker rest

let preconds : (string * (Cat -> bool)) list = 
    [ ("clear", noPrecond)
      ("swap", twoArgs)
      ("roll", twoInts)
      ("dup", oneArg)
      ("pop", oneArg)
      ("drop", containsProcMarker)
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
      ("gt", twoPrimitiveValuesOfSameType)
      ("lt", twoPrimitiveValuesOfSameType)
      ("not", oneBool)
      ("and", twoBools)
      ("or", twoBools)
      ("xor", twoBools)
      ("concat", twoListsOrStrings)
      ("len", oneListOrString)
      ("head", nonEmptyListOrString)
      ("tail", nonEmptyListOrString)
      ("take", takePrecond)
      ("cons", consPrecond)
      ("rev", oneListOrString)
      ("map", mapPrecond)
      ("reduce", reducePrecond)
      ("split", twoStrings)
      ("int", oneString)
      ("exec", execPrecond)
      ("flatten", oneList)
      ("sort", oneList)
      ("if", ifPrecond)
      ("ifelse", ifelsePrecond)
      ("proc", noPrecond)
      ("list", noPrecond)
      ("end", containsAnyMarker)
    ]

let lookupPrecond (name : string) : Cat -> bool = 
    preconds |> List.find (fun (n, p) -> n = name) |> snd

let legalOps (stack : Cat) : string list = 
    let check (name, precond) = 
        if containsProcMarker stack || precond stack then Some name else None 
    preconds |> List.choose check
