open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http
open Cat

type Item = SimpleItem of string | CompositeItem of Item list

let rec toItemList (stack : Item list list, current : Item list) (elements : string list) : (Item list list * Item list) = 
    match elements with 
    | [] ->
        (stack, List.rev current)
    | h :: t when h = "begin" ->
        // Start new block
        toItemList (current :: stack, []) t
    | h :: t when h = "end" -> 
        // Finish current block 
        match stack with 
        | headStack :: restStack -> 
            toItemList (restStack, (CompositeItem (List.rev current)) :: headStack) t
        | _ -> failwith "unexpected end"
    | h :: t -> 
        // Add to current block 
        toItemList (stack, (SimpleItem h) :: current) t

let toInput (element : string) : Input = 
    match System.Int32.TryParse element with
    | true, n -> IntInput n 
    | _ -> 
        if String.Equals("true", element, StringComparison.OrdinalIgnoreCase) then 
            BoolInput true
        else if String.Equals("false", element, StringComparison.OrdinalIgnoreCase) then 
            BoolInput false
        else 
            NameInput element

let toInputList (elements : string list) : Input list = 
    elements |> List.map toInput

let toElement (input : Input) : string = 
    match input with 
    | IntInput n -> n.ToString()
    | BoolInput b -> if b then "true" else "false"
    | NameInput s -> s 

let toElementList (inputs : Input list) : string list = 
    inputs |> List.map toElement 

let rec toUrl (elements : string list) (items : CatItem list) : string = 
    match items with 
    | [] -> 
        match elements with 
        | [] -> ""
        | _ -> elements |> List.reduce (fun s1 s2 -> s1 + "/" + s2) 
    | it :: rest -> 
        match it with 
        | IntItem n -> 
            let s = n.ToString()
            toUrl (s :: elements) rest 
        | BoolItem b ->
            let s = if b then "true" else "false"
            toUrl (s :: elements) rest 
        | ProcItem procItems -> 
            let s = "begin" + "/" + toUrl [] procItems + "/" + "end"
            toUrl (s :: elements) rest 
        | UnfinishedProcItem procItems -> 
            let s = "begin" + "/" + toUrl [] procItems
            toUrl (s :: elements) rest 

let rec applyInputs (stack : Cat) (inputs : Input list)  = 
    printfn "applyInputs, inputs left: %A" inputs
    match inputs with 
    | [] -> (Extension stack, [])
    | h :: rest -> 
        let result = pushInput h stack 
        printfn "RESULT: %A" result
        match result with 
        | Reduction st -> 
            (Reduction st, rest)
        | Extension st -> 
            applyInputs st rest

let toLocationUrl (st : Cat) (inputsLeft : Input list) = 
    match inputsLeft with 
    | [] -> 
        let url = st |> toUrl []
        "/" + url
    | _ -> 
        let urlPart1 = st |> toUrl []
        let urlPart2 = inputsLeft |> toElementList |> List.reduce (fun s1 s2 -> s1 + "/" + s2)
        "/" + urlPart1 + "/" + urlPart2 
        
let getHandler (ctx : HttpContext) : Task = 
    let routePath = ctx.Request.RouteValues["path"] :?> string
    let nonNullPath = if routePath = null then "" else routePath
    let elements = nonNullPath.Split("/") |> Array.toList |> List.filter (fun s -> s.Length > 0)
    try 
        printfn "elements %A" elements
        let result = "fojsfkj"
        let inputs = toInputList elements
        printfn "inputs %A" inputs
        match inputs |> applyInputs [] with 
        | (Reduction st, inputsLeft) -> 
            printfn "Reduced to stack: %A" st
            let url = toLocationUrl st inputsLeft
            printfn "url: %A" url
            ctx.Response.StatusCode <- 302
            ctx.Response.Headers.Location <- url
            Task.CompletedTask
        | (Extension st, []) -> 
            ctx.Response.StatusCode <- 200
            ctx.Response.WriteAsync(sprintf "%A" st)
        | _ -> 
            failwith "?"
    with 
    // | StackUnderflowException -> 
    //     ctx.Response.StatusCode <- 400
    //     ctx.Response.WriteAsync("Stack underflow exception!")
    // | TypeException msg -> 
    //     ctx.Response.StatusCode <- 400
    //     ctx.Response.WriteAsync(msg)
    | ex -> 
        ctx.Response.StatusCode <- 500
        ctx.Response.WriteAsync(ex.Message)

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()
    app.MapGet("/{**path}", Func<HttpContext, Task>(getHandler)) |> ignore
    // app.MapPost("/{**path}", Func<HttpContext, Task>(postHandler)) |> ignore
    app.Run()
    0 
