open System
open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http
open Cat
open Legal

let createOplink (url : string) name = 
    let href = Path.Combine(url, name)
    let result = sprintf "<a href=\"%s\">%s</a>" href name
    result

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
        | NameItem name ->
            toUrl (name :: elements) rest 
        | ProcItem procItems -> 
            let s = "begin" + "/" + toUrl [] procItems + "/" + "end"
            toUrl (s :: elements) rest 
        | UnfinishedProcItem procItems -> 
            let s = "begin" + "/" + toUrl [] procItems
            toUrl (s :: elements) rest 

let rec applyInputs (stack : Cat) (inputs : Input list)  = 
    match inputs with 
    | [] -> (Extension stack, [])
    | h :: rest -> 
        let result = pushInput h stack 
        match result with 
        | Reduction st -> 
            (Reduction st, rest)
        | Extension st -> 
            applyInputs st rest
        | Execution (st, procInputs) -> 
            (Reduction st, (procInputs @ rest))

let toLocationUrl (st : Cat) (inputsLeft : Input list) = 
    match (st, inputsLeft) with 
    | ([], []) -> "/"
    | (_, []) -> 
        let url = st |> toUrl []
        "/" + url
    | ([], _) -> 
        let url = inputsLeft |> toElementList |> List.reduce (fun s1 s2 -> s1 + "/" + s2)
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
        let inputs = toInputList elements
        match inputs |> applyInputs [] with 
        | (Reduction st, inputsLeft) -> 
            let url = toLocationUrl st inputsLeft
            ctx.Response.StatusCode <- 302
            ctx.Response.Headers.Location <- url
            Task.CompletedTask
        | (Extension st, []) -> 
            let legal = legalOps st
            let url = toLocationUrl st []
            let linkItems = 
                legal 
                |> List.map (fun opName -> createOplink url opName) 
                |> List.map (fun link -> sprintf "<li>%s</li>" link)
                |> List.reduce (fun ul1 ul2 -> ul1 + ul2)
            let header = sprintf "<h1>Hypercat</h1>"
            let description = sprintf "<p>A hypermedia-driven concatenative programming language.</p>"
            let linkList = sprintf "<ul>%s</ul>" linkItems
            let body = sprintf "<body>%s %s %s</body>" header description linkList
            let doc = sprintf "<html><style>body { font-family: consolas; }</style><body>%s</body></html>" body
            ctx.Response.StatusCode <- 200
            ctx.Response.WriteAsync(doc)
        | _ -> 
            failwith "?"
    with 
    | StackUnderflowError opname -> 
        ctx.Response.StatusCode <- 400
        ctx.Response.WriteAsync(sprintf "Stack underflow in %s!" opname)
    | TypeError opname -> 
        ctx.Response.StatusCode <- 400
        ctx.Response.WriteAsync(sprintf "Type error in %s!" opname)
    | ex -> 
        ctx.Response.StatusCode <- 500
        ctx.Response.WriteAsync(ex.Message)

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()
    app.MapGet("/{**path}", Func<HttpContext, Task>(getHandler)) |> ignore
    app.Run()
    0 
