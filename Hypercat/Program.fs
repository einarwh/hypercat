open System
open System.IO
open System.Threading.Tasks
open System.Text.RegularExpressions
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Server.Kestrel.Core
open Cat
open Legal
open View

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
            let m = Regex.Match(element, "\"(.+)\"")
            if m.Success then 
                let g =  m.Groups[1] 
                StringInput g.Value
            else 
                NameInput element

let toInputList (elements : string list) : Input list = 
    elements |> List.map toInput

let toElement (input : Input) : string = 
    match input with 
    | IntInput n -> n.ToString()
    | BoolInput b -> if b then "true" else "false"
    | StringInput s -> sprintf "\"%s\"" s 
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
        | StringItem s ->
            let quoted = sprintf "\"%s\"" s 
            toUrl (quoted :: elements) rest 
        | NameItem name ->
            toUrl (name :: elements) rest 
        | ListItem listItems -> 
            let s = "list" + "/" + toUrl [] listItems + "/" + "end"
            toUrl (s :: elements) rest 
        | ListMarker -> 
            toUrl ("list" :: elements) rest 
        | ProcItem procItems -> 
            let s = "proc" + "/" + toUrl [] procItems + "/" + "end"
            toUrl (s :: elements) rest 
        | ProcMarker -> 
            toUrl ("proc" :: elements) rest 

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

let createOperationsDiv stack = 
    let legal = legalOps stack
    let url = toLocationUrl stack []
    let createOplink url name = 
        let href = Path.Combine(url, name)
        a [ attr "style" "color:darkred"; attr "href" href ] [ str name ]
    let linkItems = 
        legal 
        |> List.map (fun opName -> createOplink url opName) 
        |> List.map (fun link -> li [] [link])
    div [] [ 
        h3 [] [ str "Operations" ] 
        div [] [
            ul [] linkItems
        ] 
    ] 

let rec toStackString (depth : int) (elements : string list) (items : CatItem list) : string = 
    let indentation = new string(' ', depth)
    match items with 
    | [] -> 
        match elements |> List.rev |> List.map (fun e -> indentation + e) with 
        | [] -> ""
        | mapped -> mapped |> List.reduce (fun s1 s2 -> s1 + "\n" + s2) 
    | it :: rest -> 
        match it with 
        | IntItem n -> 
            let s = n.ToString()
            toStackString depth (s :: elements) rest 
        | BoolItem b ->
            let s = if b then "true" else "false"
            toStackString depth (s :: elements) rest 
        | NameItem name ->
            toStackString depth (name :: elements) rest 
        | StringItem str ->
            toStackString depth (sprintf "\"%s\"" str :: elements) rest 
        | ListItem listItems -> 
            let s = 
                if List.isEmpty listItems then "end" + "\n" + indentation + "list"
                else "end" + "\n" + toStackString (depth + 1) [] listItems + "\n" + indentation + "list"
            toStackString depth (s :: elements) rest 
        | ListMarker -> 
            toStackString depth ("-list-" :: elements) rest
        | ProcItem procItems -> 
            let s = 
                if List.isEmpty procItems then "end" + "\n" + indentation + "proc"
                else "end" + "\n" + toStackString (depth + 1) [] procItems + "\n" + indentation + "proc"
            toStackString depth (s :: elements) rest 
        | ProcMarker -> 
            toStackString depth ("-proc-" :: elements) rest

let createInputDiv stack = 
    let url = toLocationUrl stack []
    let formActionTarget = url

    div [] [ 
        h3 [] [ str "Input" ] 
        div [] [
            form [ attr "action" formActionTarget; attr "method" "POST" ] [
                input [ attr "type" "radio"; attr "id" "number"; attr "name" "type"; attr "value" "number" ]
                label [ attr "for" "number" ] [ str "Number" ]
                input [ attr "type" "radio"; attr "id" "string"; attr "name" "type"; attr "value" "string" ]
                label [ attr "for" "string" ] [ str "String" ]
                // label [ attr "for" "text" ] [ str "Text" ]
                input [ attr "type" "text"; attr "id" "text"; attr "name" "text" ]
                input [ attr "type" "submit"; attr "value" "push" ] ] ] ] 

let createStackDiv stack =
    let stackString = toStackString 0 [] stack
    div [ attr "style" "width:300px;overflow:auto" ] [
        div [] [
            h3 [] [ str "Stack" ]
            pre [ attr "style" "font-family:Consolas" ] [ str stackString ]
        ]
    ]

let createDoc stack = 
    let operationsDiv = createOperationsDiv stack
    let inputDiv = createInputDiv stack
    let stackDiv = createStackDiv stack

    let doc = 
        html [] [
            head [] [
                title [] [ str "Hypercat: A hypermedia-driven concatenative programming language"]
                meta [ attr "name" "charset"; attr "content" "UTF-8" ]
                meta [ attr "name" "description"; attr "content" "Hypercat: A hypermedia-driven concatenative programming language" ]
                meta [ attr "name" "author"; attr "content" "Einar W. Høst" ]
                meta [ attr "name" "viewport"; attr "content" "width=device-width, initial-scale=1.0" ]
            ]
            body [ attr "style" "font-family:Consolas" ] [
                div [] [
                    h1 [] [ str "Hypercat" ] 
                    p [] [ str "A hypermedia-driven concatenative programming language." ]
                    table [ attr "valign" "top" ] [
                        tr [ attr "valign" "top" ] [
                            td [ attr "width" "200" ] [
                                operationsDiv
                                inputDiv
                            ]
                            td [ attr "width" "300" ] [
                                stackDiv
                            ]
                        ]
                    ]
                ]
            ]
        ]
    doc
        
let getHandler (ctx : HttpContext) : Task = 
    let routePath = ctx.Request.RouteValues["path"] :?> string
    let nonNullPath = if routePath = null then "" else routePath
    let elements = nonNullPath.Split("/") |> Array.toList |> List.filter (fun s -> s.Length > 0)
    try 
        if List.contains "clear" elements then 
            ctx.Response.Redirect("/", false)
            Task.CompletedTask;
        else
            let inputs = toInputList elements
            match inputs |> applyInputs [] with 
            | (Reduction st, inputsLeft) -> 
                let url = toLocationUrl st inputsLeft
                ctx.Response.StatusCode <- 302
                ctx.Response.Headers.Location <- url
                Task.CompletedTask
            | (Extension st, []) -> 
                let doc = createDoc st 
                let str = RenderView.AsString.htmlDocument doc
                ctx.Response.StatusCode <- 200
                ctx.Response.WriteAsync(str)
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

let postHandler (ctx : HttpContext) : Task = 
    match ctx.Request.HasFormContentType with
    | false -> 
        ctx.Response.StatusCode <- 400
        ctx.Response.WriteAsync("Missing form!")
    | true ->
        printfn "%A" ctx.Request.Form
        match ctx.Request.Form.TryGetValue("text"), ctx.Request.Form.TryGetValue("type") with 
        | (true, valuesText), (true, valuesType) ->
            let textStr = valuesText.[0]
            let typeStr = valuesType.[0]
            let pathStr = ctx.Request.Path.Value 
            try 
                let itemStr = 
                    if typeStr = "string" then 
                        sprintf "\"%s\"" textStr 
                    else 
                        let n = System.Int32.Parse textStr
                        n.ToString()
                let location = 
                    if pathStr.EndsWith("/") then sprintf "%s%s" pathStr itemStr 
                    else sprintf "%s/%s" pathStr itemStr
                ctx.Response.Redirect(location, false)
                Task.CompletedTask;
            with 
            | ex -> 
                ctx.Response.StatusCode <- 400
                ctx.Response.WriteAsync(sprintf "%s" ex.Message)
        | _ ->
            ctx.Response.StatusCode <- 400
            ctx.Response.WriteAsync("Missing input!")

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    builder.WebHost.ConfigureKestrel(fun serverOptions -> serverOptions.Limits.MaxRequestLineSize <- 262144) |> ignore
    
    let app = builder.Build()
    app.MapGet("/{**path}", Func<HttpContext, Task>(getHandler)) |> ignore
    app.MapPost("/{**path}", Func<HttpContext, Task>(postHandler)) |> ignore
    app.Run()
    0 
