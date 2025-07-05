open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http

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
        
let getHandler (ctx : HttpContext) : Task = 
    let routePath = ctx.Request.RouteValues["path"] :?> string
    let nonNullPath = if routePath = null then "" else routePath
    let pathElements = nonNullPath.Split("/") |> Array.toList |> List.filter (fun s -> s.Length > 0)
    try 
        printfn "%A" pathElements
        let result = "fojsfkj"
        let items = toItemList ([], []) pathElements
        printfn "ITEMS %A" items
        // let result = run pathElements
        ctx.Response.WriteAsync(result)
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
