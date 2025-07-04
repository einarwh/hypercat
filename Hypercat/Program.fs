open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http

// let toItemList (pathElements : string list) : StackItem list = 
//     match pathElements with 
//     | h :: t -> 

//         if h = "begin" then 

//     let rec fn elements acc = 
//         match elements with 
//         | 

let getHandler (ctx : HttpContext) : Task = 
    let routePath = ctx.Request.RouteValues["path"] :?> string
    let nonNullPath = if routePath = null then "" else routePath
    let pathElements = nonNullPath.Split("/") |> Array.toList |> List.filter (fun s -> s.Length > 0)
    try 
        printfn "%A" pathElements
        let result = "fojsfkj"
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
