namespace Suave

module Http =
    type RequestType = GET | POST
    type Request = {
        Route : string
        Type : RequestType
        }
    type Response = {
        Content : string
        StatusCode : int
        }
    type Context = {
        Request : Request
        Response : Response
        }
    type WebPart = Context -> Async<Context option>

module Successful =
    open Http
    let OK (content : string) : WebPart = fun context ->
        { context with Response = { Content = content; StatusCode = 200 } }
        |> Some
        |> async.Return

module Console =
    open Http
    let execute (inputContext : Context) (webpart : WebPart) : unit =
        async {
            let! outputContext = webpart inputContext
            match outputContext with
            | Some context ->
                printfn "---------------"
                printfn "Code : %d" context.Response.StatusCode
                printfn "Output : %s" context.Response.Content
                printfn "---------------"
            | None -> printfn "no output"
            } |> Async.RunSynchronously

    let parseRequest (input : string) : Request =
        let parts = input.Split ';'
        let raw_type = parts.[0]
        let route = parts.[1]
        match raw_type with
        | "GET" -> { Type = GET; Route = route }
        | "POST" -> { Type = POST; Route = route }
        | _ -> failwith "invalid request"

    let rec executeLoop (input_context : Context) (webpart : WebPart) : unit =
        printfn "enter input route :"
        match System.Console.ReadLine() with
        | "exit" -> ()
        | input ->
            let context = { input_context with Request = parseRequest input }
            try execute context webpart
                executeLoop input_context webpart
            with ex -> printfn "error : %s" ex.Message

module Combinators =
    open Http

    let compose (first : WebPart) (second : WebPart) (context : Context) : Async<Context option> =
        async {
        let! first_context = first context
        match first_context with
        | None -> return None
        | Some context ->
            let! second_context = second context
            return second_context }
    let (>=>) = compose

module Filters =
    open Http

    let iff : (Context -> bool) -> WebPart = fun condition context ->
        if condition context
        then context |> Some |> async.Return
        else None |> async.Return

    let GET : WebPart = iff (fun context -> context.Request.Type = GET)
    let POST : WebPart = iff (fun context -> context.Request.Type = POST)
    let Path (path : string) : WebPart = iff (fun context -> context.Request.Route = path)

    let rec Choose (webparts : WebPart list) : WebPart = fun context -> async {
        match webparts with
        | [] -> return None
        | webpart :: webparts ->
            let! result = webpart context
            match result with
            | Some context -> return Some context
            | None -> return! Choose webparts context
        }
