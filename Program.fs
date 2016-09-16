module MiniSuave

open Suave.Http
open Suave.Console
open Suave.Successful
open Suave.Combinators
open Suave.Filters

let app : WebPart =
    Choose [
        GET >=> Path "/hello" >=> OK "Hello GET"
        POST >=> Path "/hello" >=> OK "Hello POST"
        Path "/foo" >=> Choose [ GET >=> OK "Foo GET"
                                 POST >=> OK "Foo POST" ]
        ]

[<EntryPoint>]
let main argv =
    let request = { Route = ""; Type = Suave.Http.GET }
    let response = { Content = ""; StatusCode = 200 }
    let context = { Request = request; Response = response }
    //let composed : WebPart = (GET >=> Path "/hello" >=> OK "hello")
    executeLoop context app
    0 // return an integer exit code
