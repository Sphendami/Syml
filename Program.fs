open Language


let tryTypeof cxt term =
    try
        typeof cxt term |> Some
    with
    | TypingException msg ->
        eprintfn $"typing error: {msg}"
        None
    | e ->
        eprintfn "unexpected error:"
        eprintfn $"{e.Message}"
        eprintfn $"{e.StackTrace}"
        None

let tryEval env term =
    try
        evalR env term |> Some
    with
    | EvaluationException msg ->
        eprintfn $"runtime error: {msg}"
        None
    | e ->
        eprintfn "unexpected error:"
        eprintfn $"{e.Message}"
        eprintfn $"{e.StackTrace}"
        None


let rec repl cxt env (scriptToPrepend: string) =
    let input =
        seq {
            yield scriptToPrepend

            let prompt =
                let mutable called = false
                fun () ->
                    if not called then
                        called <- true
                        ":> "
                    else ":  "

            let mutable foundEOT = scriptToPrepend.Contains ";;"
            while not foundEOT do
                printf $"{prompt()}"
                let line = stdin.ReadLine()
                foundEOT <- line.Contains ";;"
                yield line
        }
        |> String.concat "\n"
    let splitingIndex = input.IndexOf ";;" + 2
    let scriptForThisTime = input.Substring(0, splitingIndex)
    let scriptForNextTime = input.Substring(splitingIndex)

    let lexbuf = FSharp.Text.Lexing.LexBuffer<_>.FromString scriptForThisTime
    let toplevel = 
        try
            Parser.main Lexer.token lexbuf
        with e ->
            match e.Message with
            | "unrecognized input" ->
                let position = lexbuf.StartPos
                let posLine = position.Line + 1
                let posChar = position.Column + 1
                eprintfn $"tokenizing error: @line {posLine}, char {posChar}"
            | "parse error" ->
                let token = System.String lexbuf.Lexeme
                let position = lexbuf.StartPos
                let posLine = position.Line + 1
                let posChar = position.Column + 1
                eprintfn $"parsing error: \"{token}\" @line {posLine}, char {posChar}"
            | msg ->
                eprintfn "unexpected error:"
                eprintfn $"{msg}"
                eprintfn $"{e.StackTrace}"
            repl cxt env scriptForNextTime
            exit 0
    
    match toplevel with
    | Term term ->
        match tryTypeof cxt term with
        | None -> repl cxt env scriptForNextTime
        | Some ty ->
            ty |> printfn "type: %A"
            match tryEval env term with
            | None -> repl cxt env scriptForNextTime
            | Some value ->
                value |> openClosure ||> printfn "eval: %A (with %A)"
                repl cxt env scriptForNextTime
    | ToplevelLet (x, term) ->
        match tryTypeof cxt term with
        | None -> repl cxt env scriptForNextTime
        | Some ty ->
            ty |> printfn "type: %s : %A" x
            match tryEval env term with
            | None -> repl cxt env scriptForNextTime
            | Some value ->
                value |> openClosure ||> printfn "eval: %s = %A (with %A)" x
                repl (cxt.Add(x, ty)) (env.Add(x, value)) scriptForNextTime
    | Directive directive ->
        match directive with
        | Help -> printfn "directives:
    #help;;  :  Show this help message
    #exit;;  :  Terminate this interactive session"
        | Exit -> exit 0
        repl cxt env scriptForNextTime
    | Eof ->
        ()

    
[<EntryPoint>]
let main argv =
    repl Map.empty Map.empty ""
    0