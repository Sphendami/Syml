open Language

[<EntryPoint>]
let main argv =
    let rec repl () =
        printf ":> "
        let lexbuf = FSharp.Text.Lexing.LexBuffer<_>.FromTextReader stdin
        let term = 
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
                repl ()
                exit 0
        
        try
            typeof term |> printfn "type: %A"
        with
        | TypingException msg ->
            eprintfn $"typing error: {msg}"
            repl ()
        | e ->
            eprintfn "unexpected error:"
            eprintfn $"{e.Message}"
            eprintfn $"{e.StackTrace}"
            repl ()
        
        try
            eval term |> openClosure |> printfn "eval: %A"
        with
        | EvaluationException msg ->
            eprintfn $"runtime error: {msg}"
            repl ()
        | e ->
            eprintfn "unexpected error:"
            eprintfn $"{e.Message}"
            eprintfn $"{e.StackTrace}"
            repl ()

        repl ()
    
    repl ()
    0