open System.IO

open Language


let openFileOrStdin filename =
    try
        File.OpenText filename :> TextReader
    with
    | :? FileNotFoundException as e ->
        eprintfn $"No such file: {e.FileName}"
        eprintfn $"Continue with standard input."
        stdin
    | e ->
        eprintfn "unexpected error:"
        eprintfn $"{e.Message}"
        eprintfn $"{e.StackTrace}"
        printfn ""
        eprintfn $"Continue with standard input."
        stdin

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


let rec repl cxt env (scriptToPrepend: string) (reader: TextReader) =
    let input =
        seq {
            yield scriptToPrepend

            let prompt =
                let mutable called = false
                fun () ->
                    if reader <> stdin then ""
                    else if not called then
                        called <- true
                        ":> "
                    else ":  "

            let mutable foundEOT = scriptToPrepend.Contains ";;"
            let mutable isEOF = false
            while not foundEOT && not isEOF do
                printf $"{prompt()}"
                let line = reader.ReadLine()
                if not (isNull line) then
                    foundEOT <- line.Contains ";;"
                    yield line
                else
                    isEOF <- true
        }
        |> String.concat "\n"
    let splitingIndex =
        if input.Contains ";;" then input.IndexOf ";;" + 2 else input.Length
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
                let posLine = position.Line
                let posChar = position.Column + 1
                eprintfn $"parsing error: `{token}` @line {posLine}, char {posChar}"
            | msg ->
                eprintfn "unexpected error:"
                eprintfn $"{msg}"
                eprintfn $"{e.StackTrace}"
            repl cxt env scriptForNextTime reader
            exit 0
    
    match toplevel with
    | Term term ->
        match tryTypeof cxt term with
        | None -> repl cxt env scriptForNextTime reader
        | Some ty ->
            ty |> printfn "type: %A"
            match tryEval env term with
            | None -> repl cxt env scriptForNextTime reader
            | Some value ->
                value |> openClosure ||> printfn "eval: %A (with %A)"
                repl cxt env scriptForNextTime reader
    | ToplevelLet (x, term) ->
        match tryTypeof cxt term with
        | None -> repl cxt env scriptForNextTime reader
        | Some ty ->
            ty |> printfn "type: %s : %A" x
            match tryEval env term with
            | None -> repl cxt env scriptForNextTime reader
            | Some value ->
                value |> openClosure ||> printfn "eval: %s = %A (with %A)" x
                repl (cxt.Add(x, ty)) (env.Add(x, value)) scriptForNextTime reader
    | Directive directive ->
        match directive with
        | Help ->
            printfn "If a file name is given as a command line argument,
  first it's content is read and evaluated,
  then move on to the interactive session.

directives:
    #help;;
        Show this help message.
    #load <file-name-w/o-extension>;;
        Read the file content as input and evaluate it.
        The file name is resrticted to be the string
          which can be recognized as an identifier.
        The extension '.simplang' is apended to the given file name.
    #exit;;
        Terminate the interactive session."
            repl cxt env scriptForNextTime reader
        | Load filename ->
            use fileReader = openFileOrStdin (filename + ".simplang")
            repl cxt env scriptForNextTime fileReader
        | Exit -> exit 0
    | Eof ->
        reader.Close()
        repl cxt env scriptForNextTime stdin

    
[<EntryPoint>]
let main argv =
    use reader =
        if argv.Length = 0 then stdin
        else
            openFileOrStdin argv.[0]

    repl Map.empty Map.empty "" reader
    0