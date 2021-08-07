open Language

[<EntryPoint>]
let main argv =
    printf ":> "
    let lexbuf = FSharp.Text.Lexing.LexBuffer<_>.FromTextReader stdin
    let term = Parser.main Lexer.token lexbuf
    typeof term |> printfn "type: %A"
    eval term |> openClosure |> printfn "eval: %A"
    0