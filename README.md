Small programming language with functional programming and type inference

# Usage
.NET environemnt is required.

- `dotnet run` : Run the interactive session.
- `dotnet run <file-name>` : Load and evaluate the file and start the interactive session.

# Syntax
```
T ::=
      | x                     variable
      | true                  true
      | false                 false
      | i                     integer
      | fun x -> T            function
      | T T                   application
      | if T then T else T    conditional branching
      | let x = T in T        let binding and sequencing
      | let rec x = T in T    recursive let binding and sequencing
      | T && T                boolean and
      | T || T                boolean or
      | T = T                 equality
      | T < T                 less than
      | T > T                 greater than
      | T <= T                less than or equal
      | T >= T                greater than or equal
      | T + T                 addition
      | T - T                 subtraction
      | T * T                 multiplication
      | T / T                 division
      | + T                   positive sign
      | - T                   negative sign
```

# Type
```
Ty ::=
      | Bool            boolean type
      | Int             integer type
      | Ty -> Ty        function type
```

# Comment
```
// comment
```


# Toplevel let binding
- `let x = T`
- `let rec x = T`

# Directives
- `#help`
- `#load`
- `#exit`

Put `#help;;` on the interactive session to show details.

# Example
```
:> let rec fact = fun n ->
:       if n <= 1 then 1
:       else n * fact (n - 1)
:  in
:  fact 5;;
type: Int
eval: 120 (with [])
```

" `(with [])` " shows currently bounded variable names (empty in this example.)

# Known issues
- Comment including "`;;`" causes unexpected behavior.
- Line number in error message becomes wrong in a certain case.
- Type error explanation in a certain case is unclear.
