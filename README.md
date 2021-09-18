Joke programming language where available characters to write its sourse codes are symbols only

# Usage
.NET environemnt is required.

- `dotnet run` : Run the interactive session.
- `dotnet run <file-name>` : Load and evaluate the file and start the interactive session.

# Available characters
`!` `?` `'` `"` ``` ` ``` `+` `-` `=` `(` `)` `[` `]` `{` `}` `<` `>` `;` `:` `,` `.` `@` `#` `$` `%` `&` `~` `|` `*` `^` `_` `/` `\`

# Syntax
- Variable is a sequence of available characters which starts with `$`.
- Integer is a sequence of `_` and `-` which starts with `#`, representing a binary sequence where `_` corresponds to 0 and `-` corresponds to 1. (e.g. `#--_-` means 1101 in binary, or 13 in decimal.)
```
T ::=
      | $<sequence of symbols>      variable
      | ?+                          true
      | ?-                          false
      | #<sequense of '_' and '-'>  integer
      | \<var> -> T                 function
      | T T                         application
      | T ? T : T                   conditional branching
      | !! <var> = T ; T            let binding and sequencing
      | !!^ <var> = T ; T           recursive let binding and sequencing
      | T && T                      boolean and
      | T || T                      boolean or
      | T = T                       equality
      | T < T                       less than
      | T > T                       greater than
      | T <= T                      less than or equal
      | T >= T                      greater than or equal
      | T + T                       addition
      | T - T                       subtraction
      | T * T                       multiplication
      | T / T                       division
      | + T                         positive sign
      | - T                         negative sign
```

# Type
```
Ty ::=
      | ^?            boolean type
      | ^#             integer type
      | Ty -> Ty        function type
```

# Comment
```
// comment
```


# Toplevel let binding
- `!! x = T`
- `!!^ x = T`

# Directives
- `##?`
- `##<<`
- `##><`

Put `##?;;` on the interactive session to show details.

# Example
```
:> !!^ $! = \$# ->
:       $# <= #-
:       ? #-
:       : $# * $! ($# - #-)
:  ;
:  $! #-_-;;
type: ^#
eval: #----___ (with [])
```

" `(with [])` " shows currently bounded variable names (empty in this example.)

# Known issues
- Comment including "`;;`" causes unexpected behavior.
- Line number in error message becomes wrong in a certain case.
- Type error explanation in a certain case is unclear.
