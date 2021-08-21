module Language

let ( ||>> ) = fun f g -> fun a1 a2 -> (a1, a2) ||> f |> g

type BinOp =
    | Add | Sub | Mul | Div
    | And | Or
    | Equal | Lt | Gt | Lte | Gte

type TermInfo =
    int * int
let infoToString (info: TermInfo) =
    $"line {fst info}, char {snd info}"

[<StructuredFormatDisplay("{Disp}")>]
type TermTree =
    | Var of string
    | Abs of string * Term
    | App of Term * Term
    | Boolean of bool
    | Integer of int
    | If of cond:Term * thenClause:Term * elseClause:Term
    | BinaryOp of BinOp * Term * Term
    | Let of var:string * bounded:Term * body:Term
    | LetRec of var:string * bounded:Term * body:Term
    with
        override this.ToString() =
            match this with
            | Var s -> s
            | Abs (x, body) -> $"fun %s{x} -> %A{body}"
            | App (t1, t2) -> $"(%A{t1}) (%A{t2})"
            | Boolean b -> $"{b}".ToLower()
            | Integer i -> $"{i}"
            | If (c, t, e) -> $"if %A{c} then %A{t} else %A{e}"
            | BinaryOp (op, t1, t2) ->
                let opSymb =
                    match op with
                    | Add -> "+"
                    | Sub -> "-"
                    | Mul -> "*"
                    | Div -> "/"
                    | And -> "&&"
                    | Or -> "||"
                    | Equal -> "="
                    | Lt -> "<"
                    | Gt -> ">"
                    | Lte -> "<="
                    | Gte -> ">="
                $"(%A{t1}) {opSymb} (%A{t2})"
            | Let (x, t1, t2) -> $"let %s{x} = %A{t1} in %A{t2}"
            | LetRec (x, t1, t2) -> $"let rec %s{x} = %A{t1} in %A{t2}"
        member this.Disp = this.ToString()
and [<StructuredFormatDisplay("{Disp}")>] Term =
    {TermTree: TermTree; TermInfo: TermInfo}
    with
        override this.ToString() =
            this.TermTree.ToString()
        member this.Disp = this.ToString()

type Env = Map<string, Value>
and [<StructuredFormatDisplay("{Disp}")>] Value =
    | Clos of Term * Env
    | Prim of Term
    | Rec of Value ref
    with
        override this.ToString() =
            match this with
            | Clos (term, env) -> $"<%A{term} @%A{env}>"
            | Prim term -> $"<%A{term}>"
            | Rec vref -> (!vref).ToString()
        member this.Disp = this.ToString()

let rec openClosure value =
    match value with
    | Clos (term, env) ->
        let keys = env |> Map.toList |> List.unzip |> fst
        term, keys
    | Prim term -> term, []
    | Rec vref -> openClosure (!vref)

exception EvaluationException of string
let evaluationException msg = raise (EvaluationException msg)


type TypeInfo =
    Term option

[<StructuredFormatDisplay("{Disp}")>]
type TypeTree =
    | TyVar of string
    | Fun of Type * Type
    | Bool
    | Int
    with
        override this.ToString() =
            match this with
            | TyVar s -> s
            | Fun (t1, t2) -> $"(%A{t1})->%A{t2}"
            | Bool -> "Bool"
            | Int -> "Int"
        member this.Disp = this.ToString()

        member this.HasTyVar name =
            match this with
            | TyVar s -> s = name
            | Fun (t1, t2) ->
                t1.TypeTree.HasTyVar name || t2.TypeTree.HasTyVar name
            | Bool | Int -> false
and [<StructuredFormatDisplay("{Disp}")>] Type =
    {TypeTree: TypeTree; TypeInfo: TypeInfo}
    with
        override this.ToString() =
            this.TypeTree.ToString()
        member this.Disp = this.ToString()

type Context = Map<string, Type>

type Constr = Eq of Type * Type

type Subst = MapsTo of string * Type

exception TypingException of string
let typingException msg = raise (TypingException msg)


let rec evalR (env: Env) (term: Term) =
    match term.TermTree with
    | Var s ->
        match Map.tryFind s env with
        | Some value -> value
        | None -> evaluationException "internal error: unbound variable"
    | Abs _ -> Clos (term, env)
    | App (t1, t2) ->
        let v1' = evalR env t1
        match v1' with
        | Clos ({TermTree = Abs (x, t1'Body)}, env') ->
            let v2' = evalR env t2
            evalR (env'.Add(x, v2')) t1'Body
        | Rec vref ->
            match !vref with
            | Clos ({TermTree = Abs (x, t1'Body)}, env') ->
                let v2' = evalR env t2
                evalR (env'.Add(x, v2')) t1'Body
            | _ -> evaluationException "internal error: cannot apply"
        | _ -> evaluationException "internal error: cannot apply"
    | Boolean _ | Integer _ -> Prim term
    | If (c, t, e) ->
        let vc = evalR env c
        match vc with
        | Prim {TermTree = Boolean true} ->
            evalR env t
        | Prim {TermTree = Boolean false} ->
            evalR env e
        | _ -> evaluationException "internal error: non-Boolean condition"
    | BinaryOp (op, t1, t2) ->
        let t1' = evalR env t1
        let t2' = evalR env t2
        match t1', t2' with
        | Prim {TermTree = Integer i1; TermInfo = t1'TermInfo}, Prim {TermTree = Integer i2} ->
            let termBuilder =
                match op with
                | Add -> ( + ) ||>> Integer
                | Sub -> ( - ) ||>> Integer
                | Mul -> ( * ) ||>> Integer
                | Div -> ( / ) ||>> Integer
                | Equal -> ( = ) ||>> Boolean
                | Lt -> ( < ) ||>> Boolean
                | Gt -> ( > ) ||>> Boolean
                | Lte -> ( <= ) ||>> Boolean
                | Gte -> ( >= ) ||>> Boolean
                | _ -> evaluationException "internal error: non-Integer operator"
            Prim {TermTree = termBuilder i1 i2; TermInfo = t1'TermInfo}
        | Prim {TermTree = Boolean b1; TermInfo = t1'TermInfo}, Prim {TermTree = Boolean b2} ->
            let termBuilder =
                match op with
                | And -> ( && ) ||>> Boolean
                | Or -> ( || ) ||>> Boolean
                | _ -> evaluationException "internal error: non-Boolean operator"
            Prim {TermTree = termBuilder b1 b2; TermInfo = t1'TermInfo}
        | _ -> evaluationException "internal error: unexpected operands"
    | Let (x, t1, t2) ->
        let v1 = evalR env t1
        evalR (env.Add(x, v1)) t2
    | LetRec (x, t1, t2) ->
        let dummy = Prim {TermTree = Integer 0; TermInfo = (0, 0)}
        let vref = ref dummy
        let v1 = evalR (env.Add(x, Rec vref)) t1
        vref := v1
        evalR (env.Add(x, v1)) t2
let eval = evalR Map.empty


// let updateTypeInfo (Type (ty, _)) info =
//     Type (ty, info)

let genFreshTyVar =
    let mutable i = -1
    fun () ->
        i <- i + 1
        {TypeTree = TyVar $"T{i}"; TypeInfo = None}

let rec collectConstr (cxt: Context) (term: Term) =
    match term.TermTree with
    | Var s ->
        match Map.tryFind s cxt with
        | Some ty -> ty, []
        | None -> typingException $"unbound variable: `%A{term}` @{infoToString term.TermInfo}"
    | Abs (x, body) ->
        let xType = genFreshTyVar()
        let bodyType, constr = collectConstr (cxt.Add(x, xType)) body
        {TypeTree = Fun (xType, bodyType); TypeInfo = Some term}, constr
    | App (t1, t2) ->
        let t1Type, t1Constr = collectConstr cxt t1
        let t2Type, t2Constr = collectConstr cxt t2
        let wholeType = genFreshTyVar()
        wholeType, Eq (t1Type, {TypeTree = Fun (t2Type, wholeType); TypeInfo = None}) :: t1Constr @ t2Constr
    | Boolean _ -> {TypeTree = Bool; TypeInfo = Some term}, []
    | Integer _ -> {TypeTree = Int; TypeInfo = Some term}, []
    | If (c, t, e) ->
        let cType, cConstr = collectConstr cxt c
        let tType, tConstr = collectConstr cxt t
        let eType, eConstr = collectConstr cxt e
        {TypeTree = tType.TypeTree; TypeInfo = Some term},
        Eq (cType, {TypeTree = Bool; TypeInfo = None}) :: Eq (tType, eType) :: cConstr @ tConstr @ eConstr
    | BinaryOp (op, t1, t2) ->
        let t1Type, t1Constr = collectConstr cxt t1
        let t2Type, t2Constr = collectConstr cxt t2
        match op with
        | Add | Sub | Mul | Div ->
            {TypeTree = Int; TypeInfo = Some term},
            Eq (t1Type, {TypeTree = Int; TypeInfo = None}) :: Eq (t2Type, {TypeTree = Int; TypeInfo = None}) :: t1Constr @ t2Constr
        | And | Or ->
            {TypeTree = Bool; TypeInfo = Some term},
            Eq (t1Type, {TypeTree = Bool; TypeInfo = None}) :: Eq (t2Type, {TypeTree = Bool; TypeInfo = None}) :: t1Constr @ t2Constr
        | Equal | Lt | Gt | Lte | Gte ->
            {TypeTree = Bool; TypeInfo = Some term},
            Eq (t1Type, {TypeTree = Int; TypeInfo = None}) :: Eq (t2Type, {TypeTree = Int; TypeInfo = None}) :: t1Constr @ t2Constr
    | Let (x, t1, t2) ->
        let t1Type, t1Constr = collectConstr cxt t1
        let t2Type, t2Constr = collectConstr (cxt.Add(x, t1Type)) t2
        {TypeTree = t2Type.TypeTree; TypeInfo = Some term}, t1Constr @ t2Constr
    | LetRec (x, t1, t2) ->
        let xType = genFreshTyVar()
        let t1Type, t1Constr = collectConstr (cxt.Add(x, xType)) t1
        let t2Type, t2Constr = collectConstr (cxt.Add(x, xType)) t2
        {TypeTree = t2Type.TypeTree; TypeInfo = Some term}, Eq (xType, t1Type) :: t1Constr @ t2Constr

let rec substType (MapsTo (replacedTyVarName, insertedType) as substitution) targetType =
    match targetType.TypeTree with
    | TyVar s -> if s = replacedTyVarName then insertedType else targetType
    | Fun (ty1, ty2) ->
        let ty1' = substType substitution ty1
        let ty2' = substType substitution ty2
        {TypeTree = Fun (ty1', ty2'); TypeInfo = targetType.TypeInfo}
    | Bool | Int -> targetType

let substConstr substitution constr =
    let subst = substType substitution
    constr |> List.map (fun (Eq (ty1, ty2)) -> Eq (subst ty1, subst ty2))

let rec unify constraints =
    match constraints with
    | [] -> []
    | Eq (ty1, ty2) :: constrs ->
        match (ty1.TypeTree, ty2.TypeTree) with
        | (tyTree1, tyTree2) when tyTree1 = tyTree2 -> unify constrs
        | (TyVar s, tyTree2) when not (tyTree2.HasTyVar s) ->
            let constrs' = substConstr (MapsTo (s, ty2)) constrs
            MapsTo (s, ty2) :: unify constrs'
        | (tyTree1, TyVar s) when not (tyTree1.HasTyVar s) ->
            let constrs' = substConstr (MapsTo (s, ty1)) constrs
            MapsTo (s, ty1) :: unify constrs'
        | (Fun (ty11, ty12), Fun (ty21, ty22)) ->
            let constrs' = Eq (ty11, ty21) :: Eq (ty12, ty22) :: constrs
            unify constrs'
        | _ ->
            let msg = 
                match (ty1.TypeInfo, ty2.TypeInfo) with
                | (Some t1, Some t2) ->
                    $"type mismatch: `%A{t1}` : %A{ty1} @{infoToString t1.TermInfo} vs `%A{t2}` : %A{ty2} @{infoToString t2.TermInfo}"
                | (Some t1, None) ->
                    $"type mismatch: `%A{t1}` : %A{ty1} @{infoToString t1.TermInfo} but should be %A{ty2}"
                | (None, Some t2) ->
                    $"type mismatch: `%A{t2}` : %A{ty2} @{infoToString t2.TermInfo} but should be %A{ty1}"
                | (None, None) ->
                    $"type mismatch: %A{ty1} vs %A{ty2}"
            typingException msg

let typeof cxt term =
    let baseType, constraints = collectConstr cxt term
    let substitutions = unify constraints
    List.fold
        (fun targetType substitution -> substType substitution targetType)
        baseType
        substitutions


type Directive =
    | Help
    | Load of string
    | Exit

type Toplevel =
    | Term of Term
    | ToplevelLet of string * Term
    | Directive of Directive
    | Eof