module Language

let ( ||>> ) = fun f g -> fun a1 a2 -> (a1, a2) ||> f |> g

type BinOp =
    | Add | Sub | Mul | Div
    | And | Or
    | Equal | Lt | Gt | Lte | Gte

[<StructuredFormatDisplay("{Disp}")>]
type Term =
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


[<StructuredFormatDisplay("{Disp}")>]
type Type =
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
            | Fun (t1, t2) -> t1.HasTyVar name || t2.HasTyVar name
            | Bool | Int -> false

type Context = Map<string, Type>

type Constr = Eq of Type * Type

type Subst = MapsTo of string * Type

exception TypingException of string
let typingException msg = raise (TypingException msg)


let rec evalR (env: Env) term =
    match term with
    | Var s ->
        match Map.tryFind s env with
        | Some value -> value
        | None -> evaluationException "internal error: unbound variable"
    | Abs _ as t -> Clos (t, env)
    | App (t1, t2) ->
        let v1' = evalR env t1
        match v1' with
        | Clos (Abs (x, t1'Body), env') ->
            let v2' = evalR env t2
            evalR (env'.Add(x, v2')) t1'Body
        | Rec vref ->
            match !vref with
            | Clos (Abs (x, t1'Body), env') ->
                let v2' = evalR env t2
                evalR (env'.Add(x, v2')) t1'Body
            | _ -> evaluationException "internal error: cannot apply"
        | _ -> evaluationException "internal error: cannot apply"
    | Boolean _ | Integer _ as t -> Prim t
    | If (c, t, e) ->
        let vc = evalR env c
        match vc with
        | Prim (Boolean true) ->
            evalR env t
        | Prim (Boolean false) ->
            evalR env e
        | _ -> evaluationException "internal error: non-Boolean condition"
    | BinaryOp (op, t1, t2) ->
        let t1' = evalR env t1
        let t2' = evalR env t2
        match t1', t2' with
        | Prim (Integer i1), Prim (Integer i2) ->
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
            Prim (termBuilder i1 i2)
        | Prim (Boolean b1), Prim (Boolean b2) ->
            let termBuilder =
                match op with
                | And -> ( && ) ||>> Boolean
                | Or -> ( || ) ||>> Boolean
                | _ -> evaluationException "internal error: non-Boolean operator"
            Prim (termBuilder b1 b2)
        | _ -> evaluationException "internal error: unexpected operands"
    | Let (x, t1, t2) ->
        let v1 = evalR env t1
        evalR (env.Add(x, v1)) t2
    | LetRec (x, t1, t2) ->
        let dummy = Prim (Integer 0)
        let vref = ref dummy
        let v1 = evalR (env.Add(x, Rec vref)) t1
        vref := v1
        evalR (env.Add(x, v1)) t2
let eval = evalR Map.empty


let genFreshTyVar =
    let mutable i = -1
    fun () ->
        i <- i + 1
        TyVar $"T{i}"

let rec collectConstr (cxt: Context) term =
    match term with
    | Var s ->
        match Map.tryFind s cxt with
        | Some ty -> ty, []
        | None -> typingException "unbound variable"
    | Abs (x, body) ->
        let xType = genFreshTyVar()
        let bodyType, constr = collectConstr (cxt.Add(x, xType)) body
        Fun (xType, bodyType), constr
    | App (t1, t2) ->
        let t1Type, t1Constr = collectConstr cxt t1
        let t2Type, t2Constr = collectConstr cxt t2
        let wholeType = genFreshTyVar()
        wholeType, Eq (t1Type, Fun (t2Type, wholeType)) :: t1Constr @ t2Constr
    | Boolean _ -> Bool, []
    | Integer _ -> Int, []
    | If (c, t, e) ->
        let cType, cConstr = collectConstr cxt c
        let tType, tConstr = collectConstr cxt t
        let eType, eConstr = collectConstr cxt e
        tType, Eq (cType, Bool) :: Eq (tType, eType) :: cConstr @ tConstr @ eConstr
    | BinaryOp (op, t1, t2) ->
        let t1Type, t1Constr = collectConstr cxt t1
        let t2Type, t2Constr = collectConstr cxt t2
        match op with
        | Add | Sub | Mul | Div ->
            Int, Eq (t1Type, Int) :: Eq (t2Type, Int) :: t1Constr @ t2Constr
        | And | Or ->
            Bool, Eq (t1Type, Bool) :: Eq (t2Type, Bool) :: t1Constr @ t2Constr
        | Equal | Lt | Gt | Lte | Gte ->
            Bool, Eq (t1Type, Int) :: Eq (t2Type, Int) :: t1Constr @ t2Constr
    | Let (x, t1, t2) ->
        let t1Type, t1Constr = collectConstr cxt t1
        let t2Type, t2Constr = collectConstr (cxt.Add(x, t1Type)) t2
        t2Type, t1Constr @ t2Constr
    | LetRec (x, t1, t2) ->
        let xType = genFreshTyVar()
        let t1Type, t1Constr = collectConstr (cxt.Add(x, xType)) t1
        let t2Type, t2Constr = collectConstr (cxt.Add(x, xType)) t2
        t2Type, Eq (xType, t1Type) :: t1Constr @ t2Constr

let rec substType (MapsTo (replacedTyVarName, insertedType) as substitution) targetType =
    match targetType with
    | TyVar s as tv -> if s = replacedTyVarName then insertedType else tv
    | Fun (ty1, ty2) ->
        let ty1' = substType substitution ty1
        let ty2' = substType substitution ty2
        Fun (ty1', ty2')
    | Bool | Int as ty -> ty

let substConstr substitution constr =
    let subst = substType substitution
    constr |> List.map (fun (Eq (ty1, ty2)) -> Eq (subst ty1, subst ty2))

let rec unify constraints =
    match constraints with
    | [] -> []
    | Eq (ty1, ty2) :: constrs ->
        match (ty1, ty2) with
        | (ty1, ty2) when ty1 = ty2 -> unify constrs
        | (TyVar s, ty2) when not (ty2.HasTyVar s) ->
            let constrs' = substConstr (MapsTo (s, ty2)) constrs
            MapsTo (s, ty2) :: unify constrs'
        | (ty1, TyVar s) when not (ty1.HasTyVar s) ->
            let constrs' = substConstr (MapsTo (s, ty1)) constrs
            MapsTo (s, ty1) :: unify constrs'
        | (Fun (ty11, ty12), Fun (ty21, ty22)) ->
            let constrs' = Eq (ty11, ty21) :: Eq (ty12, ty22) :: constrs
            unify constrs'
        | _ -> typingException "type mismatch"

let typeof cxt term =
    let baseType, constraints = collectConstr cxt term
    let substitutions = unify constraints
    List.fold
        (fun targetType substitution -> substType substitution targetType)
        baseType
        substitutions


type Toplevel =
    | Term of Term
    | ToplevelLet of string * Term