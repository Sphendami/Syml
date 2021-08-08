module Language


[<StructuredFormatDisplay("{Disp}")>]
type Term =
    | Var of string
    | Abs of string * Term
    | App of Term * Term
    | True
    | False
    | If of cond:Term * thenClause:Term * elseClause:Term
    with
        override this.ToString() =
            match this with
            | Var s -> s
            | Abs (x, body) -> $"fun %s{x} -> %A{body}"
            | App (t1, t2) -> $"(%A{t1}) (%A{t2})"
            | True -> "true"
            | False -> "false"
            | If (c, t, e) -> $"if %A{c} then %A{t} else %A{e}"
        member this.Disp = this.ToString()

type Env = Map<string, Value>
and [<StructuredFormatDisplay("{Disp}")>] Value =
    | Clos of Term * Env
    | Prim of Term
    with
        override this.ToString() =
            match this with
            | Clos (term, env) -> $"<%A{term} @%A{env}>"
            | Prim term -> $"<%A{term}>"
        member this.Disp = this.ToString()

let openClosure value =
    match value with
    | Clos (term, _) -> term
    | Prim term -> term

exception EvaluationException of string
let evaluationException msg = raise (EvaluationException msg)


[<StructuredFormatDisplay("{Disp}")>]
type Type =
    | TyVar of string
    | Fun of Type * Type
    | Bool
    with
        override this.ToString() =
            match this with
            | TyVar s -> s
            | Fun (t1, t2) -> $"(%A{t1})->%A{t2}"
            | Bool -> "Bool"
        member this.Disp = this.ToString()

        member this.HasTyVar name =
            match this with
            | TyVar s -> s = name
            | Fun (t1, t2) -> t1.HasTyVar name || t2.HasTyVar name
            | Bool -> false

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
        | _ -> evaluationException "internal error: cannot apply"
    | True | False as t -> Prim t
    | If (c, t, e) ->
        let vc = evalR env c
        match vc with
        | Prim True ->
            evalR env t
        | Prim False ->
            evalR env e
        | _ -> evaluationException "internal error: non-Boolean condition"

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
    | True | False -> Bool, []
    | If (c, t, e) ->
        let cType, cConstr = collectConstr cxt c
        let tType, tConstr = collectConstr cxt t
        let eType, eConstr = collectConstr cxt e
        tType, Eq (cType, Bool) :: Eq (tType, eType) :: cConstr @ tConstr @ eConstr

let rec substType (MapsTo (replacedTyVarName, insertedType) as substitution) targetType =
    match targetType with
    | TyVar s as tv -> if s = replacedTyVarName then insertedType else tv
    | Fun (ty1, ty2) ->
        let ty1' = substType substitution ty1
        let ty2' = substType substitution ty2
        Fun (ty1', ty2')
    | Bool as ty -> ty

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

let typeof term =
    let baseType, constraints = collectConstr Map.empty term
    let substitutions = unify constraints
    List.fold
        (fun targetType substitution -> substType substitution targetType)
        baseType
        substitutions
