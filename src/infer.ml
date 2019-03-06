open Ast
open Ftv
open Apply
open Unify

exception InferError of string
let infer_err msg = raise (InferError msg)

let generalize env ty =
  let open StringSet in
  let vars = elements (remove_ids (ftv_mono ty) (ftv_env env)) in
  TForAll (vars, ty)

let count = ref (-1)

let gen_ty prefix =
  incr count;
  TVar (prefix ^ string_of_int !count)

let instantiate ty =
  let open StringMap in
  match ty with
  | TForAll (vars, ty) ->
    let fresh_tys = List.map gen_ty vars in
    let env = List.fold_left2
      (fun acc id ty -> add id ty acc) empty vars fresh_tys in
    apply_mono env ty

let ti_lit l =
  match l with
  | Int _   -> (null_subs, TInt)
  | Bool _  -> (null_subs, TBool)

let rec ti env exp =
  let open StringMap in
  match exp with
  | Var id ->
    begin match find_opt id env with
    | Some env -> (null_subs, instantiate env)
    | None -> infer_err ("Unbound variable")
    end
  | Lit l -> ti_lit l
  | Abs (id, e) ->
    let fresh_ty = gen_ty "a" in
    let env' = remove id env in
    let sub = singleton id (TForAll ([], fresh_ty)) in
    let env'' = union (fun k a b -> Some a) env' sub in
    let s1, t1 = ti env'' e in
    (s1, TFun ((apply_mono s1 fresh_ty), t1))
  | App (fn, arg) ->
    let fresh_ty = gen_ty "a" in
    let s1, t1 = ti env fn in
    let s2, t2 = ti (apply_env s1 env) arg in
    let s3 = mgu (apply_mono s2 t1) (TFun (t2, fresh_ty)) in
    (compose_subs s3 (compose_subs s2 s1), apply_mono s3 fresh_ty)
  | Let (id, i, b) ->
    let s1, t1 = ti env i in
    let env' = remove id env in
    let ty = generalize (apply_env s1 env) t1 in
    let env'' = add id ty env' in
    let s2, t2 = ti (apply_env s1 env'') b in
    (compose_subs s1 s2, t2)
  | Binop (l, op, r) ->
    let s1, t1 = ti env l in
    let s2, t2 = ti env r in
    let subs = compose_subs s1 s2 in
    match op with
    | Add | Sub | Mul | Div ->
      let s3 = compose_subs (mgu t1 TInt) (mgu t2 TInt) in
      (compose_subs subs s3, TInt)
    | And | Or ->
      let s3 = compose_subs (mgu t1 TBool) (mgu t2 TBool) in
      (compose_subs subs s3, TBool)

let infer env e =
  count := - 1;
  let subs, ty = ti env e in
  apply_mono subs ty
