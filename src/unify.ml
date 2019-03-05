open Ast
open Ftv
open Apply

exception UnifyError of string
let unify_err msg = raise (UnifyError msg)

let bind_var id ty =
  match ty with
  | ty when ty = TVar id -> null_subs
  | ty when StringSet.mem id (ftv_mono ty) ->
    unify_err ("Occurs check: Type variable " ^ id ^ " does not appear in "
    ^ str_mono_ty ty)
  | _ -> StringMap.singleton id ty

let rec mgu ty1 ty2 =
  match ty1, ty2 with
  | TFun (p1, ret1), TFun (p2, ret2) ->
    let subs1 = mgu p1 p2 in
    let subs2 = mgu (apply_mono subs1 ret1) (apply_mono subs1 ret2) in
    compose_subs subs1 subs2
  | TVar id, ty | ty, TVar id -> bind_var id ty
  | TInt, TInt | TBool, TBool -> null_subs
  | l, r -> unify_err ("Unify: Mismatching types")

