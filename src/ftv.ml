open Ast

let rec ftv_mono ty =
  let _rec ty = ftv_mono ty in
  let open StringSet in
  match ty with
  | TInt | TBool -> empty
  | TVar id -> singleton id
  | TFun (p, ret) -> union (_rec p) (_rec ret)

let ftv_poly ty =
  let open StringSet in
  match ty with
  | TForAll (vars, ty) -> List.fold_right remove vars (ftv_mono ty)

let ftv_env env =
  let open StringSet in
  let free_vars = List.map (ftv_poly) (map_bindings env) in
  List.fold_left (fun t a -> union a t) empty free_vars

